import
  strutils, hashes, std / sha1, os, tables,
  times, math, intsets, options as opt

import ../compiler/[
  ast, astalgo, platform, magicsys, extccomp, trees, bitsets,
  nversion, nimsets, msgs, idents, types, options, ropes,
  passes, ccgutils, wordrecg, renderer, rodutils,
  cgmeth, lowerings, sighashes, modulegraphs, lineinfos]

import
  spirvTypes, glslTypes, openclTypes

type
  SpirvId = uint32

  TypeWidth = enum
    tw8
    tw16
    tw32
    tw64 

  SpirvModuleList* = ref object of RootObj
    modules*: seq[SpirvGen]     # list of all compiled modules
    config*: ConfigRef
    graph*: ModuleGraph

  SpirvGenObj = object of TPassContext
    g*: SpirvModuleList
    filename: string
    allWords: seq[uint32]
    currentFunction: SpirvFunction
    module: PSym
    nextId: uint32
    entryPoints: seq[tuple[function: SpirvFunction; executionModel: SpvExecutionModel]]
    variables: Table[int, SpirvVariable]
    functions: Table[int, SpirvFunction]
    functionTypes: seq[SpirvFunctionType]

    decorationWords: seq[uint32]
    constantWords: seq[uint32]
    typeWords: seq[uint32]

    voidType: SpirvId
    boolType: SpirvId
    intTypes: array[TypeWidth, array[bool, SpirvId]]
    floatTypes: array[TypeWidth, SpirvId]
    pointerTypes: Table[(SpirvId, SpvStorageClass), SpirvId]

    intConstants: Table[(SpirvId, BiggestInt), SpirvId]
    floatConstants: Table[(SpirvId, BiggestFloat), SpirvId]
    trueConstant: SpirvId
    falseConstant: SpirvId

  SpirvGen = ref SpirvGenObj

  SpirvFunction = ref object
    symbol: PSym
    id: SpirvId
    words: seq[uint32]

  SpirvVariable = ref object
    symbol: PSym
    id: SpirvId
    words: seq[uint32]

  SpirvFunctionType = ref object
    id: SpirvId
    returnType: SpirvId
    argTypes: seq[SpirvId]

template config*(m: SpirvGen): ConfigRef = m.g.config

proc words(m: SpirvGen): var seq[uint32] =
  if m.currentFunction != nil:
    return m.currentFunction.words
  else:
    return m.allWords 

proc genNode(g: SpirvGen; n: PNode): SpirvId

proc newModuleList*(g: ModuleGraph): SpirvModuleList =
  SpirvModuleList(modules: @[], config: g.config, graph: g)

proc rawNewModule(g: SpirvModuleList; module: PSym, filename: string): SpirvGen =
  new(result)
  result.g = g
  result.filename = filename
  result.nextId = 1
  result.module = module
  result.functions = initTable[int, SpirvFunction]()
  result.variables = initTable[int, SpirvVariable]()
  result.pointerTypes = initTable[(SpirvId, SpvStorageClass), SpirvId]()
  result.intConstants = initTable[(SpirvId, BiggestInt), SpirvId]()
  result.floatConstants = initTable[(SpirvId, BiggestFloat), SpirvId]()
  # result.sigConflicts = initCountTable[SigHash]()
  # if globals == nil:
  #   globals = newGlobals()

proc rawNewModule(g: SpirvModuleList; module: PSym; conf: ConfigRef): SpirvGen =
  result = rawNewModule(g, module, toFullPath(conf, module.position.FileIndex))
  
proc newModule(g: SpirvModuleList; module: PSym; conf: ConfigRef): SpirvGen =
  # we should create only one cgen module for each module sym
  result = rawNewModule(g, module, conf)

proc addInstruction(stream: var seq[uint32]; opCode: SpvOp; operands: varargs[uint32]) =

  var wordCount = 1 + operands.len
  let head =
    ((wordCount and SpvOpCodeMask) shl SpvWordCountShift).uint32 or
    (opCode.ord and SpvOpCodeMask).uint32

  stream.add(head)
  stream.add(operands)

proc writeOutput(g: SpirvGen) =
  let outFile = changeFileExt(completeCFilePath(g.config, g.filename), "spv")

  var file: File
  if file.open(outFile, fmWrite):
    discard file.writeBuffer(addr g.words[0], g.words.len * sizeof(uint32))
    file.close()
  else:
    rawMessage(g.config, errCannotOpenFile, g.filename)

proc toWords(text: string): seq[uint32] =
  newSeq(result, (text.len + 1 + 3) div 4)
  for i, c in text:
    result[i div 4] = result[i div 4] or (c.uint32 shl ((i mod 4) * 8))

var level = 0

proc generateId(g: SpirvGen): uint32 =
  result = g.nextId
  inc g.nextId

iterator procParams(typ: PType): PNode =
  for a in typ.n.sons[1..^1]:
    let param = a.sym
    if isCompileTimeOnly(param.typ): continue
    yield a

proc procParams(typ: PType): seq[PNode] =
  accumulateResult(procParams(typ))

proc genVoidType(g: SpirvGen): SpirvId =
  if g.voidType == 0:
    g.voidType = g.generateId()
    g.typeWords.addInstruction(SpvOpTypeVoid, g.voidType)
  return g.voidType

proc genBoolType(g: SpirvGen): SpirvId =
  if g.boolType == 0:
    g.voidType = g.generateId()
    g.typeWords.addInstruction(SpvOpTypeBool, g.boolType)
  return g.boolType

proc bits(size: TypeWidth): uint32 =
  case size:
    of tw8: 8
    of tw16: 16
    of tw32: 32
    of tw64: 64

proc genIntType(g: SpirvGen; size: TypeWidth; isSigned: bool): SpirvId =
  result = g.intTypes[size][isSigned]
  if result == 0:
    result = g.generateId()
    g.intTypes[size][isSigned] = result
    g.typeWords.addInstruction(SpvOpTypeInt, result, size.bits, isSigned.uint32)

proc genFloatType(g: SpirvGen; size: TypeWidth): SpirvId =
  result = g.floatTypes[size]
  if result == 0:
    result = g.generateId()
    g.floatTypes[size] = result
    g.typeWords.addInstruction(SpvOpTypeFloat, result, size.bits)
  
proc genType(g: SpirvGen; t: PType): SpirvId =

  case t.kind:
    of tyVoid: return g.genVoidType()
    of tyBool: return g.genBoolType()

    #of tyFloat16: return g.genFloatType(tw16)
    of tyFloat32, tyFloat: return g.genFloatType(tw32)
    of tyFloat64: return g.genFloatType(tw64)
    
    of tyInt8: return g.genIntType(tw8, false)
    of tyInt16: return g.genIntType(tw16, false)
    of tyInt32, tyInt: return g.genIntType(tw32, false)
    of tyInt64: return g.genIntType(tw64, false)

    of tyUInt8: return g.genIntType(tw8, true)
    of tyUInt16: return g.genIntType(tw16, true)
    of tyUInt32, tyUInt: return g.genIntType(tw32, true)
    of tyUInt64: return g.genIntType(tw64, true)

    of tyGenericInst:
      if t[0].lastSon.sym.name.s == "Vector":
        let id = g.generateId()
        g.typeWords.addInstruction(SpvOpTypeVector, id, g.genType(t[1]), t[2].n.intVal.uint32)
        return id

        # TODO: Cache with t.lastSon.sym.id

    of tyVar:
      return g.genType(t[0])

    of tyDistinct, tyAlias, tyInferred: return g.genType(t.lastSon)
    #of tyObject:
    else:
      echo t.kind
      echo t

proc genPointerType(g: SpirvGen; valueType: SpirvId; storageClass: SpvStorageClass): SpirvId =
  let key = (valueType, storageClass)
  if g.pointerTypes.contains(key):
    return g.pointerTypes[key]

  result = g.generateId()
  g.pointerTypes.add(key, result)
  g.typeWords.addInstruction(SpvOpTypePointer, result, storageClass.uint32, valueType)

proc genConstant(g: SpirvGen; valueType: SpirvId; value: SomeInteger): SpirvId =
  let key = (valueType, value.BiggestInt)
  if g.intConstants.contains(key):
    return g.intConstants[key]

  result = g.generateId()
  g.intConstants.add(key, result)
  g.constantWords.addInstruction(SpvOpConstant, valueType, result, value.uint32) # ToWords

proc genConstant(g: SpirvGen; valueType: SpirvId; value: SomeFloat): SpirvId =
  let key = (valueType, value.BiggestFloat)
  if g.floatConstants.contains(key):
    return g.floatConstants[key]

  result = g.generateId()
  g.floatConstants.add(key, result)
  g.constantWords.addInstruction(SpvOpConstant, valueType, result, value.float32) # ToWords
  
proc genBoolConstant(g: SpirvGen; value: bool): SpirvId =
  result = if value: g.trueConstant else: g.falseConstant
  if result == 0:
    result = g.generateId()
    let op = if value: SpvOpConstantTrue else: SpvOpConstantFalse
    if value: g.trueConstant = result
    else: g.falseConstant = result
    g.constantWords.addInstruction(op, result)

proc genParamType(g: SpirvGen; t: PType): SpirvId = discard

proc genFunctionType(g: SpirvGen; t: PType): SpirvFunctionType =

  let returnType =
    if t.sons[0] == nil: g.genVoidType()
    else: g.genType(t.sons[0])

  var argTypes = newSeq[SpirvId]()

  for param in t.procParams():
    let paramType = param.sym.typ.skipTypes({ tyGenericInst, tyAlias, tySink })
    argTypes.add(g.genParamType(paramType))

    # if skipTypes(t, {tyVar}).kind in { tyOpenArray, tyVarargs }:
    #   argTypes.add(g.intType)  # Extra length parameter

  for knownType in g.functionTypes:
    if returnType != knownType.returnType: continue

    var found = true
    for i, argType in knownType.argTypes:
      if argTypes[i] != argType: found = false

    if found:
      return knownType

  new(result)
  result.id = g.generateId()
  result.returnType = returnType
  result.argTypes = argTypes
  g.functionTypes.add(result)

  g.typeWords.addInstruction(SpvOpTypeFunction, result.id, returnType) 

proc genFunction(g: SpirvGen; s: PSym): SpirvFunction =
  
  if g.functions.contains(s.id):
    return g.functions[s.id]

  let functionType = g.genFunctionType(s.typ)

  new(result)
  result.symbol = s
  result.id = g.generateId()
  let labelId = g.generateId()

  g.functions.add(s.id, result)

  result.words.addInstruction(SpvOpFunction, functionType.returnType, result.id, 0'u32, functionType.id)
  result.words.addInstruction(SpvOpLabel, labelId)

  g.currentFunction = result
  discard g.genNode(s.getBody())
  g.currentFunction = nil

  if functionType.returnType == g.genVoidType():
    result.words.addInstruction(SpvOpReturn)
  result.words.addInstruction(SpvOpFunctionEnd)  

proc genIdentDefs(g: SpirvGen; n: PNode): SpirvVariable =

  new(result)
  result.id = g.generateId()

  if n[0].kind == nkPragmaExpr:
    result.symbol = n[0][0].sym
  else:
    result.symbol = n[0].sym

  var storageClass = SpvStorageClassFunction
  if result.symbol.flags.contains(sfGlobal):
    storageClass = SpvStorageClassPrivate

  if n[0].kind == nkPragmaExpr:
    
    for pragma in n.sons[0][1]:
      if pragma.kind == nkExprColonExpr and pragma[0].kind == nkSym:
        case pragma[0].sym.name.s.normalize():
          of "location": g.decorationWords.addInstruction(SpvOpDecorate, result.id, SpvDecorationLocation.uint32, pragma[1].intVal.uint32)
          of "descriptorSet": g.decorationWords.addInstruction(SpvOpDecorate, result.id, SpvDecorationDescriptorSet.uint32, pragma[1].intVal.uint32)
          of "binding": g.decorationWords.addInstruction(SpvOpDecorate, result.id, SpvDecorationBinding.uint32, pragma[1].intVal.uint32)
          else: discard
      elif pragma.kind == nkSym:
        case pragma.sym.name.s:
          of "input": storageClass = SpvStorageClassInput
          of "output": storageClass = SpvStorageClassOutput
          else: discard

  g.variables.add(result.symbol.id, result)

  # TODO: Cache pointer type
  let 
    variableType = g.genType(result.symbol.typ)
    pointerType = g.genPointerType(variableType, storageClass)

  # TODO: Initializer
  result.words.addInstruction(SpvOpVariable, pointerType, result.id, storageClass.uint32)

proc genSons(g: SpirvGen; n: PNode) =
  for s in n: discard g.genNode(s)

proc genNode(g: SpirvGen; n: PNode): SpirvId =

  if sfMainModule notin g.module.flags:
    return

  case n.kind:
    of nkEmpty: discard

    of nkIntLit: return g.genConstant(g.genType(n.typ), n.intVal.int32)

    of nkHiddenAddr:
      return g.genNode(n[0])

    of nkSym:
      if g.variables.contains(n.sym.id):
        return g.variables[n.sym.id].id

    of nkCallKinds:

      if n[0].sym.name.s == "[]=":
        let
          base = g.genNode(n[1])
          left = g.generateId()
          leftType = g.genType(n[1].typ)

        g.words.addInstruction(SpvOpAccessChain, g.genPointerType(leftType, SpvStorageClassOutput), left, base, g.genNode(n[2]))
        g.words.addInstruction(SpvOpStore, left, g.genNode(n[3]))

      elif n[0].sym.name.s == "[]":
        let
          base = g.genNode(n[1])
          temp = g.generateId()
          resultType = g.genType(n.typ)
        result = g.generateId()

        g.words.addInstruction(SpvOpAccessChain, g.genPointerType(resultType, SpvStorageClassInput), temp, base, g.genNode(n[2]))
        g.words.addInstruction(SpvOpLoad, resultType, result, temp)

    #of nkIdentDefs: discard g.genIdentDefs(n)
    of nkProcDef, nkFuncDef, nkMethodDef, nkIteratorDef, nkConverterDef: #g.genProcDef(n)

      let s = n.sons[namePos].sym
      if sfImportc in s.flags:
        return

      var executionModels: set[SpvExecutionModel]

      for pragma in n[pragmasPos]:
        if pragma.kind == nkExprColonExpr and
           pragma[0].kind == nkSym and
           pragma[0].sym.name.s.normalize() == "stage":
          let executionModel =
            case pragma[1].ident.s.normalize():
            of "vertex": SpvExecutionModelVertex
            of "fragment": SpvExecutionModelFragment
            of "geometry": SpvExecutionModelGeometry
            of "tessellationcontrol":  SpvExecutionModelTessellationControl
            of "tessellationevaluation": SpvExecutionModelTessellationEvaluation
            of "compute": SpvExecutionModelGLCompute
            else: raise newException(ValueError, "Unsupported value")
          
          executionModels.incl(executionModel)
      
      if executionModels != {}:
        let function = g.genFunction(s)
        for executionModel in executionModels:
          g.entryPoints.add((function, executionModel))

    of nkVarSection, nkLetSection, nkConstSection: #g.genSons(n)
      for child in n.sons:
        discard g.genIdentDefs(child)
      discard
    of nkStmtList: g.genSons(n)

    else: discard # internalError(n.info, "Unhandled node: " & $n)

proc myProcess(b: PPassContext, n: PNode): PNode =
  result = n
  if b == nil: return
  var m = SpirvGen(b)
  if passes.skipCodegen(m.config, n): return
  
  discard m.genNode(n)
  # var p = newProc(globals, m, nil, m.module.options)
  # p.unique = globals.unique
  # genModule(p, n)
  # add(p.g.code, p.locals)
  # add(p.g.code, p.body)

proc myClose(graph: ModuleGraph; b: PPassContext, n: PNode): PNode =
  
  result = n
  if b == nil: return
  var m = SpirvGen(b)
  if passes.skipCodegen(m.config, n): return

  let glslId = m.generateId()
  discard m.genNode(n)

  # Header
  m.words.add(SpvMagicNumber)
  m.words.add(SpvVersion)
  m.words.add(0) # Generator tool specific magic number
  m.words.add(m.nextId) # Id bound
  m.words.add(0) # Reserved (Instruction schema)

  # Instruction stream

  # Capabilities
  m.words.addInstruction(SpvOpCapability, ord(SpvCapabilityShader).uint32)

  # Extensions

  # ExtInstImports
  m.words.addInstruction(SpvOpExtInstImport, @[glslId] & "GLSL.std.450".toWords)

  # MemoryModel
  m.words.addInstruction(SpvOpMemoryModel, ord(SpvAddressingModelLogical).uint32, ord(SpvMemoryModelGLSL450).uint32)

  # EntryPoint
  for entryPoint in m.entryPoints:
    m.words.addInstruction(SpvOpEntryPoint, @[ord(entryPoint.executionModel).uint32, entryPoint.function.id] & entryPoint.function.symbol.name.s.toWords)

  # ExecutionMode

  # Debug instructions
    # Strings
    # SourceExtensions
    # Source
    # SourceContinued
    # Names
#  m.words.addInstruction(SpvOpName, uint32.none, uint32.none, 2, "main".toWords)
    # MemberNames
  # Annotations
    # Decorates
  m.words.add(m.decorationWords)
    # MemberDescorates
    # GroupDecorates
    # GroupMemberDecorates
    # DecorationsGroups
  # (Lines valid from here)
    # Types
  m.words.add(m.typeWords)
    # Constants
  m.words.add(m.constantWords)
    # Non-function Variables
  for id, variable in m.variables:
    m.words.add(variable.words)
    # Undef
  # Function declarations (Functions, FunctionParameters, FunctionsEnds)
  # Function definitions

  for id, function in m.functions:
    m.words.add(function.words)

    # Blocks
      # Label (opt preceeded by Line)

  m.writeOutput()

  # if sfMainModule in m.module.flags:
  #   let ext = "js"
  #   let f = if globals.classes.len == 0: toFilename(FileIndex m.module.position)
  #           else: "nimsystem"
  #   let code = wholeCode(graph, m)
  #   let outfile =
  #     if options.outFile.len > 0:
  #       if options.outFile.isAbsolute: options.outFile
  #       else: getCurrentDir() / options.outFile
  #     else:
  #       changeFileExt(completeCFilePath(f), ext)
  #   discard writeRopeIfNotEqual(genHeader() & code, outfile)
  #   for obj, content in items(globals.classes):
  #     genClass(obj, content, ext)


template injectG() {.dirty.} =
  if graph.backend == nil:
    graph.backend = newModuleList(graph)
  let g = SpirvModuleList(graph.backend)

proc myOpen(graph: ModuleGraph; module: PSym): PPassContext =
  injectG()
  result = newModule(g, module, graph.config)

const spirvGenPass* = makePass(myOpen, myProcess, myClose)