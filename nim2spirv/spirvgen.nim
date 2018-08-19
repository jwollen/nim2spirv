import
  strutils, hashes, std / sha1, os, tables, sets,
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
    modules*: seq[SpirvModule]     # list of all compiled modules
    config*: ConfigRef
    graph*: ModuleGraph

  SpirvModuleObj = object of TPassContext
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
    nameWords: seq[uint32]
    constantWords: seq[uint32]
    typeWords: seq[uint32]

    voidType: SpirvId
    boolType: SpirvId
    intTypes: array[TypeWidth, array[bool, SpirvId]]
    floatTypes: array[TypeWidth, SpirvId]
    pointerTypes: Table[(SpirvId, SpvStorageClass), SpirvId]
    types: seq[(PType, SpirvId)]

    intConstants: Table[(SpirvId, BiggestInt), SpirvId]
    floatConstants: Table[(SpirvId, BiggestFloat), SpirvId]
    trueConstant: SpirvId
    falseConstant: SpirvId

  SpirvModule = ref SpirvModuleObj

  SpirvFunction = ref object
    symbol: PSym
    id: SpirvId
    words: seq[uint32]
    usedVariables: HashSet[SpirvVariable]

  SpirvVariable = ref object
    symbol: PSym
    id: SpirvId
    words: seq[uint32]
    storageClass: SpvStorageClass

  SpirvFunctionType = ref object
    id: SpirvId
    returnType: SpirvId
    argTypes: seq[SpirvId]

proc hash(v: SpirvVariable): Hash =
  hash(v.id)

template config*(m: SpirvModule): ConfigRef = m.g.config

proc words(m: SpirvModule): var seq[uint32] =
  if m.currentFunction != nil:
    return m.currentFunction.words
  else:
    return m.allWords 

proc genNode(m: SpirvModule; n: PNode): SpirvId

proc newModuleList*(m: ModuleGraph): SpirvModuleList =
  SpirvModuleList(modules: @[], config: m.config, graph: m)

proc rawNewModule(g: SpirvModuleList; module: PSym, filename: string): SpirvModule =
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

proc rawNewModule(g: SpirvModuleList; module: PSym; conf: ConfigRef): SpirvModule =
  result = rawNewModule(g, module, toFullPath(conf, module.position.FileIndex))
  
proc newModule(g: SpirvModuleList; module: PSym; conf: ConfigRef): SpirvModule =
  # we should create only one cgen module for each module sym
  result = rawNewModule(g, module, conf)

proc addInstruction(stream: var seq[uint32]; opCode: SpvOp; operands: varargs[uint32]) =

  var wordCount = 1 + operands.len
  let head =
    ((wordCount and SpvOpCodeMask) shl SpvWordCountShift).uint32 or
    (opCode.ord and SpvOpCodeMask).uint32

  stream.add(head)
  stream.add(operands)

proc writeOutput(m: SpirvModule) =
  let outFile = changeFileExt(completeCFilePath(m.config, m.filename), "spv")
  echo outFile

  var file: File
  if file.open(outFile, fmWrite):
    discard file.writeBuffer(addr m.words[0], m.words.len * sizeof(uint32))
    file.close()
  else:
    rawMessage(m.config, errCannotOpenFile, m.filename)

proc toWords(text: string): seq[uint32] =
  newSeq(result, (text.len + 1 + 3) div 4)
  for i, c in text:
    result[i div 4] = result[i div 4] or (c.uint32 shl ((i mod 4) * 8))

var level = 0

proc generateId(m: SpirvModule): uint32 =
  result = m.nextId
  inc m.nextId

iterator procParams(typ: PType): PNode =
  for a in typ.n.sons[1..^1]:
    let param = a.sym
    if isCompileTimeOnly(param.typ): continue
    yield a

proc procParams(typ: PType): seq[PNode] =
  accumulateResult(procParams(typ))

proc genVoidType(m: SpirvModule): SpirvId =
  if m.voidType == 0:
    m.voidType = m.generateId()
    m.typeWords.addInstruction(SpvOpTypeVoid, m.voidType)
  return m.voidType

proc genBoolType(m: SpirvModule): SpirvId =
  if m.boolType == 0:
    m.voidType = m.generateId()
    m.typeWords.addInstruction(SpvOpTypeBool, m.boolType)
  return m.boolType

proc bits(size: TypeWidth): uint32 =
  case size:
    of tw8: 8
    of tw16: 16
    of tw32: 32
    of tw64: 64

proc genIntType(m: SpirvModule; size: TypeWidth; isSigned: bool): SpirvId =
  result = m.intTypes[size][isSigned]
  if result == 0:
    result = m.generateId()
    m.intTypes[size][isSigned] = result
    m.typeWords.addInstruction(SpvOpTypeInt, result, size.bits, isSigned.uint32)

proc genFloatType(m: SpirvModule; size: TypeWidth): SpirvId =
  result = m.floatTypes[size]
  if result == 0:
    result = m.generateId()
    m.floatTypes[size] = result
    m.typeWords.addInstruction(SpvOpTypeFloat, result, size.bits)
  
proc genType(m: SpirvModule; t: PType): SpirvId =

  case t.kind:
    of tyVoid: return m.genVoidType()
    of tyBool: return m.genBoolType()

    #of tyFloat16: return m.genFloatType(tw16)
    of tyFloat32, tyFloat: return m.genFloatType(tw32)
    of tyFloat64: return m.genFloatType(tw64)
    
    of tyInt8: return m.genIntType(tw8, false)
    of tyInt16: return m.genIntType(tw16, false)
    of tyInt32, tyInt: return m.genIntType(tw32, false)
    of tyInt64: return m.genIntType(tw64, false)

    of tyUInt8: return m.genIntType(tw8, true)
    of tyUInt16: return m.genIntType(tw16, true)
    of tyUInt32, tyUInt: return m.genIntType(tw32, true)
    of tyUInt64: return m.genIntType(tw64, true)

    of tyGenericInst:
      # The last child is concrete, unless this is an alias.
      # Then it's another generic inst.
      var inst = t
      if inst.lastSon.kind == tyGenericInst:
        inst = inst.lastSon

      for x in m.types:
        if sameType(t, x[0]):
          return x[1]

      if sfImportc in inst[0].lastSon.sym.flags:
        result = m.generateId()
        if inst[0].lastSon.sym.name.s == "Vector":
          m.typeWords.addInstruction(SpvOpTypeVector, result, m.genType(inst[1]), inst[2].n.intVal.uint32)
        elif inst[0].lastSon.sym.name.s == "MatrixBase":
          m.typeWords.addInstruction(SpvOpTypeMatrix, result, m.genType(inst[1]), inst[2].n.intVal.uint32)
        # else: internalError(m.config, body.sym.loc, "Unkown intrinsic type " & $body)

      m.types.add((t, result))

    of tyVar:
      return m.genType(t[0])

    of tyDistinct, tyAlias, tyInferred: return m.genType(t.lastSon)

    of tyObject, tyTuple:
      for a in m.types:
        if sameType(a[0], t):
          return a[1]

      result = m.generateId()
      var memberTypes = newSeq[SpirvId]()
      for i, member in t.n.pairs:
        memberTypes.add(m.genType(member.typ))
        m.nameWords.addInstruction(SpvOpMemberName, @[result, i.uint32] & member.sym.name.s.toWords())

        m.decorationWords.addInstruction(SpvOpDecorate, result, SpvDecorationBlock.uint32)

        # TODO: Only when used in uniforms
        m.decorationWords.addInstruction(SpvOpMemberDecorate, result, i.uint32, SpvDecorationOffset.uint32, member.sym.offset.uint32)
        if ($member.typ).startsWith("Matrix"):
          # TODO: Handle row major
          m.decorationWords.addInstruction(SpvOpMemberDecorate, result, i.uint32, SpvDecorationColMajor.uint32)
          m.decorationWords.addInstruction(SpvOpMemberDecorate, result, i.uint32, SpvDecorationMatrixStride.uint32, 16'u32)

      m.typeWords.addInstruction(SpvOpTypeStruct, @[result] & memberTypes)

      m.types.add((t, result))

    else:
      discard

proc genPointerType(m: SpirvModule; valueType: SpirvId; storageClass: SpvStorageClass): SpirvId =
  let key = (valueType, storageClass)
  if m.pointerTypes.contains(key):
    return m.pointerTypes[key]

  result = m.generateId()
  m.pointerTypes.add(key, result)
  m.typeWords.addInstruction(SpvOpTypePointer, result, storageClass.uint32, valueType)

func toWords(value: SomeNumber): seq[uint32] =
  const wordCount = (sizeof(value) + 3) div 4

  result = newSeq[uint32](wordCount)
  var word = cast[BiggestUInt](value)
  for i in 0 ..< wordCount:
    result[i] = cast[uint32](word)
    word = word shr 32
  
  # Sign extend integers
  when value is SomeSignedInt:
    if value < 0:
      const
        bitsInLastWord = (sizeof(value) mod 4) * 8
        mask = not ((1 shl bitsInLastWord) - 1).uint32 
      result[^1] = result[^1] or mask
    
proc genConstant(m: SpirvModule; valueType: SpirvId; value: SomeInteger): SpirvId =
  let key = (valueType, value.BiggestInt)
  if m.intConstants.contains(key):
    return m.intConstants[key]

  result = m.generateId()
  m.intConstants.add(key, result)
  m.constantWords.addInstruction(SpvOpConstant, @[valueType, result] & value.toWords())

proc genConstant(m: SpirvModule; valueType: SpirvId; value: SomeFloat): SpirvId =
  let key = (valueType, value.BiggestFloat)
  if m.floatConstants.contains(key):
    return m.floatConstants[key]

  result = m.generateId()
  m.floatConstants.add(key, result)
  m.constantWords.addInstruction(SpvOpConstant, @[valueType, result] & value.toWords())
  
proc genConstant(m: SpirvModule; n: PNode): SpirvId =
  case n.typ.kind:
    of tyInt8: return m.genConstant(m.genType(n.typ), n.intVal.int8)
    of tyInt16: return m.genConstant(m.genType(n.typ), n.intVal.int16)
    of tyInt, tyInt32: return m.genConstant(m.genType(n.typ), n.intVal.int32)
    of tyInt64: return m.genConstant(m.genType(n.typ), n.intVal.int64)

    of tyUInt8: return m.genConstant(m.genType(n.typ), n.intVal.uint8)
    of tyUInt16: return m.genConstant(m.genType(n.typ), n.intVal.uint16)
    of tyUInt, tyUInt32: return m.genConstant(m.genType(n.typ), n.intVal.uint32)
    of tyUInt64: return m.genConstant(m.genType(n.typ), n.intVal.uint64)

    of tyFloat, tyFloat32: return m.genConstant(m.genType(n.typ), n.floatVal.float32)
    of tyFloat64: return m.genConstant(m.genType(n.typ), n.floatVal.float64)

    else: discard

proc genBoolConstant(m: SpirvModule; value: bool): SpirvId =
  result = if value: m.trueConstant else: m.falseConstant
  if result == 0:
    result = m.generateId()
    let op = if value: SpvOpConstantTrue else: SpvOpConstantFalse
    if value: m.trueConstant = result
    else: m.falseConstant = result
    m.constantWords.addInstruction(op, result)

proc genParamType(m: SpirvModule; t: PType): SpirvId = discard

proc genFunctionType(m: SpirvModule; t: PType): SpirvFunctionType =

  let returnType =
    if t.sons[0] == nil: m.genVoidType()
    else: m.genType(t.sons[0])

  var argTypes = newSeq[SpirvId]()

  for param in t.procParams():
    let paramType = param.sym.typ.skipTypes({ tyGenericInst, tyAlias, tySink })
    argTypes.add(m.genParamType(paramType))

    # if skipTypes(t, {tyVar}).kind in { tyOpenArray, tyVarargs }:
    #   argTypes.add(m.intType)  # Extra length parameter

  for knownType in m.functionTypes:
    if returnType != knownType.returnType: continue

    var found = true
    for i, argType in knownType.argTypes:
      if argTypes[i] != argType: found = false

    if found:
      return knownType

  new(result)
  result.id = m.generateId()
  result.returnType = returnType
  result.argTypes = argTypes
  m.functionTypes.add(result)

  m.typeWords.addInstruction(SpvOpTypeFunction, result.id, returnType) 

proc genFunction(m: SpirvModule; s: PSym): SpirvFunction =
  
  if m.functions.contains(s.id):
    return m.functions[s.id]

  let functionType = m.genFunctionType(s.typ)

  new(result)
  result.usedVariables = initSet[SpirvVariable]()
  result.symbol = s
  result.id = m.generateId()
  let labelId = m.generateId()

  m.functions.add(s.id, result)

  result.words.addInstruction(SpvOpFunction, functionType.returnType, result.id, 0'u32, functionType.id)
  result.words.addInstruction(SpvOpLabel, labelId)

  m.currentFunction = result
  discard m.genNode(s.getBody())
  m.currentFunction = nil

  if functionType.returnType == m.genVoidType():
    result.words.addInstruction(SpvOpReturn)
  result.words.addInstruction(SpvOpFunctionEnd)  

proc genIdentDefs(m: SpirvModule; n: PNode): SpirvVariable =

  new(result)
  result.id = m.generateId()

  if n[0].kind == nkPragmaExpr:
    result.symbol = n[0][0].sym
  else:
    result.symbol = n[0].sym

  if result.symbol.flags.contains(sfGlobal) or m.currentFunction == nil:
    result.storageClass = SpvStorageClassPrivate
  else:
    result.storageClass = SpvStorageClassFunction

  if n[0].kind == nkPragmaExpr:
    
    for pragma in n.sons[0][1]:
      if pragma.kind == nkExprColonExpr and pragma[0].kind == nkSym:
        case pragma[0].sym.name.s.normalize():
          of "location": m.decorationWords.addInstruction(SpvOpDecorate, result.id, SpvDecorationLocation.uint32, pragma[1].intVal.uint32)
          of "descriptorset": m.decorationWords.addInstruction(SpvOpDecorate, result.id, SpvDecorationDescriptorSet.uint32, pragma[1].intVal.uint32)
          of "binding": m.decorationWords.addInstruction(SpvOpDecorate, result.id, SpvDecorationBinding.uint32, pragma[1].intVal.uint32)
          of "builtin":
            var builtIn: SpvBuiltIn
            case pragma[1].ident.s.normalize():
              of "position": builtIn = SpvBuiltInPosition 
              else: discard
            m.decorationWords.addInstruction(SpvOpDecorate, result.id, SpvDecorationBuiltIn.uint32, builtIn.uint32)
          else: discard
      elif pragma.kind == nkSym:
        case pragma.sym.name.s:
          of "input": result.storageClass = SpvStorageClassInput
          of "output": result.storageClass = SpvStorageClassOutput
          of "uniform": result.storageClass = SpvStorageClassUniform
          else: discard

  m.variables.add(result.symbol.id, result)

  let 
    variableType = m.genType(result.symbol.typ)
    pointerType = m.genPointerType(variableType, result.storageClass)

  # TODO: Initializer
  result.words.addInstruction(SpvOpVariable, pointerType, result.id, result.storageClass.uint32)

proc genSons(m: SpirvModule; n: PNode) =
  for s in n: discard m.genNode(s)

proc genNode(m: SpirvModule; n: PNode): SpirvId =

  if sfMainModule notin m.module.flags:
    return

  case n.kind:
    of nkEmpty: discard

    # TODO: Interpret as expected type, if it's used in an expression, so no cast is necessary
    of nkIntLit .. nkFloat128Lit: return m.genConstant(n)

    of nkHiddenAddr:
      return m.genNode(n[0])

    of nkSym:
      if m.variables.contains(n.sym.id):
        let variable = m.variables[n.sym.id]
        m.currentFunction.usedVariables.incl(variable)
        return variable.id

    of nkAsgn:
      let temp = m.generateId()
      m.words.addInstruction(SpvOpLoad, m.genType(n[1].typ), temp, m.genNode(n[1]))
      m.words.addInstruction(SpvOpStore, m.genNode(n[0]), temp)

    of nkCallKinds:

      if n[0].sym.name.s == "[]=":
        let
          base = m.genNode(n[1])
          left = m.generateId()
          leftType = m.genType(n[3].typ)

        m.words.addInstruction(SpvOpAccessChain, m.genPointerType(leftType, SpvStorageClassOutput), left, base, m.genNode(n[2]))
        m.words.addInstruction(SpvOpStore, left, m.genNode(n[3]))

      elif n[0].sym.name.s == "[]":
        let
          base = m.genNode(n[1])
          temp = m.generateId()
          resultType = m.genType(n.typ)
        result = m.generateId()

        m.words.addInstruction(SpvOpAccessChain, m.genPointerType(resultType, SpvStorageClassInput), temp, base, m.genNode(n[2]))
        m.words.addInstruction(SpvOpLoad, resultType, result, temp)

    #of nkIdentDefs: discard m.genIdentDefs(n)
    of nkProcDef, nkFuncDef, nkMethodDef, nkIteratorDef, nkConverterDef: #m.genProcDef(n)

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
        let function = m.genFunction(s)
        for executionModel in executionModels:
          m.entryPoints.add((function, executionModel))

    of nkVarSection, nkLetSection, nkConstSection: #m.genSons(n)
      for child in n.sons:
        discard m.genIdentDefs(child)
      discard
    of nkStmtList: m.genSons(n)

    else: discard # internalError(n.info, "Unhandled node: " & $n)

proc myProcess(b: PPassContext, n: PNode): PNode =
  result = n
  if b == nil: return
  var m = SpirvModule(b)
  if passes.skipCodegen(m.config, n): return
  
  discard m.genNode(n)
  # var p = newProc(globals, m, nil, m.module.options)
  # p.unique = globals.unique
  # genModule(p, n)
  # add(p.m.code, p.locals)
  # add(p.m.code, p.body)

proc myClose(graph: ModuleGraph; b: PPassContext, n: PNode): PNode =
  
  result = n
  if b == nil: return
  var m = SpirvModule(b)
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
    
    # Collect the entry point's interface: referenced input/output variables (or a superset)
    # TODO: Include invoked functions too!
    var iface = newSeq[SpirvId]()
    for usedVariable in entryPoint.function.usedVariables:
      if usedVariable.storageClass in { SpvStorageClassInput, SpvStorageClassOutput }:
        iface.add(usedVariable.id)
        
    m.words.addInstruction(SpvOpEntryPoint,
      @[ord(entryPoint.executionModel).uint32, entryPoint.function.id] &
      entryPoint.function.symbol.name.s.toWords &
      iface)

  # ExecutionMode
  for entryPoint in m.entryPoints:
    if entryPoint.executionModel == SpvExecutionModelFragment:
      m.words.addInstruction(SpvOpExecutionMode, entryPoint.function.id, SpvExecutionModeOriginUpperLeft.uint32)

  # Debug instructions
    # Strings
    # SourceExtensions
    # Source
    # SourceContinued
    # Names
  m.words.add(m.nameWords)
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