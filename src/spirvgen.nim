import
  strutils, hashes, std / sha1, os, tables,
  times, math, intsets, options as opt

import ../compiler/[
  ast, astalgo, platform, magicsys, extccomp, trees, bitsets,
  nversion, nimsets, msgs, idents, types, options, ropes,
  passes, ccgutils, wordrecg, renderer, rodread, rodutils,
  cgmeth, lowerings, sighashes, modulegraphs]

import
  spirvTypes, glslTypes, openclTypes

type
  SpirvId = uint32

  SpirvGenObj = object of TPassContext
    words: seq[uint32]
    module: PSym
    nextId: uint32
    entryPoints: seq[tuple[function: SpirvFunction; executionModel: SpvExecutionModel]]
    functions: Table[int, SpirvFunction]
    functionTypes: seq[SpirvFunctionType]
    typeWords: seq[uint32]
    voidType: SpirvId
    boolType: SpirvId
    intTypes: array[32, array[2, SpirvId]]
    floatTypes: array[32, SpirvId]

  SpirvGen = ref SpirvGenObj

  SpirvFunction = ref object
    symbol: PSym
    id: SpirvId
    words: seq[SpirvId]

  SpirvFunctionType = ref object
    id: SpirvId
    returnType: SpirvId
    argTypes: seq[SpirvId]

proc newModule(module: PSym): SpirvGen =
  new(result)
  result.nextId = 1
  result.words = @[]
  result.module = module
  result.entryPoints = @[]
  result.functions = initTable[int, SpirvFunction]()
  result.typeWords = @[]
  # result.sigConflicts = initCountTable[SigHash]()
  # if globals == nil:
  #   globals = newGlobals()

proc addInstruction(stream: var seq[uint32]; opCode: SpvOp; operands: varargs[uint32]) =

  var wordCount = 1 + operands.len
  let head =
    ((wordCount and SpvOpCodeMask) shl SpvWordCountShift).uint32 or
    (opCode.ord and SpvOpCodeMask).uint32

  stream.add(head)
  stream.add(operands)

proc writeOutput(g: SpirvGen, project: string) =
  var outFile: string
  if options.outFile.len > 0:
    if options.outFile.isAbsolute:
      outFile = options.outFile
    else:
      outFile = getCurrentDir() / options.outFile
  else:
    outFile = project & ".spv"

  var file: File
  if file.open(outFile, fmWrite):
    discard file.writeBuffer(addr g.words[0], g.words.len * sizeof(uint32))
    file.close()
  else:
    errorHandler(rCannotOpenFile, outFile, false)

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

proc genType(g: SpirvGen; t: PType): SpirvId =
  case t.kind:
    of tyVoid: return g.genVoidType()
    of tyGenericInst: return g.genType(t.lastSon)
    of tyDistinct, tyAlias, tyInferred: return g.genType(t.lastSon)
    else: discard

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
  result.words = @[]

  g.functions.add(s.id, result)

  result.words.addInstruction(SpvOpFunction, functionType.id, result.id, 0'u32, functionType.returnType)
  #result.words.addInstruction(SpvOpLabel, 5'u32)
  result.words.addInstruction(SpvOpReturn)
  result.words.addInstruction(SpvOpFunctionEnd)  

proc genNode(g: SpirvGen; n: PNode) =
  # var text = spaces(level * 2) & $n.kind

  # case n.kind:
  #   of nkSym: text &= ": " & n.sym.name.s
  #   of nkIdent: text &= ": " & n.ident.s
  #   of nkStmtList: text &= ": " & $n.sonsLen
  #   else: discard
    
  # echo text

  # inc level

  # case n.kind:
  #   of nkCharLit..nkUInt64Lit: discard
  #   of nkFloatLit..nkFloat128Lit: discard
  #   of nkStrLit..nkTripleStrLit: discard
  #   of nkSym: discard
  #   of nkIdent: discard
  #   else:
  #     for child in n.sons:
  #       g.genNode(child)

  # dec level

  if sfMainModule notin g.module.flags:
    return

  case n.kind:
    of nkEmpty: discard
    of nkCallKinds: discard
    of nkProcDef, nkFuncDef, nkMethodDef, nkConverterDef: #g.genProcDef(n)

      let s = n.sons[namePos].sym

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

    else: discard # internalError(n.info, "Unhandled node: " & $n)

proc myProcess(b: PPassContext, n: PNode): PNode =
  if passes.skipCodegen(n): return n
  result = n

  var m = SpirvGen(b)
  if m.module == nil: internalError(n.info, "myProcess")
  
  m.genNode(n)
  # var p = newProc(globals, m, nil, m.module.options)
  # p.unique = globals.unique
  # genModule(p, n)
  # add(p.g.code, p.locals)
  # add(p.g.code, p.body)

proc myClose(graph: ModuleGraph; b: PPassContext, n: PNode): PNode =
  
  if passes.skipCodegen(n): return n
  result = n

  var m = SpirvGen(b)
  let glslId = m.generateId()
  m.genNode(n)

  # Header
  m.words.add(SpvMagicNumber)
  m.words.add(SpvVersion)
  m.words.add(0) # Generator tool specific magic number
  m.words.add(m.nextId) # Id bound
  m.words.add(0) # Reserved (Instruction schema)

  # Instruction stream

  # Capabilities
  m.words.addInstruction(SpvOpCapability, ord(SpvCapabilityShader))

  # Extensions

  # ExtInstImports
  m.words.addInstruction(SpvOpExtInstImport, @[glslId] & "GLSL.std.450".toWords)

  # MemoryModel
  m.words.addInstruction(SpvOpMemoryModel, ord(SpvAddressingModelLogical), ord(SpvMemoryModelGLSL450))

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
    # MemberDescorates
    # GroupDecorates
    # GroupMemberDecorates
    # DecorationsGroups
  # (Lines valid from here)
    # Types
  m.words.add(m.typeWords)
    # Constants
    # Non-function Variables
    # Undef
  # Function declarations (Functions, FunctionParameters, FunctionsEnds)
  # Function definitions

  for id, function in m.functions:
    m.words.add(function.words)

    # Blocks
      # Label (opt preceeded by Line)

  m.writeOutput(changeFileExt(gProjectFull, ""))

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

proc myOpenCached(graph: ModuleGraph; s: PSym, rd: PRodReader): PPassContext =
  internalError("Symbol files are not supported by the SPIR-V backend")
  result = nil

proc myOpen(graph: ModuleGraph; module: PSym; cache: IdentCache): PPassContext =
  return newModule(module)

const spirvGenPass* = makePass(myOpen, myOpenCached, myProcess, myClose)