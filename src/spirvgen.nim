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
  SpirvGenObj = object of TPassContext
    words: seq[uint32]
    module: PSym
    functions: Table[int, SpirvFunction]
    nextId: uint32

  SpirvGen = ref SpirvGenObj

  SpirvFunction = ref object
    symbol: PSym
    id: uint32
    body: seq[uint32]

proc newModule(module: PSym): SpirvGen =
  new(result)
  result.words = @[]
  result.module = module
  result.functions = initTable[int, SpirvFunction]()
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

proc genFunction(g: SpirvGen; s: PSym): SpirvFunction =
  
  if g.functions.contains(s.id):
    return g.functions[s.id]

  new(result)
  result.symbol = s
  result.id = g.generateId()

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
      discard g.genFunction(s)

      discard

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
  m.genNode(n)

  # Header
  m.words.add(SpvMagicNumber)
  m.words.add(SpvVersion)
  m.words.add(0) # Generator tool specific magic number
  m.words.add(100) # Id bound
  m.words.add(0) # Reserved (Instruction schema)

  # Instruction stream

  # Capabilities
  m.words.addInstruction(SpvOpCapability, ord(SpvCapabilityShader))

  # Extensions

  # ExtInstImports
  m.words.addInstruction(SpvOpExtInstImport, @[1'u32] & "GLSL.std.450".toWords)

  # MemoryModel
  m.words.addInstruction(SpvOpMemoryModel, ord(SpvAddressingModelLogical), ord(SpvMemoryModelGLSL450))

  # EntryPoint
  m.words.addInstruction(SpvOpEntryPoint, @[ord(SpvExecutionModelVertex).uint32, 1] & "main".toWords)

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
  m.words.addInstruction(SpvOpTypeVoid, 2'u32) 
  m.words.addInstruction(SpvOpTypeFunction, 3'u32, 2'u32) 
    # Constants
    # Non-function Variables
    # Undef
  # Function declarations (Functions, FunctionParameters, FunctionsEnds)
  # Function definitions

  m.words.addInstruction(SpvOpFunction, 2'u32, 4'u32, 0'u32, 2'u32)
  m.words.addInstruction(SpvOpLabel, 5'u32)
  m.words.addInstruction(SpvOpReturn)
  m.words.addInstruction(SpvOpFunctionEnd)

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