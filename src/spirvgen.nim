import
  strutils, hashes, std / sha1, os, tables, sets,
  times, math, intsets, options as opt

import ../compiler/[
  ast, astalgo, platform, magicsys, extccomp, trees, bitsets,
  nversion, nimsets, msgs, idents, types, options, ropes,
  passes, ccgutils, wordrecg, renderer, rodutils, msgs,
  cgmeth, lowerings, sighashes, modulegraphs, lineinfos, pathutils, transf]

import
  spirvTypes, glslTypes, openclTypes, spirvDfa

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
    strings: Table[string, SpirvId]
    fileNames: Table[FileIndex, SpirvId]
    entryPoints: seq[tuple[function: SpirvFunction; executionModel: SpvExecutionModel]]
    variables: Table[int, SpirvVariable]
    parameters: Table[int, SpirvId]
    functions: Table[int, SpirvFunction]
    functionTypes: seq[SpirvFunctionType]

    stringWords: seq[uint32]
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
    typ: SpirvFunctionType
    resultVariable: SpirvVariable
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

  InstructionKind = enum
    Goto, Fork, Use

  SpirvControlFlowInstruction = object
    n: PNode
    case kind: InstructionKind:
      of Use: sym: PSym
      else: dest: int

  SpirvControlFlowGraph = seq[SpirvControlFlowInstruction]

proc hash(self: SpvStorageClass): Hash = hash(self.ord)

proc hash(self: SpirvVariable): Hash = hash(self.id)

template config*(m: SpirvModule): ConfigRef = m.g.config

proc words(m: SpirvModule): var seq[uint32] =
  if m.currentFunction != nil:
    return m.currentFunction.words
  else:
    return m.allWords 

proc genNode(m: SpirvModule; n: PNode; load: bool = false): SpirvId

proc newModuleList*(m: ModuleGraph): SpirvModuleList =
  SpirvModuleList(modules: @[], config: m.config, graph: m)

proc rawNewModule(g: SpirvModuleList; module: PSym, filename: string): SpirvModule =
  new(result)
  result.g = g
  result.filename = filename
  result.nextId = 1
  result.module = module
  result.strings = initTable[string, SpirvId]()
  result.fileNames = initTable[FileIndex, SpirvId]()
  result.functions = initTable[int, SpirvFunction]()
  result.variables = initTable[int, SpirvVariable]()
  result.parameters = initTable[int, SpirvId]()
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
  if sfMainModule in m.module.flags:
    let outFile = m.config.prepareToWriteOutput()

    var file: File
    if file.open(outFile.string, fmWrite):
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

proc genLineInfo(m: SpirvModule; n: PNode) =
  var nameId = m.fileNames.getOrDefault(n.info.fileIndex, 0)
  if nameId == 0:
    var name = toFilename(m.config, n.info.fileIndex)
    nameId = m.strings.getOrDefault(name, 0)
    if nameId == 0:
      m.strings.add(name, nameId)
    m.fileNames.add(n.info.fileIndex, nameId)

  m.stringWords.addInstruction(SpvOpLine, nameId, n.info.line.uint32, n.info.col.uint32)

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
    m.boolType = m.generateId()
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

        var matrixLayout = SpvDecorationColMajor

        let src = member.sym.loc.lode
        if src != nil and src.kind == nkPragmaExpr:
          for pragma in src[1]:
            if pragma.kind == nkSym:
              case pragma.sym.name.s:
                of "rowMajor": matrixLayout = SpvDecorationRowMajor
                else: discard

        m.nameWords.addInstruction(SpvOpMemberName, @[result, i.uint32] & member.sym.name.s.toWords())

        m.decorationWords.addInstruction(SpvOpDecorate, result, SpvDecorationBlock.uint32)

        # TODO: Only when used in uniforms
        m.decorationWords.addInstruction(SpvOpMemberDecorate, result, i.uint32, SpvDecorationOffset.uint32, member.sym.position.uint32)
        if ($member.typ).startsWith("Matrix"):
          # TODO: Handle row major
          m.decorationWords.addInstruction(SpvOpMemberDecorate, result, i.uint32, matrixLayout.uint32)
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
  
proc genBoolConstant(m: SpirvModule; value: bool): SpirvId =
  result = if value: m.trueConstant else: m.falseConstant
  if result == 0:
    result = m.generateId()
    let op = if value: SpvOpConstantTrue else: SpvOpConstantFalse
    if value: m.trueConstant = result
    else: m.falseConstant = result
    m.constantWords.addInstruction(op, m.genBoolType(), result)

proc genConstant(m: SpirvModule; n: PNode): SpirvId =
  case n.typ.kind:
    of tyBool: return m.genBoolConstant(n.intVal != 0)

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

    else: internalError(m.g.config, "Unkhandle literal: " & $n & " (" & $n.typ.kind & ")")

proc genParamType(m: SpirvModule; t: PType): SpirvId =
  m.genPointerType(m.genType(t), SpvStorageClassFunction)

proc genFunctionType(m: SpirvModule; t: PType): SpirvFunctionType =

  let returnType =
    if t.sons[0] == nil: m.genVoidType()
    else: m.genType(t.sons[0])

  var argTypes = newSeq[SpirvId]()

  for param in t.procParams():
    let paramType = param.sym.typ#.skipTypes({ tyGenericInst, tyAlias, tySink })
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

  m.typeWords.addInstruction(SpvOpTypeFunction, @[result.id, returnType] & argTypes) 

proc genFunction(m: SpirvModule; s: PSym): SpirvFunction =
  
  if m.functions.contains(s.id):
    return m.functions[s.id]

  # Lazy transformation
  let body = transformBody(m.g.graph, s, cache = false)

  new(result)
  result.typ = m.genFunctionType(s.typ)
  result.usedVariables = initSet[SpirvVariable]()
  result.symbol = s
  result.id = m.generateId()
  m.functions.add(s.id, result)  

  # Debug info
  m.nameWords.addInstruction(SpvOpName, @[result.id] & s.name.s.toWords())

  # Header
  let labelId = m.generateId()
  result.words.addInstruction(SpvOpFunction, result.typ.returnType, result.id, 0'u32, result.typ.id)

  for i, paramType in result.typ.argTypes:
    let paramId = m.generateId()
    result.words.addInstruction(SpvOpFunctionParameter, paramType, paramId)
    let paramSym = s.typ.n[i + 1].sym
    m.parameters[paramSym.id] = paramId

    m.nameWords.addInstruction(SpvOpName, @[paramId] & paramSym.name.s.toWords())

  result.words.addInstruction(SpvOpLabel, labelId)
  
  # Create the implicit result variable
  let hasResult = result.typ.returnType != m.genVoidType()
  if hasResult:
    new(result.resultVariable)
    result.resultVariable.id = m.generateId()
    result.resultVariable.storageClass = SpvStorageClassFunction

    result.words.addInstruction(SpvOpVariable, m.genPointerType(result.typ.returnType, SpvStorageClassFunction), result.resultVariable.id, result.resultVariable.storageClass.uint32)
    m.nameWords.addInstruction(SpvOpName, @[result.resultVariable.id] & "result".toWords())
    #m.variables.add(result.symbol.id, result) # Not visible

  # Generate the function  body
  let previousFunction = m.currentFunction
  m.currentFunction = result

  #m.g.graph.dfa(s, body)

  var returnValue = m.genNode(body, hasResult)
  m.currentFunction = previousFunction

  # Return the result. This can be the value of the body, if it's an expression, or the value of the result variable.
  if hasResult:
    if returnValue == 0:
      returnValue = m.generateId()
      result.words.addInstruction(SpvOpLoad, result.typ.returnType, returnValue, result.resultVariable.id)
    result.words.addInstruction(SpvOpReturnValue, returnValue)
  else:
    result.words.addInstruction(SpvOpReturn)

  result.words.addInstruction(SpvOpFunctionEnd)

proc genIdentDefs(m: SpirvModule; n: PNode): SpirvVariable =

  new(result)
  result.id = m.generateId()
  result.symbol = n[0].sym

  if result.symbol.flags.contains(sfGlobal) or m.currentFunction == nil:
    result.storageClass = SpvStorageClassPrivate
  else:
    result.storageClass = SpvStorageClassFunction

  # No lode for skForVar
  let src = n[0].sym.loc.lode
  if src != nil and src.kind == nkPragmaExpr:
    
    for pragma in src[1]:
      if pragma.kind == nkExprColonExpr and pragma[0].kind == nkSym:
        case pragma[0].sym.name.s.normalize():
          of "location": m.decorationWords.addInstruction(SpvOpDecorate, result.id, SpvDecorationLocation.uint32, pragma[1].intVal.uint32)
          of "descriptorset": m.decorationWords.addInstruction(SpvOpDecorate, result.id, SpvDecorationDescriptorSet.uint32, pragma[1].intVal.uint32)
          of "binding": m.decorationWords.addInstruction(SpvOpDecorate, result.id, SpvDecorationBinding.uint32, pragma[1].intVal.uint32)
          of "builtin":
            block found:
              for builtIn in SpvBuiltIn:
                if ($builtIn).normalize().startsWith(("SpvBuiltIn" & pragma[1].ident.s).normalize()):
                  m.decorationWords.addInstruction(SpvOpDecorate, result.id, SpvDecorationBuiltIn.uint32, builtIn.uint32)
                  break found
              internalError(m.g.config, pragma[1].info, "Unhandled value: " & $pragma[1])
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
  
  m.nameWords.addInstruction(SpvOpName, @[result.id] & result.symbol.name.s.toWords())

proc genIntrinsic(m: SpirvModule; op: SpvOp; n: PNode): SpirvId =
  result = m.generateId()
  var args: seq[SpirvId]
  for i in 1 ..< n.sonsLen:
    args.add(m.genNode(n[i], true))
  m.words.addInstruction(op, @[m.genType(n.typ), result] & args)

proc genMagic(m: SpirvModule; n: PNode): SpirvId =
  var op: SpvOp
  case n[0].sym.magic:
    # of mHigh: result = m.genMagicHigh(n)
    # of mSizeOf: result = m.genMagicSizeOf(n)
    # of mOf: result = m.genMagicOf(n)
    # of mEcho: m.genMagicEcho(n)
    # of mUnaryLt: result = m.genMagicUnaryLt(n)
    of mInc:
      let
        op = if n[1].typ.kind in tyFloat..tyFloat128: SpvOpFAdd else: SpvOpIAdd
        temp = m.generateId()
      m.words.addInstruction(op, @[m.genType(n[1].typ), temp, m.genNode(n[1], true), m.genNode(n[2], true)])
      m.words.addInstruction(SpvOpStore, m.genNode(n[1]), temp)
      return
      
    # of mDec: m.genIntrinsic()
    # of mOrd: result = m.genMagicOrd(n)
    # of mNew: m.genMagicNew(n)
    # of mNewFinalize: m.genMagicNewFinalize(n)
    # of mNewSeq: m.genMagicNewSeq(n)
    # of mNewSeqOfCap: result = m.genMagicNewSeqOfCap(n)
    # of mLengthOpenArray: result = m.genMagicLength(n)
    # of mLengthStr: result = m.genMagicLength(n)
    # of mLengthArray: result = m.genMagicLength(n)
    # of mLengthSeq: result = m.genMagicLength(n)
    # of mXLenStr, mXLenSeq: result = m.genMagicXLen(n)
    # of mIncl: m.genMagicIncl(n)
    # of mExcl: m.genMagicExcl(n)
    # of mCard: result = m.genMagicCard(n)
    # of mChr: result = m.genMagicChr(n)
    # of mGCref: m.genMagicGCref(n)
    # of mGCunref: m.genMagicGCunref(n)
    of mAddI: op = SpvOpIAdd
    of mSubI: op = SpvOpIAdd
    of mMulI: op = SpvOpIMul
    of mDivI: op = SpvOpSDiv
    of mModI: op = SpvOpSMod
    # of mSucc: result = m.genMagicBinOp(n, llvm.Add)
    # of mPred: result = m.genMagicBinOp(n, llvm.Sub)
    of mAddF64: op = SpvOpFAdd
    of mSubF64: op = SpvOpFSub
    of mMulF64: op = SpvOpFMul
    of mDivF64: op = SpvOpFDiv
    of mShrI: op = SpvOpShiftRightLogical
    of mShlI: op = SpvOpShiftLeftLogical
    of mBitandI: op = SpvOpBitwiseAnd
    of mBitorI: op = SpvOpBitwiseOr
    of mBitxorI: op = SpvOpBitwiseXor
    # of mMinI: op = SpvOpIMin # sign
    # of mMaxI: op = SpvOpIMax # sign
    # of mMinF64: op = SpvOpFMin
    # of mMaxF64: result = m.genIntrinsicF(SpvOpFMax
    of mAddU: op = SpvOpIAdd
    of mSubU: op = SpvOpISub
    of mMulU: op = SpvOpIMul
    of mDivU: op = SpvOpUDiv
    of mModU: op = SpvOpUMod
    of mEqI: op = SpvOpIEqual
    of mLeI: op = SpvOpSLessThanEqual
    of mLtI: op = SpvOpSLessThan
    of mEqF64: op = SpvOpFOrdEqual
    of mLeF64: op = SpvOpFOrdLessThanEqual
    of mLtF64: op = SpvOpFOrdLessThan
    of mLeU: op = SpvOpULessThanEqual
    of mLtU: op = SpvOpSLessThan
    of mLeU64: op = SpvOpULessThanEqual
    of mLtU64: op = SpvOpSLessThan
    # of mEqEnum: op =n, llvm.IntEQ)
    # of mLeEnum: op =n, llvm.IntULE) # TODO underlying
    # of mLtEnum: op =n, llvm.IntULT) # TODO underlying
    # of mEqCh: op =n, llvm.IntEQ)
    # of mLeCh: op =n, llvm.IntULE)
    # of mLtCh: op =n, llvm.IntULT)
    of mEqB: op = SpvOpLogicalEqual
    # of mLeB: op =n, llvm.IntULE)
    # of mLtB: op =n, llvm.IntULT)
    # of mEqRef: op =n, llvm.IntEQ)
    # of mEqUntracedRef: result = m.genMagicCmpI(n, llvm.IntEQ)
    # of mLePtr: result = m.genMagicCmpI(n, llvm.IntULE)
    # of mLtPtr: result = m.genMagicCmpI(n, llvm.IntULT)
    # of mEqCString: result = m.genMagicCmpI(n, llvm.IntEQ)
    # of mXor: result = m.genMagicCmpI(n, llvm.IntNE)
    # of mEqProc: result = m.genMagicEqProc(n)
    of mUnaryMinusI, mUnaryMinusI64: op = SpvOpSNegate
    # of mAbsI: op = SpvOpIAbs
    # of mNot: result = m.genMagicNot(n)
    # of mBitnotI: result = m.genMagicBitnot(n)
    of mUnaryMinusF64: op = SpvOpFNegate
    # of mAbsF64: result = m.genMagicAbsF64(n)
    #of mZe8ToI..mZeIToI64: result = m.genMagicZe(n)
    # of mToU8, mToU16, mToU32: result = m.genMagicToU(n)
    # of mToFloat, mToBiggestFloat: result = m.genMagicToFloat(n)
    # of mToInt, mToBiggestInt: result = m.genMagicToInt(n)
    # of mCharToStr: result = m.genMagicToStr(n, "nimCharToStr")
    # of mBoolToStr: result = m.genMagicToStr(n, "nimBoolToStr")
    # of mIntToStr: result = m.genMagicToStr(n, "nimIntToStr")
    # of mInt64ToStr: result = m.genMagicToStr(n, "nimInt64ToStr")
    # of mFloatToStr: result = m.genMagicToStr(n, "nimFloatToStr")
    # of mCStrToStr: result = m.genMagicToStr(n, "cstrToNimstr")
    # of mStrToStr: result = m.genMagicStrToStr(n)
    # of mEnumToStr: result = m.genMagicEnumToStr(n)
    # of mAnd, mOr: result = m.genMagicAndOr(n)
    # of mEqStr: result = m.genMagicEqStr(n)
    # of mLeStr: result = m.genMagicLeStr(n)
    # of mLtStr: result = m.genMagicLtStr(n)
    # of mEqSet: result = m.genMagicEqSet(n)
    # of mLeSet: result = m.genMagicSetCmp(false
    # of mLtSet: result = m.genMagicSetCmp(true
    # of mMulSet: result = m.genMagicSetBinOp(llvm.And, false
    # of mPlusSet: result = m.genMagicSetBinOp(llvm.Or, false
    # of mMinusSet: result = m.genMagicSetBinOp(llvm.And, true
    # of mSymDiffSet: result = m.genMagicSetBinOp(llvm.Xor, false
    # of mConStrStr: result = m.genMagicConStrStr(n)
    # of mDotDot: result = m.genMagicDotDot(n, load)
    # of mAppendStrCh: m.genMagicAppendStrCh(n)
    # of mAppendStrStr: m.genMagicAppendStrStr(n)
    # of mAppendSeqElem: m.genMagicAppendSeqElem(n)
    # of mInSet: result = m.genMagicInSet(n)
    # of mRepr: result = m.genMagicRepr(n)
    # of mExit: discard m.genCall(n, false)
    # of mSetLengthStr: m.genMagicSetLengthStr(n)
    # of mSetLengthSeq: m.genMagicSetLengthSeq(n)
    # of mParallel: m.genMagicParallel(n)
    # of mSwap: m.genMagicSwap(n)
    # of mReset: m.genMagicReset(n)
    # of mIsNil: result = m.genMagicIsNil(n)
    # of mArrToSeq: result = m.genMagicArrToSeq(n)
    # of mCopyStr, mCopyStrLast, mNewString, mNewStringOfCap, mParseBiggestFloat:
    #   result = m.genMagicCall(n, load)
    # of mSpawn: result = m.genMagicSpawn(n)
    # of mDeepCopy: m.genMagicDeepCopy(n)
    # of mGetTypeInfo: result = m.genMagicGetTypeInfo(n)
    else: internalError(m.config, n.info, "Unhandled magic: " & $n[0].sym.magic)

  m.genIntrinsic(op, n)

proc genIfRecursive(m: SpirvModule; n: PNode; index: int; resultId: SpirvId) =

  let isExpression = n.kind == nkIfExpr
  if n[index].kind in { nkElifBranch, nkElifExpr }:
    let isLastBranch = index + 1 >= n.len

    let
      mergeId = m.generateId()
      trueId = m.generateId()
      falseId = if isLastBranch: mergeId else: m.generateId()
      
    # TODO: Flatten/don't flatten
    # TODO: Branch weights? Likely/unlikely?
    m.words.addInstruction(SpvOpSelectionMerge, mergeId, SpvSelectionControlMaskNone.uint32)
    m.words.addInstruction(SpvOpBranchConditional, m.genNode(n[index][0], true), trueId, falseId)

    # True branch
    m.words.addInstruction(SpvOpLabel, trueId)
    let branchResult = m.genNode(n[index][1], isExpression)
    if isExpression:
      m.words.addInstruction(SpvOpStore, resultId, branchResult)

    # False branch
    if not isLastBranch:
      m.words.addInstruction(SpvOpLabel, falseId)
      m.words.addInstruction(SpvOpBranch, mergeId) 
      m.genIfRecursive(n, index + 1, resultId)

    m.words.addInstruction(SpvOpLabel, mergeId)

  # Else branch
  else:
    let branchResult = m.genNode(n[index][0], isExpression)
    if isExpression:
      m.words.addInstruction(SpvOpStore, resultId, branchResult)

proc genIf(m: SpirvModule; n: PNode; load: bool): SpirvId =
  var
    resultId: SpirvId
    resultType: SpirvId

  # If this is an expression, store the branch result in a temporary variable
  # TODO: Use OpPhi
  if n.kind == nkIfExpr:
    resultId = m.generateId()
    resultType = m.genType(n.typ)
    m.words.addInstruction(SpvOpVariable, m.genPointerType(resultType, SpvStorageClassFunction), resultId, SpvStorageClassFunction.uint32)

  # Generate branches, recursively creating structured control-flow blocks
  m.genIfRecursive(n, 0, resultId)

  # Load and return the temporary variable's value
  if n.kind == nkIfExpr:
    result = m.generateId()
    m.words.addInstruction(SpvOpLoad, resultType, result, resultId)

proc genBlock(m: SpirvModule; n: PNode; load: bool): SpirvId =
  result = m.genNode(n[1], load)
  # m.words.addInstruction(SpvOpLabel, )
  # return result

proc genWhile(m: SpirvModule; n: PNode): SpirvId =
  let
    headerId = m.generateId()
    bodyId = m.generateId()
    continueId = m.generateId()
    mergeId = m.generateId()

  # Header block and loop condition
  m.words.addInstruction(SpvOpLabel, headerId) # TODO: This should be the surrounding blocks id
  m.words.addInstruction(SpvOpLoopMerge, mergeId, continueId, SpvLoopControlMaskNone.uint32)
  m.words.addInstruction(SpvOpBranchConditional, m.genNode(n[0], true), bodyId, mergeId)

  # Loop body
  m.words.addInstruction(SpvOpLabel, bodyId)
  discard m.genNode(n[1])

  # Continue target and back-edge block
  m.words.addInstruction(SpvOpLabel, continueId)
  m.words.addInstruction(SpvOpBranch, headerId)

  # Merge block
  m.words.addInstruction(SpvOpLabel, mergeId)

proc genNestedBreak(m: SpirvModule; blockSym: PSym; depth: int): SpirvId =
  discard
  # # If this block has a break path to the target block, reuse it
  # let b = m.currentBlocks[^(depth + 1)]
  # if blockSym.id in b.breakTargets:
  #   return b.breakTargets[blockSym.id]

  # # Otherwise generate an id for the target label.
  # # The label will be emitted when the target block gets finished up.
  # let
  #   targetId = m.generateId()
  #   parent
  # b.breakTargets.add(blockSym)
  # m.genNestedBreak(blockSym, depth + 1)
  # for b in countdown(m.currentBlocks.high, 0):
  #   var breakTarget = breakTargets
  #   if blockSym.id in b.breakTargets

proc genBreak(m: SpirvModule; n: PNode): SpirvId =
  let targetId = m.genNestedBreak(n[0].sym, 0)
  m.words.addInstruction(SpvOpBranch, targetId)

proc genSons(m: SpirvModule; n: PNode, load: bool = false): SpirvId =
  for i in 0 ..< n.len - 1:
    discard m.genNode(n[i], false)

  # The result of a statement list is the value of the last statement (if it is an expression statement)
  if n.len > 0:
    return m.genNode(n.lastSon, load)

proc genNode(m: SpirvModule; n: PNode, load: bool = false): SpirvId =

  if sfMainModule notin m.module.flags:
    return

  case n.kind:
    of nkEmpty: discard
    
    of nkCommentStmt, nkIteratorDef, nkIncludeStmt,
      nkImportStmt, nkImportExceptStmt, nkExportStmt, nkExportExceptStmt,
      nkFromStmt, nkTemplateDef, nkMacroDef, nkStaticStmt:
      discard

    of nkTypeSection: discard

    # TODO: Interpret as expected type, if it's used in an expression, so no cast is necessary
    of nkCharLit..nkFloat128Lit: return m.genConstant(n)

    of nkHiddenAddr:
      return m.genNode(n[0])

    of nkSym:
      var variableId: SpirvId
      if n.sym.kind == skResult:
        variableId = m.currentFunction.resultVariable.id

      elif n.sym.kind == skParam:
        variableId = m.parameters[n.sym.id]

      elif m.variables.contains(n.sym.id):
        let variable = m.variables[n.sym.id]
        variableId = variable.id
        m.currentFunction.usedVariables.incl(variable)

      if variableId != 0:
        if load:
          result = m.generateId()
          let resultType = m.genType(n.typ)
          m.words.addInstruction(SpvOpLoad, resultType, result, variableId)
        else:
          return variableId
      else:
        internalError(m.config, "Unknown symbol " & $n.sym.name.s)

    of nkAsgn, nkFastAsgn:
      m.words.addInstruction(SpvOpStore, m.genNode(n[0]), m.genNode(n[1], true))

    of nkDotExpr:
      let
        temp = m.generateId()
        resultType = m.genType(n.typ)
      result = m.generateId()

      # TODO: Generate proper access chain for non-variables
      let variable = m.variables[n[0].sym.id]
      m.currentFunction.usedVariables.incl(variable)

      m.words.addInstruction(SpvOpAccessChain, m.genPointerType(resultType, variable.storageClass), temp, variable.id,
        m.genConstant(m.genType(getSysType(m.g.graph, unknownLineInfo(), tyUint32)), n[1].sym.position.uint32))
      m.words.addInstruction(SpvOpLoad, resultType, result, temp)

    of nkReturnStmt:
      if n.len > 0 and n[0].kind == nkEmpty:
        m.words.addInstruction(SpvOpReturnValue, m.genNode(n[0], true))
      elif m.currentFunction.resultVariable != nil:
        var temp = m.generateId()
        m.words.addInstruction(SpvOpLoad, m.currentFunction.typ.returnType, temp, m.currentFunction.resultVariable.id)
        m.words.addInstruction(SpvOpReturnValue, temp)
      else:
        m.words.addInstruction(SpvOpReturn)

    of nkCallKinds:

      if n[0].sym.magic != mNone:
        return m.genMagic(n)
 
      elif n[0].sym.name.s == "[]=":
        let
          left = m.generateId()
          leftType = m.genType(n[3].typ)

        # TODO: Generate proper access chain for non-variables
        let variable = m.variables[n[1].skipHidden() .sym.id]
        m.currentFunction.usedVariables.incl(variable)

        m.words.addInstruction(SpvOpAccessChain, m.genPointerType(leftType, variable.storageClass), left, variable.id, m.genNode(n[2]))
        m.words.addInstruction(SpvOpStore, left, m.genNode(n[3]))

      elif n[0].sym.name.s == "[]":
        let
          temp = m.generateId()
          resultType = m.genType(n.typ)
        result = m.generateId()

        # TODO: Generate proper access chain for non-variables
        let variable = m.variables[n[1].sym.id]
        m.currentFunction.usedVariables.incl(variable)

        m.words.addInstruction(SpvOpAccessChain, m.genPointerType(resultType, variable.storageClass), temp, variable.id, m.genNode(n[2]))
        m.words.addInstruction(SpvOpLoad, resultType, result, temp)

      elif sfImportc notin n[0].sym.flags:
        let function = m.genFunction(n[0].sym)
        var args = newSeq[SpirvId](n.len - 1)
        for i in 1 ..< n.len:
          let
            valueId = m.genNode(n[i])
            varId = m.generateId()
          m.words.addInstruction(SpvOpVariable, m.genPointerType(m.genType(n[i].typ), SpvStorageClassFunction) , varId, SpvStorageClassFunction.uint32)
          m.words.addInstruction(SpvOpStore, varId, m.genNode(n[i], true))
          args[i - 1] = varId

        result = m.generateId()
        m.words.addInstruction(SpvOpFunctionCall, @[function.typ.returnType, result, function.id] & args)

      else:
        # TODO: Build lookup
        for op in SpvOp:
          if $op == "Spv" & $n[0].sym.loc.r:
            return m.genIntrinsic(op, n)

        internalError(m.config, "Unhandled SPIR-V intrinsic " & $n[0].sym.loc.r)

    of nkProcDef, nkFuncDef, nkConverterDef: #m.genProcDef(n)

      let s = n.sons[namePos].sym
      if sfImportc in s.flags:
        return

      var executionModels: set[SpvExecutionModel]

      for pragma in n[pragmasPos]:
        if pragma.kind == nkExprColonExpr and
           pragma[0].kind == nkSym and
           pragma[0].sym.name.s.normalize() == "stage":

          block found:
            for executionModel in SpvExecutionModel:
              if ($executionModel).normalize().startsWith(("SpvExecutionModel" & pragma[1].ident.s).normalize()):
                executionModels.incl(executionModel)
                break found
            internalError(m.g.config, pragma[1].info, "Unhandled value: " & $pragma[1])
      
      if executionModels != {}:
        let function = m.genFunction(s)
        for executionModel in executionModels:
          m.entryPoints.add((function, executionModel))

    of nkVarSection, nkLetSection, nkConstSection: #m.genSons(n)
      for child in n.sons:
        discard m.genIdentDefs(child)

    of nkBlockStmt: return m.genBlock(n, load)

    of nkStmtList:
      return m.genSons(n, load)

    of nkIfStmt, nkIfExpr: return m.genIf(n, load)

    of nkWhileStmt: return m.genWhile(n)

    of nkBreakStmt: discard

    of nkDiscardStmt: return m.genNode(n[0])

    else: internalError(m.g.config, n.info, "Unhandled node: " & $n & " (" & $n.kind & ")")

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
    # Strings, SourceExtensions, Source SourceContinued
  m.words.add(m.stringWords)
    # Names, # MemberNames
  m.words.add(m.nameWords)
    
  # Annotations (Decorates, MemberDescorates, GroupDecorates, GroupMemberDecorates)
  m.words.add(m.decorationWords)
    
  # (Lines valid from here)
    # Types, constants, Non-function Variables
  m.words.add(m.typeWords)
  m.words.add(m.constantWords)
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