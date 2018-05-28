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
    module: PSym

  SpirvGen = ref SpirvGenObj

proc newModule(module: PSym): SpirvGen =
  new(result)
  result.module = module

var level = 0

proc genNode(g: SpirvGen; n: PNode) =
  if sfMainModule notin g.module.flags:
    return

  var text = spaces(level * 2) & $n.kind

  case n.kind:
    of nkSym: text &= ": " & n.sym.name.s
    of nkIdent: text &= ": " & n.ident.s
    of nkStmtList: text &= ": " & $n.sonsLen
    else: discard
    
  echo text

  inc level

  case n.kind:
    of nkCharLit..nkUInt64Lit: discard
    of nkFloatLit..nkFloat128Lit: discard
    of nkStrLit..nkTripleStrLit: discard
    of nkSym: discard
    of nkIdent: discard
    else:
      for child in n.sons:
        g.genNode(child)

  dec level

proc myProcess(b: PPassContext, n: PNode): PNode =
  if passes.skipCodegen(n): return n
  result = n

  var m = SpirvGen(b)
  if m.module == nil: internalError(n.info, "myProcess")
  
  m.genNode(n)

proc myClose(graph: ModuleGraph; b: PPassContext, n: PNode): PNode =
  
  if passes.skipCodegen(n): return n
  result = n

  var m = SpirvGen(b)
  m.genNode(n)

proc myOpenCached(graph: ModuleGraph; s: PSym, rd: PRodReader): PPassContext =
  internalError("Symbol files are not supported by the SPIR-V backend")
  result = nil

proc myOpen(graph: ModuleGraph; module: PSym; cache: IdentCache): PPassContext =
  return newModule(module)

const spirvSemanticPass* = makePass(myOpen, myOpenCached, myProcess, myClose)