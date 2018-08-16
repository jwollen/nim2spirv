import
  strutils, hashes, std / sha1, os, tables,
  times, math, intsets, options as opt

import ../compiler/[
  ast, astalgo, platform, magicsys, extccomp, trees, bitsets,
  nversion, nimsets, msgs, idents, types, options, ropes,
  passes, ccgutils, wordrecg, renderer, rodutils,
  cgmeth, lowerings, sighashes, modulegraphs]

import
  spirvTypes, glslTypes, openclTypes

type
  SpirvGenObj = object of TPassContext
    module: PSym
    config: ConfigRef

  SpirvGen = ref SpirvGenObj

proc newModule(module: PSym; config: ConfigRef): SpirvGen =
  new(result)
  result.module = module
  result.config = config

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
  result = n
  if b == nil: return
  var m = SpirvGen(b)
  if passes.skipCodegen(m.config, n): return
  
  m.genNode(n)

proc myClose(graph: ModuleGraph; b: PPassContext, n: PNode): PNode =
  result = n
  if b == nil: return
  var m = SpirvGen(b)
  if passes.skipCodegen(m.config, n): return

  if n != nil:
    m.genNode(n)

proc myOpen(graph: ModuleGraph; module: PSym): PPassContext =
  return newModule(module, graph.config)

const spirvSemanticPass* = makePass(myOpen, myProcess, myClose)