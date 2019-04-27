import intsets, tables

import ../compiler/[ast, modulegraphs, dfa]

proc dfa*(g: ModuleGraph; owner: PSym; n: PNode) =

  var code = constructCfg(owner, n)

  var u = newSeq[IntSet](code.len) # usages
  var d = newSeq[IntSet](code.len) # defs
  var c = newSeq[IntSet](code.len) # consumed
  var backrefs = initTable[int, int]()
  for i in 0..<code.len:
    u[i] = initIntSet()
    d[i] = initIntSet()
    c[i] = initIntSet()
    case code[i].kind
    of use: u[i].incl(code[i].sym.id)
    of def: d[i].incl(code[i].sym.id)
    of fork, goto:
      let d = i+code[i].dest
      backrefs.add(d, i)

  var w = @[0]
  var maxIters = 50
  var someChange = true
  var takenGotos = initIntSet()
  var consuming = -1
  while w.len > 0 and maxIters > 0: # and someChange:
    dec maxIters
    var pc = w.pop() # w[^1]
    var prevPc = -1
    # this simulates a single linear control flow execution:
    while pc < code.len:
      if prevPc >= 0:
        someChange = false
        # merge step and test for changes (we compute the fixpoints here):
        # 'u' needs to be the union of prevPc, pc
        # 'd' needs to be the intersection of 'pc'
        for id in u[prevPc]:
          if not u[pc].containsOrIncl(id):
            someChange = true
        # in (a; b) if ``a`` sets ``v`` so does ``b``. The intersection
        # is only interesting on merge points:
        for id in d[prevPc]:
          if not d[pc].containsOrIncl(id):
            someChange = true
        # if this is a merge point, we take the intersection of the 'd' sets:
        if backrefs.hasKey(pc):
          var intersect = initIntSet()
          assign(intersect, d[pc])
          var first = true
          for prevPc in backrefs.allValues(pc):
            for def in d[pc]:
              if def notin d[prevPc]:
                excl(intersect, def)
                someChange = true
                when defined(debugDfa):
                  echo "Excluding ", pc, " prev ", prevPc
          assign d[pc], intersect
      if consuming >= 0:
        if not c[pc].containsOrIncl(consuming):
          someChange = true
        consuming = -1

      # our interpretation ![I!]:
      prevPc = pc
      case code[pc].kind
      of goto:
        # we must leave endless loops eventually:
        if not takenGotos.containsOrIncl(pc) or someChange:
          pc = pc + code[pc].dest
        else:
          inc pc
      of fork:
        # we follow the next instruction but push the dest onto our "work" stack:
        #if someChange:
        w.add pc + code[pc].dest
        inc pc
      of use:
        #if not d[prevPc].missingOrExcl():
        # someChange = true
        consuming = code[pc].sym.id
        inc pc
      of def:
        if not d[pc].containsOrIncl(code[pc].sym.id):
          someChange = true
        inc pc

  when true: #defined(useDfa) and defined(debugDfa):
    var all = initIntSet()
    for i in 0..<code.len:
      if code[i].kind in { def, use }:
        if not all.containsOrIncl(code[i].sym.id):
          echo code[i].sym.name.s & ": " & $code[i].sym.id

    for i in 0..<code.len:
      #echo "PC ", i, ": defs: ", d[i], "; uses ", u[i], "; consumes ", c[i]
      echo "PC ", i, " : defs: ", d[i], "; uses ", u[i], "; consumes ", c[i]
