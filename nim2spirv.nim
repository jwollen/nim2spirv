import strutils, os, times, parseopt

import ../compiler/[
  commands, condsyms, idents, lexer, llstream, modulegraphs,
  modules, msgs, nimconf, options, passes, sem, lineinfos,
  platform, rod, passaux, idgen, extccomp, scriptconfig,
  cmdlinehelper, pathutils]

import src/[spirvgen, spirvSemantics]

proc semanticPasses(graph: ModuleGraph) =
  registerPass(graph, verbosePass)
  registerPass(graph, semPass)

proc commandCompileToSpirv(graph: ModuleGraph) =
  let conf = graph.config

  if conf.outDir.isEmpty:
    conf.outDir = conf.projectPath
  if conf.outFile.isEmpty:
    conf.outFile = RelativeFile(conf.projectName & ".spv")

  setTarget(conf.target, osStandalone, cpuI386)
  conf.selectedGC = gcNone
  defineSymbol(conf.symbols, "nogc")

  defineSymbol(conf.symbols, "spirv")
  semanticPasses(graph)
  registerPass(graph, spirvSemanticPass)
  registerPass(graph, spirvGenPass)
  compileProject(graph)

proc mainCommand*(graph: ModuleGraph) =
  let conf = graph.config
  let cache = graph.cache

  setupModuleCache(graph)
  # In "nim serve" scenario, each command must reset the registered passes
  clearPasses(graph)
  conf.lastCmdTime = epochTime()
  conf.searchPaths.add(conf.libpath)
  setId(100)

  case conf.command.normalize
  # Take over the default compile command
  of "spirv", "compiletospirv":
    #gCmd = cmdCompileToSpirv
    commandCompileToSpirv(graph)
  else:
    rawMessage(conf, errGenerated, "invalid command: " & conf.command)

  if conf.errorCounter == 0 and
     conf.cmd notin {cmdInterpret, cmdRun, cmdDump}:
    when declared(system.getMaxMem):
      let usedMem = formatSize(getMaxMem()) & " peakmem"
    else:
      let usedMem = formatSize(getTotalMem())
    rawMessage(conf, hintSuccessX, [$conf.linesCompiled,
               formatFloat(epochTime() - conf.lastCmdTime, ffDecimal, 3),
               usedMem,
               if isDefined(conf, "release"): "Release Build"
               else: "Debug Build"])

  resetAttributes(conf)

proc processCmdLine(pass: TCmdLinePass, cmd: string; config: ConfigRef) =
  var p = parseopt.initOptParser(cmd)
  var argsCount = 0
  while true:
    parseopt.next(p)
    case p.kind
    of cmdEnd: break
    of cmdLongoption, cmdShortOption:
      if p.key == " ":
        p.key = "-"
        if processArgument(pass, p, argsCount, config): break
      else:
        processSwitch(pass, p, config)
    of cmdArgument:
      if processArgument(pass, p, argsCount, config): break
  if pass == passCmd2:
    if optRun notin config.globalOptions and config.arguments.len > 0 and config.command.normalize != "run":
      rawMessage(config, errGenerated, errArgsNeedRunOption)

proc handleCmdLine(cache: IdentCache; conf: ConfigRef) =
  let self = NimProg(
    supportsStdinFile: true,
    processCmdLine: processCmdLine,
    mainCommand: mainCommand
  )
  self.initDefinesProg(conf, "nim_compiler")
  if paramCount() == 0:
    writeCommandLineUsage(conf)
    return

  self.processCmdLineAndProjectPath(conf)
  if not self.loadConfigsAndRunMainCommand(cache, conf): return
  # if optHints in conf.options and hintGCStats in conf.notes: echo(GC_getStatistics())
  # #echo(GC_getStatistics())
  # if conf.errorCounter != 0: return
  # when hasTinyCBackend:
  #   if conf.cmd == cmdRun:
  #     tccgen.run(conf.arguments)
  # if optRun in conf.globalOptions:
  #   if conf.cmd == cmdCompileToJS:
  #     var ex: string
  #     if not conf.outFile.isEmpty:
  #       ex = conf.outFile.prependCurDir.quoteShell
  #     else:
  #       ex = quoteShell(
  #         completeCFilePath(conf, changeFileExt(conf.projectFull, "js").prependCurDir))
  #     execExternalProgram(conf, findNodeJs() & " " & ex & ' ' & conf.arguments)
  #   else:
  #     var binPath: AbsoluteFile
  #     if not conf.outFile.isEmpty:
  #       # If the user specified an outFile path, use that directly.
  #       binPath = conf.outFile.prependCurDir
  #     else:
  #       # Figure out ourselves a valid binary name.
  #       binPath = changeFileExt(conf.projectFull, ExeExt).prependCurDir
  #     var ex = quoteShell(binPath)
  #     execExternalProgram(conf, ex & ' ' & conf.arguments)

let conf = newConfigRef()
handleCmdLine(newIdentCache(), conf)
when declared(GC_setMaxPause):
  echo GC_getStatistics()
msgQuit(int8(conf.errorCounter > 0))