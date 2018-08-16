import strutils, os, times, parseopt

import ../compiler/[
  commands, condsyms, idents, lexer, llstream, modulegraphs,
  modules, msgs, nimconf, options, passes, sem, lineinfos,
  platform, rod, passaux, idgen, extccomp, scriptconfig]

import nim2spirv/[spirvgen, spirvSemantics]

proc semanticPasses(graph: ModuleGraph) =
  registerPass(graph, verbosePass)
  registerPass(graph, semPass)

proc commandCompileToSpirv(graph: ModuleGraph) =
  setTarget(graph.config.target, osStandalone, cpuI386)
  defineSymbol(graph.config.symbols, "spirv")
  registerPass(graph, spirvSemanticPass)
  semanticPasses(graph)
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
  condsyms.initDefines(conf.symbols)
  if paramCount() == 0:
    writeCommandLineUsage(conf, conf.helpWritten)
  else:
    # Process command line arguments:
    processCmdLine(passCmd1, "", conf)
    if conf.projectName == "-":
      conf.projectName = "stdinfile"
      conf.projectFull = "stdinfile"
      conf.projectPath = canonicalizePath(conf, getCurrentDir())
      conf.projectIsStdin = true
    elif conf.projectName != "":
      try:
        conf.projectFull = canonicalizePath(conf, conf.projectName)
      except OSError:
        conf.projectFull = conf.projectName
      let p = splitFile(conf.projectFull)
      let dir = if p.dir.len > 0: p.dir else: getCurrentDir()
      conf.projectPath = canonicalizePath(conf, dir)
      conf.projectName = p.name
    else:
      conf.projectPath = canonicalizePath(conf, getCurrentDir())
    loadConfigs(DefaultConfig, cache, conf) # load all config files
    let scriptFile = conf.projectFull.changeFileExt("nims")
    if fileExists(scriptFile):
      runNimScript(cache, scriptFile, freshDefines=false, conf)
      # 'nim foo.nims' means to just run the NimScript file and do nothing more:
      if scriptFile == conf.projectFull: return
    elif fileExists(conf.projectPath / "config.nims"):
      # directory wide NimScript file
      runNimScript(cache, conf.projectPath / "config.nims", freshDefines=false, conf)
    # now process command line arguments again, because some options in the
    # command line can overwite the config file's settings
    extccomp.initVars(conf)
    processCmdLine(passCmd2, "", conf)
    if conf.command == "":
      rawMessage(conf, errGenerated, "command missing")
    mainCommand(newModuleGraph(cache, conf))
    if optHints in conf.options and hintGCStats in conf.notes: echo(GC_getStatistics())
    #echo(GC_getStatistics())

let conf = newConfigRef()
handleCmdLine(newIdentCache(), conf)
when declared(GC_setMaxPause):
  echo GC_getStatistics()
msgQuit(int8(conf.errorCounter > 0))