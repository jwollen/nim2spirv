import strutils, os, times

import ../compiler/[
  commands, condsyms, idents, lexer, llstream, modulegraphs,
  modules, msgs, nimconf, options, passes, sem,
  service, platform, rod, passaux, idgen]

import spirvgen

proc semanticPasses =
  registerPass verbosePass
  registerPass semPass

proc commandCompileToSpirv(graph: ModuleGraph; cache: IdentCache) =
  setTarget(osStandalone, cpuI386)
  defineSymbol("spirv")
  semanticPasses()
  registerPass(spirvGenPass)
  compileProject(graph, cache)

proc mainCommand*(graph: ModuleGraph; cache: IdentCache) =
  setupModuleCache()
  # In "nim serve" scenario, each command must reset the registered passes
  clearPasses()
  gLastCmdTime = epochTime()
  searchPaths.add(options.libpath)
  when false: # gProjectFull.len != 0:
    # current path is always looked first for modules
    prependStr(searchPaths, gProjectPath)
  setId(100)
  case command.normalize

  # Take over the default compile command
  of "spirv", "compiletospirv":
    #gCmd = cmdCompileToSpirv
    commandCompileToSpirv(graph, cache)

  else:
    rawMessage(errInvalidCommandX, command)

  if msgs.gErrorCounter == 0 and
     gCmd notin {cmdInterpret, cmdRun, cmdDump}:
    rawMessage(hintSuccess, [])

var configRef = newConfigRef()

proc mainCommand*() = mainCommand(newModuleGraph(configRef), newIdentCache())

proc handleCmdLine(config: ConfigRef) =
  # For now, we reuse the nim command line options parser, mainly because
  # the options are used all over the compiler, but also because we want to
  # act as a drop-in replacement (for now)
  # Most of this is taken from the main nim command
  if os.paramCount() == 0:
    echo """
you can: nlvm c <filename> (see standard nim compiler for options)
magic options:
  --nlvm.target=wasm32 cross-compile to WebAssembly
"""
  else:
    # Main nim compiler has some reaons for two-pass parsing
    service.processCmdLine(passCmd1, "", config)

    # Use project name like main nim compiler
    # TODO upstream to common location...
    if options.gProjectName == "-":
      options.gProjectName = "stdinfile"
      options.gProjectFull = "stdinfile"
      options.gProjectPath = os.getCurrentDir()
      options.gProjectIsStdin = true
    elif options.gProjectName != "":
      try:
        options.gProjectFull = canonicalizePath(options.gProjectName)
      except OSError:
        options.gProjectFull = options.gProjectName
      let p = splitFile(options.gProjectFull)
      options.gProjectPath = p.dir
      options.gProjectName = p.name
    else:
      gProjectPath = os.getCurrentDir()

    nimconf.loadConfigs(DefaultConfig)
    service.processCmdLine(passCmd2, "", config)

    #gSelectedGC = gcMarkAndSweep
    #defineSymbol("gcmarkandsweep")

    # default signal handler does memory allocations and all kinds of
    # disallowed-in-signal-handler-stuff
    defineSymbol("noSignalHandler")

    # lib/pure/bitops.num
    defineSymbol("noIntrinsicsBitOpts")

    mainCommand()

var tmp = getAppDir()
options.gPrefixDir = tmp / "Nim"
condsyms.initDefines()
handleCmdLine(configRef)