# Package
version     = "0.1.0"
author      = "Jörg Wollenschäger"
description = "SPIRV-V backend for Nim"
license     = "MIT"

bin = @["nim2spirv"]
skipExt = @["nim"]

# Deps
requires "nim >= 0.18.1"
