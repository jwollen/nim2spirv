# nim2spirv

A backend for the Nim compiler and standalone executable to compile Nim into SPIR-V.

> Note: This is an early proof-of-concept and work-in-progress!

## Goals

- Make Nim an awesome shader language, closely mapping to SPIR-V concepts.
- Share code between host program and shader modules by compiling `.nim`-files with both `nim` and `nim2spirv`.
- Define a high-level shader DSL through the power meta-programming that
  - trivializes shader permutation management
  - abstracts API differences
  - exposes shader meta-data to the host program and authoring tools, by directly importing shader modules

## Usage

This needs to built against a patched branch of the nim compiler:
https://github.com/fragcolor-xyz/Nim/tree/nim2spirv

Use similarly to `nim`:

```
nim2spirv spirv test.nim
spirv-dis test.spv
spirv-opt -o test-opt.spv test.spv
```

Consume direcly with Vulkan or cross-compile with [SPIRV-Cross](https://github.com/KhronosGroup/SPIRV-Cross).

## Progress

Currently the following compiles and produces a valid SPIR-V module with a Vulkan vertex and fragment entry-point:

```nimrod
import shaders

type
  Data = object
    worldViewProjection {.rowMajor.}: Matrix4x4

var
  data {.uniform, descriptorSet: 0, binding: 0.}: Data

  position {.input, location: 0.}: Vector4
  normal {.input, location: 1.}: Vector3
  texCoordVIn {.input, location: 2.}: Vector2
  texCoordVOut {.output, location: 0.}: Vector2
  clipSpacePosition {.output, builtIn: Position.}: Vector4

  texCoord {.input, location: 0.}: Vector2
  color {.output, location: 0.}: Vector4

proc vsMain() {.stage: Vertex.} =
  clipSpacePosition = construct[Vector4](position.xyz, 1.0'f32) * data.worldViewProjection
  clipSpacePosition.y = -clipSpacePosition.y
  texCoordVOut = texCoordVIn

proc fsMain() {.stage: Fragment.} =
  color = construct[Vector4](texCoord.xy, 0.0'f32, 1.0'f32)

```