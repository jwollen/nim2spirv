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
