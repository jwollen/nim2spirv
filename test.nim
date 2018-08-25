import shaders

type
  Data = object
    worldViewProjection: Matrix4x4

var
  data {.uniform, descriptorSet: 0, binding: 0.}: Data

  position {.input, location: 0.}: Vector4
  normal {.input, location: 1.}: Vector3
  texCoordVIn {.input, location: 2.}: Vector2
  texCoordVOut {.output, location: 0.}: Vector2
  position0 {.output, builtIn: Position.}: Vector4

  texCoord {.input, location: 0.}: Vector2
  color {.output, location: 0.}: Vector4

proc vsMain() {.stage: Vertex.} =
  position0 = data.worldViewProjection * construct[Vector4](position[0], position[1], position[2], 1.0'f32)
  position0[1] = -position0[1]
  texCoordVOut = texCoordVIn

proc fsMain() {.stage: Fragment.} =
  color[0] = texCoord[0]
  color[1] = texCoord[1]
  color[2] = 0.0'f32
  color[3] = 1.0'f32
