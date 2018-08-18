
type
  ShaderStage* = enum
    Vertex
    Fragment
    TessellationControl
    TessellationEvaluation
    Geometry
    Compute
    #Kernel

  Builtin* = enum
    Position
    VertexId
    InstanceId
    PrimitiveId

  Vector* {.importc.} [T; size: static[int]] = object

  MatrixBase* {.importc.} [T; width: static[int]] = object
  Matrix* [T; height, width: static[int]] = MatrixBase[Vector[T, height], width]

  Vector2* = Vector[float32, 2]
  Vector3* = Vector[float32, 3]
  Vector4* = Vector[float32, 4]

  Matrix3x3 = Matrix[float32, 3, 3]
  Matrix4x3 = Matrix[float32, 4, 3]
  Matrix4x4 = Matrix[float32, 4, 4]

func `[]`*(self: Vector; index: int): Vector.T {.importc.}
func `[]=`*(self: var Vector; index: int; value: Vector.T) {.importc.}

template stage*(ShaderStage) {.pragma.}
template builtin*(Builtin) {.pragma.}
template location*(int) {.pragma.}
template binding*(int) {.pragma.}
template descriptorSet*(int) {.pragma.}

template input*() {.pragma.}
template output*() {.pragma.}
template uniform*() {.pragma.}

type
  Data = object
    worldViewProjection: Matrix4x4

var
  data {.uniform, descriptorSet: 0, binding: 0.}: Data
  texCoord {.input, location: 0.}: Vector2
  color {.output, location: 0.}: Vector4

proc vertexShader() {.stage: Vertex.} =
  discard

proc main() {.stage: Fragment.} =
  color[0] = texCoord[0]
  color[1] = texCoord[1]
  color[2] = 0.0'f32
  color[3] = 1.0'f32