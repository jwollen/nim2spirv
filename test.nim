
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
#    elements*: array[size, T]

  Vector2* = Vector[float32, 2]
  Vector3* = Vector[float32, 3]
  Vector4* = Vector[float32, 4]

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

var
  texCoord {.input, location: 0.}: Vector2
  color {.output, location: 0.}: Vector4

# proc vertexShader() {.stage: Vertex.} =
#   discard

proc main() {.stage: Fragment.} =
  color[0] = texCoord[0]
  color[1] = texCoord[1]
  color[2] = 0.0'f32
  color[3] = 1.0'f32