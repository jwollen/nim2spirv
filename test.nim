
template vectorType(typedesc) {.pragma.}

type
  ShaderStage* = enum
    Vertex
    Fragment
    TessellationControl
    TessellationEvaluation
    Pixel
    Compute
    #Kernel

  Builtin* = enum
    Position
    VertexId
    InstanceId
    PrimitiveId

  Vector*[T; size: static[int]] = object
    elements*: array[2, float]

  Vector2* = Vector[float32, 2]

  Vector4* = Vector[float32, 4]   

template stage*(ShaderStage) {.pragma.}
template builtin*(Builtin) {.pragma.}
template stage*(ShaderStage) {.pragma.}
template location*(int) {.pragma.}
template binding*(int) {.pragma.}
template descriptorSet*(int) {.pragma.}

template input*() {.pragma.}
template output*() {.pragma.}
template uniform*() {.pragma.}

var
  texCoord {.input, location: 0.}: Vector2
  color {.output, location: 0.}: Vector4

proc fragmentShader() {.stage: Fragment.} =
  discard