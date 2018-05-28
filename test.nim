
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

  Vector2* = object
    elements*: array[2, float]   

  Vector4* = object
    elements*: array[4, float]   

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