import std/[macros, strutils]

{.experimental: "dotOperators".}

type
  ShaderStage* = enum
    Vertex
    TessellationControl
    TessellationEvaluation
    Geometry
    Fragment
    Compute
    #Kernel

  BuiltIn* = enum
    Position
    VertexId
    InstanceId
    PrimitiveId

  float16* {.importc: "float16".} = object

  Vector* {.importc: "Vector".} [T; size: static[int]] = object

  MatrixBase* {.importc: "Matrix".} [T; width: static[int]] = object
  Matrix* [T; height, width: static[int]] = MatrixBase[Vector[T, height], width]

  Vector2* = Vector[float32, 2]
  Vector3* = Vector[float32, 3]
  Vector4* = Vector[float32, 4]

  Matrix3x3* = Matrix[float32, 3, 3]
  Matrix4x3* = Matrix[float32, 4, 3]
  Matrix4x4* = Matrix[float32, 4, 4]

  DepthKind* {.pure} = enum
    HasDepth
    HasNoDepth
    Unspecified

  Dimension* {.pure.} = enum
    Image1D
    Image2D
    Image3D
    Cube
    Rect
    SubPass
    
  ArrayKind* {.pure.} = enum
    NonArray
    Array

  MultisampleKind* {.pure.} = enum
    SingleSample
    MultiSample

  SampleKind* {.pure.} = enum
    Unspecified
    Sampled
    Storage

  Format* {.pure.} = enum
    Unknown

  AccessQualifier* {.pure.} = enum
    ReadOnly
    WriteOnly
    ReadWrite

  Image* {.importc.} [T;
    dim: static[Dimension];
    depth: static[DepthKind];
    arrayed: static[ArrayKind];
    ms: static[MultisampleKind];
    sampled: static[SampleKind];
    format: static[Format];
    access: static[AccessQualifier]] = object

  Image2D* = Image[void, Dimension.Image2D,
    DepthKind.Unspecified, ArrayKind.NonArray,
    MultisampleKind.SingleSample, SampleKind.Sampled,
    Format.Unknown, AccessQualifier.ReadOnly]

  Sampler* {.importc.} = object

  SampledImage* {.importc.} [T: Image] = object

proc sampledImage*[T: Image](image: T; sampler: Sampler): SampledImage[T] {.importc: "OpSampledImage".}

proc sample*(image: SampledImage; coordinate: Vector): SampledImage.T.T {.importc: "OpImageSampleImplicitLod".}
proc sample*(image: SampledImage; coordinate: Vector; bias: SomeFloat): SampledImage.T.T {.importc: "OpImageSampleImplicitLod".}
proc sample*(image: SampledImage; coordinate: Vector; constOffset: distinct Vector): SampledImage.T.T {.importc: "OpImageSampleImplicitLod".}

func `[]`*(self: Vector; index: int): Vector.T {.importc.}
func `[]=`*(self: var Vector; index: int; value: Vector.T) {.importc.}

func `-`*[T: SomeFloat; size: static[int]](self: Vector[T, size]): Vector[T, size] {.importc: "OpFNegate".}
func `+`*[T: SomeFloat; size: static[int]](left, right: Vector[T, size]): Vector[T, size] {.importc: "OpFAdd".}
func `-`*[T: SomeFloat; size: static[int]](left, right: Vector[T, size]): Vector[T, size] {.importc: "OpFSub".}
func `*`*[T: SomeFloat; size: static[int]](left, right: Vector[T, size]): Vector[T, size] {.importc: "OpFMul".}
func `/`*[T: SomeFloat; size: static[int]](left, right: Vector[T, size]): Vector[T, size] {.importc: "OpFDiv".}

func construct*[T](): T {.varargs, importc: "OpCompositeConstruct".}

# proc toVector*[T; size: static[int]](args: varargs[T]): Vector[T, size] =
#   construct[Vector[T, size]](args)

func `*`*[T; size: static[int]](left: Vector[T, size]; right: T): Vector[T, size] {.importc: "OpVectorTimesScalar".}
func `*`*[T; height, width: static[int]](left: Matrix[T, height, width]; right: T): Matrix[T, height, width] {.importc: "OpMatrixTimesScalar".}
func `*`*[T; height, width: static[int]](left: Matrix[T, height, width]; right: Vector[T, width]): Vector[T, width] {.importc: "OpMatrixTimesVector".}
func `*`*[T; height, width: static[int]](left: Vector[T, height]; right: Matrix[T, height, width]): Vector[T, height] {.importc: "OpVectorTimesMatrix".}
func `*`*[T; height, width, size: static[int]](left: Matrix[T, height, size]; right: Matrix[T, size, width]): Matrix[T, height, width] {.importc: "OpMatrixTimesMatrix".}

template stage*(ShaderStage) {.pragma.}
template builtIn*(BuiltIn) {.pragma.}
template location*(int) {.pragma.}
template binding*(int) {.pragma.}
template descriptorSet*(int) {.pragma.}

template input*() {.pragma.}
template output*() {.pragma.}
template uniform*() {.pragma.}

# Vector swizzles
func getSwizzleIndex(c: char): int {.compileTime.} =
  case c:
    of 'x', 'r': return 0
    of 'y', 'g': return 1
    of 'z', 'b': return 2
    of 'w', 'a': return 3
    else: raiseAssert($c & " is not a valid vector swizzle")

# Assumes `[]` operator and the convertor from arrays on vectors
macro `.`*(self: Vector; swizzle: untyped): untyped =
  var
    cardinality = ($swizzle).len

  # For one-element swizzles, just return the scalar element
  # v.elements[0]
  if cardinality == 1:
    return nnkBracketExpr.newTree(self, newIntLitNode(($swizzle)[0].getSwizzleIndex))
    
  else:
    # The result is a call to `construct`
    result = nnkCall.newTree()

    # The first argument is the symbol of the appropriate generic instance of `construct`
    result.add quote do:
      construct[Vector[type(`self`).T, `cardinality`]]
    
    # Add the call arguments, one for each swizzle index
    # [temp[1], temp[0], ...]
    for c in $swizzle:
      result.add(nnkBracketExpr.newTree(self, newIntLitNode(c.getSwizzleIndex)))

# Assumes `[]` and `[]=` operators on vectors
macro `.=`*(self: var Vector; swizzle: untyped; value: untyped): untyped =
  var 
    cardinality = ($swizzle).len
  
  # For single elements, just do single assignment
  if cardinality == 1:
    return newAssignment(nnkBracketExpr.newTree(self, newIntLitNode(($swizzle)[0].getSwizzleIndex)), value)

  else:
    var temp = genSym()

    # Evaluate the right-hand side into a temporary variable
    # let temp = self
    result = newStmtList(
      nnkLetSection.newTree(
        newIdentDefs(temp, newEmptyNode(), value)))

    # For each swizzle index, add an assignment of the corresponding element
    # self[swizzleIndex(c)] = temp[i]
    for i, c in $swizzle:
      result.add(
        newAssignment(
          nnkBracketExpr.newTree(self, newIntLitNode(c.getSwizzleIndex)),
          nnkBracketExpr.newTree(temp, newIntLitNode(i))))

# func getMatrixSwizzles(swizzle: string): seq[(int, int)] {.compileTime.} =

#   #assert(match(swizzle, re"(m\d\d)*"), "'" & swizzle & "' is not a valid matrix swizzling")
#   assert(swizzle.len mod 3 == 0, "'" & swizzle & "' is not a valid matrix swizzling pattern")
#   result = newSeqOfCap[(int, int)](swizzle.len div 3)

#   for i in countup(0, swizzle.len - 1, 3):
#     assert(swizzle[i] == 'm' and swizzle[i + 1].isDigit and swizzle[i + 2].isDigit, "'" & swizzle & "' is not a valid matrix swizzling pattern")
#     result.add((parseInt($swizzle[i + 1]), parseInt($swizzle[i + 2])))

# macro `.`*(self: Matrix; swizzle: untyped): untyped =
#   var
#     indices = getMatrixSwizzles($swizzle)
#     cardinality = indices.len

#   # For one-element swizzles, just return the scalar element
#   # v.elements[0]
#   if cardinality == 1:
#     let (row, column) = indices[0]
#     return nnkBracketExpr.newTree(self, newIntLitNode(row), newIntLitNode(column))
    
#   else:
#     var
#       values = newNimNode(nnkBracket)
#       temp = genSym()

#     # Make an array of values, one for each swizzle index
#     # [temp[1, 2], temp[0, 3], ...]
#     for index in indices:
#       let (row, column) = index
#       values.add(nnkBracketExpr.newTree(temp, newIntLitNode(row), newIntLitNode(column)))

#     return quote do:
#       let `temp` = `self`
#       toVector[`temp`.T, `cardinality`](`values`)

# # Assumes `[]` and `[]=` operators on matrices
# macro `.=`*(self: var Matrix; swizzle: untyped; value: untyped): untyped =
#   var
#     indices = getMatrixSwizzles($swizzle)
#     cardinality = indices.len
  
#   # For single elements, just do single assignment
#   if cardinality == 1:
#     let (row, column) = indices[0]
#     return newAssignment(nnkBracketExpr.newTree(self, newIntLitNode(row), newIntLitNode(column)), value)

#   else:
#     var temp = genSym()

#     # Evaluate the right-hand side into a temporary variable
#     # let temp = self
#     result = newStmtList(
#       nnkLetSection.newTree(
#         newIdentDefs(temp, newEmptyNode(), value)))

#     # For each swizzle index, add an assignment of the corresponding element
#     # self[swizzleIndex(c)] = temp[i]
#     for i, pos in indices:
#       let (row, column) = pos
#       result.add(
#         newAssignment(
#           nnkBracketExpr.newTree(self, newIntLitNode(row), newIntLitNode(column)),
#           nnkBracketExpr.newTree(temp, newIntLitNode(i))))
