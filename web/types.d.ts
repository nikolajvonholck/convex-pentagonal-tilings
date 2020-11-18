type Length = 'a' | 'b' | 'c' | 'd' | 'e'
type InteriorAngle = 'A' | 'B' | 'C' | 'D' | 'E'
type ExteriorAngle = '?' | '0' | 'Pi'
type Angle = InteriorAngle | ExteriorAngle

type Vertex = {
  edges: EdgeInfo[]
}

type VertexWithLocation = {
  x: number
  y: number
  edges: EdgeInfo[]
}

type Corner = {
  ea: ExteriorAngle
  l1: Length
  v1: string
  ia: InteriorAngle
  l2: Length
  v2: string
}

type EdgeInfo = {
  a: Angle
  l: Length
  v: string
}

type TilingGraph = Record<string, Corner[]>

type TilingGraphWithLocations = Record<string, VertexWithLocation>

type Rational = {
  p: number
  q: number
}

type Vector<T> = T[]

type AffineSubspace = {
  p: Vector<Rational>
  bs: Vector<Rational>[]
  hps: Equation[]
}

type Equation = {
  t: Vector<Rational>
  c: Rational
}

type ScaledEquation = {
  t: Vector<number>
  c: number
}

type LinearProgram = {
  ass: AffineSubspace
  cs: Equation[]
}
