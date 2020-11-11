type Vertex = {
  x: number
  y: number
  v: string
  edges: EdgeInfo[]
}

type EdgeInfo = {
  a: string
  v: string
  s: string
}

type TilingGraph = Record<string, Vertex>

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

type ConvexPolytope = {
  ass: AffineSubspace
  cs: Equation[]
}
