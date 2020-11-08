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
