import React, { FC, useMemo } from 'react'
import styles from '../styles/graph.module.css'
import Edge from './Edge'
import Vertex from './Vertex'
import Angle from './Angle'

type GraphProps = {
  graph: TilingGraph
  lengths: number[]
  angles: number[]
}

const makeViewBox = (graph: TilingGraphWithLocations) => {
  const vertices = Object.values(graph)
  const xs = vertices.map(({ x }) => x)
  const ys = vertices.map(({ y }) => y)
  const minX = Math.min(...xs)
  const maxX = Math.max(...xs)
  const minY = Math.min(...ys)
  const maxY = Math.max(...ys)
  const width = maxX - minX
  const height = maxY - minY
  const margin = 0.2
  const mh = width * margin
  const mv = height * margin
  return `${minX - mh} ${-maxY - mv} ${width + 2 * mh} ${height + 2 * mv}`
}

const angleList = (angles: number[], edges: EdgeInfo[]): number[] => {
  const result = []
  edges.forEach((edge, i) => {
    const previousAngle = i === 0 ? 0 : result[i - 1]
    const angle = getAngle(angles, edge.a)
    result.push(previousAngle + angle)
  })
  return result
}

const edgeToNeighbourWithLocation = (
  gl: TilingGraphWithLocations,
  edges: EdgeInfo[]
): EdgeInfo | undefined => {
  const vWithL = Object.keys(gl)
  for (const edge of edges) {
    if (vWithL.includes(edge.v)) {
      return edge
    }
  }
}

const nextVertexToAddLocation = (
  graph: Record<string, EdgeInfo[]>,
  gl: TilingGraphWithLocations,
  verticesWithoutLocation: string[]
): string | undefined => {
  for (const v of verticesWithoutLocation) {
    if (edgeToNeighbourWithLocation(gl, graph[v])) {
      return v
    }
  }
}

const planarize = (
  graph: Record<string, EdgeInfo[]>,
  angles: number[],
  lengths: number[],
  gl: TilingGraphWithLocations
): TilingGraphWithLocations => {
  const verticesWithLocation = Object.keys(gl)
  const verticesWithoutLocation = Object.keys(graph).filter(
    (v) => !verticesWithLocation.includes(v)
  )
  if (verticesWithoutLocation.length === 0) return gl // We are done.
  const v = nextVertexToAddLocation(graph, gl, verticesWithoutLocation)
  if (!v) {
    return planarize(graph, angles, lengths, {
      ...gl,
      '1': { x: 0, y: 0, edges: graph['1'] } // We place first vertex at origo.
    })
  }
  const { v: w1, l } = edgeToNeighbourWithLocation(gl, graph[v]) // Will always succeed.
  const { x: w1X, y: w1Y, edges: w1Edges } = gl[w1]
  const e1 = edgeToNeighbourWithLocation(gl, w1Edges)
  if (!e1) {
    return planarize(graph, angles, lengths, {
      ...gl,
      '2': { x: getLength(lengths, 'a'), y: 0, edges: graph['2'] } // We place second vertex to the right of first vertex.
    })
  }
  const { v: w2 } = e1
  const w2Info = gl[w2]
  const angleW1W2 = Math.atan2(w2Info.y - w1Y, w2Info.x - w1X) / Math.PI
  const anglesAroundW1 = angleList(angles, w1Edges)
  const calcAngleV = anglesAroundW1[w1Edges.findIndex((edge) => edge.v === v)]
  const calcAngleW2 = anglesAroundW1[w1Edges.findIndex((edge) => edge.v === w2)]
  const angleV = angleW1W2 + calcAngleV - calcAngleW2
  const length = getLength(lengths, l)
  const x = w1X + length * Math.cos(angleV * Math.PI)
  const y = w1Y + length * Math.sin(angleV * Math.PI)
  return planarize(graph, angles, lengths, {
    ...gl,
    [v]: { x, y, edges: graph[v] }
  })
}

const getLength = (lengths: number[], length: Length): number => {
  switch (length) {
    case 'a':
      return lengths[0]
    case 'b':
      return lengths[1]
    case 'c':
      return lengths[2]
    case 'd':
      return lengths[3]
    case 'e':
      return lengths[4]
    default:
      throw Error('Unknown length.')
  }
}

const getAngle = (angles: number[], angle: Angle): number => {
  switch (angle) {
    case 'A':
      return angles[0]
    case 'B':
      return angles[1]
    case 'C':
      return angles[2]
    case 'D':
      return angles[3]
    case 'E':
      return angles[4]
    case '?':
      return 0
    case '0':
      return 0
    case 'Pi':
      return 1
  }
}

const edgesFromCorner = ({ ea, l1, v1, ia, l2, v2 }: Corner): EdgeInfo[] => [
  { a: ea, l: l1, v: v1 },
  { a: ia, l: l2, v: v2 }
]

const getIntermediateGraph = (graph: TilingGraph): Record<string, EdgeInfo[]> =>
  Object.fromEntries(
    Object.entries(graph).map(([k, v]) => [
      k,
      [].concat(...v.map(edgesFromCorner))
    ])
  )

const Graph: FC<GraphProps> = ({ graph: g, angles, lengths }) => {
  const graph = useMemo(
    () => planarize(getIntermediateGraph(g), angles, lengths, {}),
    [g, angles, lengths]
  )

  const viewBox = makeViewBox(graph)
  const entries = Object.entries(graph)

  return (
    <svg
      viewBox={viewBox}
      xmlns='http://www.w3.org/2000/svg'
      className={styles.graph}
    >
      {entries.map(([v, { x, y, edges }]) => {
        const numEdges = edges.length
        return (
          <g key={`angles-${v}`}>
            {edges.map(({ a, v: w2 }, index) => {
              if (a == '?' || a == '0') return null
              const prevIndex = (numEdges + index - 1) % numEdges
              const w1 = edges[prevIndex].v
              return (
                <Angle
                  key={`angle-${index}`}
                  a={graph[w1]}
                  b={{ x, y }}
                  c={graph[w2]}
                  angle={a}
                />
              )
            })}
          </g>
        )
      })}
      {entries.map(([v1, { edges }]) => (
        <g key={`edges-${v1}`}>
          {edges.map(({ v: v2 }, index) => (
            <Edge
              key={`${v1}-edge-to-${v2}-with-index-${index}`}
              v1={graph[v1]}
              v2={graph[v2]}
            />
          ))}
        </g>
      ))}
      {entries.map(([v, { x, y }]) => (
        <Vertex key={`vertex-${v}`} x={x} y={y} v={v} />
      ))}
    </svg>
  )
}

export default Graph
