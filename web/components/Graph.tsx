import React, { FC } from 'react'
import styles from '../styles/graph.module.css'
import Edge from './Edge'
import Vertex from './Vertex'
import Angle from './Angle'

type GraphProps = {
  graph: TilingGraph
}

const makeViewBox = (graph: TilingGraph) => {
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

const Graph: FC<GraphProps> = ({ graph }) => {
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
