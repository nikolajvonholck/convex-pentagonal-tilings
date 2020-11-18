import React, { FC } from 'react'
import styles from '../styles/graph.module.css'

type VertexProps = {
  x: number
  y: number
  v: string
}

const Vertex: FC<VertexProps> = ({ x, y, v }) => (
  <g>
    <circle cx={x} cy={-y} r={0.018} className={styles.vertex} />
    <text
      x={x}
      y={-y}
      textAnchor='middle'
      alignmentBaseline='central'
      className={styles.vertexLabel}
    >
      {v}
    </text>
  </g>
)

export default Vertex
