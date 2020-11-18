import React, { FC } from 'react'
import styles from '../styles/graph.module.css'

type EdgeProps = {
  v1: VertexWithLocation
  v2: VertexWithLocation
}

const Edge: FC<EdgeProps> = ({
  v1: { x: x1, y: y1 },
  v2: { x: x2, y: y2 }
}) => (
  <line
    x1={x1}
    y1={-y1}
    x2={x2}
    y2={-y2}
    vectorEffect='non-scaling-stroke'
    className={styles.edge}
  />
)

export default Edge
