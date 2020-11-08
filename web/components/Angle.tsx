import React, { FC } from 'react'
import styles from '../styles/graph.module.css'

type Point = {
  x: number
  y: number
}

type AngleProps = {
  a: Point
  b: Point
  c: Point
  angle: string
}

const colors = [
  'rgb(236, 31, 38)', // Red
  'rgb(121, 193, 68)', // Green
  'rgb(0, 125, 199)', // Blue
  'rgb(244, 112, 37)', // Orange
  'rgb(252, 223, 7)', // Yellow
  'rgb(138, 40, 143)' // Violet
]

const angleColor = (angle: string) => {
  switch (angle) {
    case '1':
      return colors[0]
    case '2':
      return colors[1]
    case '3':
      return colors[2]
    case '4':
      return colors[3]
    case '5':
      return colors[4]
    case 'pi':
      return colors[5]
    default:
      return 'black'
  }
}

const Angle: FC<AngleProps> = ({
  a: { x: x1, y: y1 },
  b: { x, y },
  c: { x: x2, y: y2 },
  angle
}) => {
  const begin = Math.atan2(y1 - y, x1 - x)
  const end = Math.atan2(y2 - y, x2 - x)
  const theta = (end - begin + 2 * Math.PI) % (2 * Math.PI)
  const [fA, fS] = [theta > Math.PI ? 1 : 0, theta > 0 ? 0 : 1]
  const [eX, eY] = [Math.cos(-theta), Math.sin(-theta)]
  const d = `M 0 0 h 1 A 1 1 0 ${fA} ${fS} ${eX} ${eY} Z`
  const rotation = (begin / Math.PI) * 180
  const radius = 0.05
  const transform = `rotate(${-rotation}) scale(${radius})`

  const tAngle = begin + theta / 2
  const tRadius = 0.6 * radius
  const tx = x + tRadius * Math.cos(tAngle)
  const ty = y + tRadius * Math.sin(tAngle)
  return (
    <g>
      <g transform={`translate(${x} ${-y})`}>
        <path d={d} fill={angleColor(angle)} transform={transform} />
      </g>
      <text
        x={tx}
        y={-ty}
        textAnchor='middle'
        alignmentBaseline='central'
        vectorEffect='non-scaling-stroke'
        className={styles.angleLabel}
      >
        {angle === 'pi' ? 'π' : angle}
      </text>
    </g>
  )
}

export default Angle
