import React, { FC } from 'react'

type VertexProps = {
  x: number
  y: number
  v: string
}

const Vertex: FC<VertexProps> = ({ x, y, v }) => (
  <g>
    <circle
      cx={x}
      cy={-y}
      r={0.018}
      className='fill-current text-black opacity-40'
    />
    <text
      x={x}
      y={-y}
      textAnchor='middle'
      alignmentBaseline='central'
      fontSize='0.02px'
      className='fill-current text-white'
    >
      {v}
    </text>
  </g>
)

export default Vertex
