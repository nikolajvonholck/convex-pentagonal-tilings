import React, { useState, useEffect } from 'react'
import Head from 'next/head'
import { useRouter } from 'next/router'
import styles from '../../styles/ui.module.css'
import { NextPage } from 'next'
import Graph from '../../components/Graph'
import Angle from '../../components/Angle'

const Home: NextPage = () => {
  const router = useRouter()
  const { goodSubset, frame } = router.query
  const goodSubsetId = parseInt(goodSubset as string)
  const frameIndex = parseInt(frame as string)
  const hasValidParams = goodSubsetId >= 0 && frameIndex >= 0

  const [graph, setGraph] = useState(undefined)

  const loadGraph = async () => {
    try {
      const response = await fetch(
        `http://localhost:3333/${goodSubsetId}/${frameIndex}`
      )
      const json = await response.json()
      setGraph(json as any)
    } catch (error) {
      console.error(error)
    }
  }

  useEffect(() => {
    if (hasValidParams) {
      loadGraph()
    }
  }, [goodSubsetId, frameIndex])

  if (!hasValidParams) {
    return null
  }

  return (
    <div className={styles.container}>
      <Head>
        <title>Convex Pentagonal Tilings</title>
      </Head>
      <div className={styles.sidebar}>
        <h2>
          Frame {frameIndex} of tiling of good subset with index {goodSubsetId}
        </h2>
        <button
          type='button'
          onClick={() =>
            router.push(`/${goodSubsetId}/${Math.max(frameIndex - 1, 0)}`)
          }
        >
          Load previous frame
        </button>
        <button
          type='button'
          onClick={() => router.push(`/${goodSubsetId}/${frameIndex + 1}`)}
        >
          Load next frame
        </button>
      </div>
      <div className={styles.graphContainer}>
        {graph && <Graph graph={graph} />}
      </div>
    </div>
  )
}

export default Home
