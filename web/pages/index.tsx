import React, { useEffect, useState } from 'react'
import Head from 'next/head'
import styles from '../styles/ui.module.css'
import { NextPage } from 'next'
import Graph from '../components/Graph'
import LinearProgram from '../components/LinearProgram'
import useIteration from '../hooks/useIteration'
import {
  RiPlayFill,
  RiPauseFill,
  RiRewindFill,
  RiSkipBackFill,
  RiSkipForwardFill
} from 'react-icons/ri'

const Page: NextPage = () => {
  const [goodSetId, setGoodSetId] = useState(1)

  const [iteration, setIteration] = useState(0)

  const [response, isLoading, error] = useIteration(goodSetId, iteration)

  const [isPlaying, setIsPlaying] = useState(false)

  useEffect(() => error && setIsPlaying(false), [error]) // Stop playing upon error.

  // Increase iteration upon loading it.
  useEffect(() => {
    if (error) {
      return setIsPlaying(false)
    }
    if (isPlaying && !isLoading) {
      setIteration((i) => i + 1)
    }
  }, [isPlaying, isLoading])

  return (
    <div className={styles.container}>
      <Head>
        <title>Convex Pentagonal Tilings</title>
      </Head>
      <div className={styles.sidebar}>
        <div className={styles.section}>
          Good set:
          <input
            type='text'
            onChange={(e) => setGoodSetId(+e.target.value)}
            value={goodSetId}
          />
        </div>
        <div className={styles.section}>
          <div>Iteration:</div>
          <input
            type='text'
            onChange={(e) => setIteration(+e.target.value)}
            value={iteration}
          />
          {isLoading && <img className={styles.spinner} src='/spinner.svg' />}
        </div>
        <div className={styles.buttonContainer}>
          <button type='button' onClick={() => setIsPlaying((v) => !v)}>
            {isPlaying ? <RiPauseFill size={20} /> : <RiPlayFill size={20} />}
          </button>
        </div>
        <div className={styles.buttonContainer}>
          <button type='button' onClick={() => setIteration(0)}>
            <RiRewindFill size={20} />
          </button>
          <button
            type='button'
            onClick={() => setIteration((i) => Math.max(i - 1, 0))}
          >
            <RiSkipBackFill size={20} />
          </button>

          <button type='button' onClick={() => setIteration((i) => i + 1)}>
            <RiSkipForwardFill size={20} />
          </button>
        </div>
        {error && (
          <div className={[styles.section, styles.error].join(' ')}>
            {error.message}
          </div>
        )}
        <div className={styles.section}>Length constraints</div>
        {response && <LinearProgram linearProgram={response.linearProgram} />}
      </div>
      <div className={styles.graphContainer}>
        {response && (
          <Graph
            graph={response.graph}
            lengths={response.lengths}
            angles={response.angles}
          />
        )}
      </div>
    </div>
  )
}

export default Page
