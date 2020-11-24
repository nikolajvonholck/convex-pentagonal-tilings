import React from 'react'
import Head from 'next/head'
import { useRouter } from 'next/router'
import styles from '../../styles/ui.module.css'
import { NextPage } from 'next'
import Graph from '../../components/Graph'
import LinearProgram from '../../components/LinearProgram'
import useIteration from '../../hooks/useIteration'

const Home: NextPage = () => {
  const router = useRouter()
  const { goodSubset, frame } = router.query
  const goodSubsetId = +goodSubset
  const iteration = +frame

  const [response, isLoading, error] = useIteration(goodSubsetId, iteration)
  const goToIteration = (iteration) =>
    router.push(`/${goodSubsetId}/${iteration}`)

  const nextIteration = iteration + 1
  const prevIteration = Math.max(iteration - 1, 0)

  return (
    <div className={styles.container}>
      <Head>
        <title>Convex Pentagonal Tilings</title>
      </Head>
      <div className={styles.sidebar}>
        <div className={styles.section}>Good subset: {goodSubsetId}</div>
        <div className={styles.section}>
          <div>Iteration: {iteration}</div>
          {isLoading && <img className={styles.spinner} src='/spinner.svg' />}
        </div>
        <div className={styles.buttonContainer}>
          <button type='button' onClick={() => goToIteration(prevIteration)}>
            Previous
          </button>
          <button type='button' onClick={() => goToIteration(nextIteration)}>
            Next
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

export default Home
