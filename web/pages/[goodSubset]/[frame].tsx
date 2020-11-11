import React from 'react'
import Head from 'next/head'
import { useRouter } from 'next/router'
import styles from '../../styles/ui.module.css'
import { NextPage } from 'next'
import Graph from '../../components/Graph'
import ConvexPolytope from '../../components/ConvexPolytope'
import useIteration from '../../hooks/useIteration'

const Home: NextPage = () => {
  const router = useRouter()
  const { goodSubset, frame } = router.query
  const goodSubsetId = parseInt(goodSubset as string)
  const iteration = parseInt(frame as string)

  const [result, , error] = useIteration(goodSubsetId, iteration)
  const graph = result?.graph
  const convexPolytope = result?.convexPolytope

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
        <div className={styles.section}>Iteration: {iteration}</div>
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
        {convexPolytope && <ConvexPolytope cp={convexPolytope} />}
      </div>
      <div className={styles.graphContainer}>
        {graph && <Graph graph={graph} />}
      </div>
    </div>
  )
}

export default Home
