import { useState, useEffect } from 'react'

const useIteration = (
  goodSubsetId: number,
  iteration: number
): [TilingGraph | undefined, ConvexPolytope | undefined] => {
  const [graph, setGraph] = useState<TilingGraph | undefined>(undefined)
  const [convexPolytope, setConvexPolytope] = useState<
    ConvexPolytope | undefined
  >(undefined)

  const loadIteration = async () => {
    try {
      const response = await fetch(
        `http://localhost:3333/${goodSubsetId}/${iteration}`
      )
      const json = await response.json()
      setGraph(json.graph as any)
      setConvexPolytope(json.lp as any)
    } catch (error) {
      console.error(error)
    }
  }

  useEffect(() => {
    if (goodSubsetId >= 0 && iteration >= 0) {
      loadIteration()
    }
  }, [goodSubsetId, iteration])

  return [graph, convexPolytope]
}

export default useIteration
