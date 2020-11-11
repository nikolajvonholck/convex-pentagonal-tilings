import { useState, useEffect } from 'react'

const useIteration = (
  goodSubsetId: number,
  iteration: number
): [
  { graph: TilingGraph; convexPolytope: ConvexPolytope } | undefined,
  boolean,
  Error | undefined
] => {
  const [graph, setGraph] = useState<TilingGraph | undefined>(undefined)
  const [convexPolytope, setConvexPolytope] = useState<
    ConvexPolytope | undefined
  >(undefined)
  const [isLoading, setIsLoading] = useState(false)
  const [error, setError] = useState<Error | undefined>(undefined)

  useEffect(() => {
    let ignore = false
    const loadIteration = async () => {
      setIsLoading(true)
      setError(undefined)
      try {
        const response = await fetch(
          `http://localhost:3333/${goodSubsetId}/${iteration}`
        )
        const json = await response.json()
        if (!ignore) {
          setGraph(json.graph as any)
          setConvexPolytope(json.lp as any)
        }
      } catch (error) {
        if (!ignore) {
          setError(error)
        }
      }
      if (!ignore) {
        setIsLoading(false)
      }
    }
    loadIteration()
    return () => {
      ignore = true
    }
  }, [goodSubsetId, iteration])

  const res = graph && convexPolytope ? { graph, convexPolytope } : undefined

  return [res, isLoading, error]
}

export default useIteration
