import { useState, useEffect } from 'react'

type Response = {
  graph: TilingGraph
  linearProgram: LinearProgram
  angles: number[]
  lengths: number[]
}

const useStep = (
  goodSetId: number,
  step: number
): [Response | undefined, boolean, Error | undefined] => {
  const [response, setResponse] = useState<Response | undefined>(undefined)
  const [isLoading, setIsLoading] = useState(false)
  const [error, setError] = useState<Error | undefined>(undefined)

  useEffect(() => {
    let ignore = false
    const loadStep = async () => {
      setIsLoading(true)
      setError(undefined)
      try {
        const response = await fetch(
          `http://localhost:3001/${goodSetId}/${step}`
        )
        const json = await response.json()
        if (!ignore) {
          setResponse(json as Response)
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
    loadStep()
    return () => {
      ignore = true
    }
  }, [goodSetId, step])

  return [response, isLoading, error]
}

export default useStep
