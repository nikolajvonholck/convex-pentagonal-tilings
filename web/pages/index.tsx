import React, { useEffect, useState } from 'react'
import Head from 'next/head'
import { NextPage } from 'next'
import Graph from '../components/Graph'
import LinearProgram from '../components/LinearProgram'
import useStep from '../hooks/useStep'
import { RiPlayFill, RiPauseFill } from 'react-icons/ri'

const Page: NextPage = () => {
  const [goodSetId, setGoodSetId] = useState<number | undefined>(1)
  const [step, setStep] = useState<number | undefined>(1)

  const [response, isLoading, error] = useStep(goodSetId ?? 1, step ?? 1)

  const [isPlaying, setIsPlaying] = useState(false)

  useEffect(() => error && setIsPlaying(false), [error]) // Stop playing upon error.

  // Increase step upon loading it.
  useEffect(() => {
    if (error) {
      return setIsPlaying(false)
    }
    if (isPlaying && !isLoading) {
      setStep((i) => i + 1)
    }
  }, [isPlaying, isLoading])

  return (
    <div className='h-screen w-screen overflow-hidden flex'>
      <Head>
        <title>Convex Pentagonal Tilings</title>
      </Head>
      <div className='w-80 bg-gray-100 px-4 py-8 border-r border-gray-200 space-y-4'>
        <div>
          <label className='text-sm font-medium text-gray-700'>Good set</label>
          <input
            type='number'
            className='mt-1 focus:ring-blue-500 focus:border-blue-500 w-full shadow-sm text-sm border-gray-300 rounded-md'
            onChange={(e) => {
              const v = parseInt(e.target.value)
              if (Number.isNaN(v) || v === 0) {
                setGoodSetId(undefined)
                setStep(undefined)
              } else {
                setGoodSetId(Math.max(1, Math.min(371, v)))
                setStep(1)
              }
            }}
            value={goodSetId ?? ''}
          />
        </div>
        <div>
          <label className='text-sm font-medium text-gray-700'>Step</label>
          <input
            type='number'
            className='mt-1 focus:ring-blue-500 focus:border-blue-500 w-full shadow-sm text-sm border-gray-300 rounded-md'
            onChange={(e) => {
              const v = parseInt(e.target.value)
              if (Number.isNaN(v) || v === 0) {
                setStep(undefined)
              } else {
                setStep(Math.max(1, v))
              }
            }}
            value={step ?? ''}
          />
        </div>
        <div>
          <button
            className='w-full flex justify-center px-4 py-2 border border-gray-300 rounded-md shadow-sm text-sm font-medium text-gray-700 bg-white hover:bg-gray-50'
            onClick={() => setIsPlaying((v) => !v)}
          >
            {isPlaying ? <RiPauseFill size={20} /> : <RiPlayFill size={20} />}
          </button>
        </div>
        <div className='flex justify-center h-16'>
          {error && <div className='text-sm text-red-500'>{error.message}</div>}
          {(isLoading || isPlaying) && <img src='/spinner.svg' />}
        </div>
        <div>
          <label className='text-sm font-medium text-gray-700'>
            Length constraints
          </label>
          <div className='text-sm font-medium text-gray-700'>
            {response && (
              <LinearProgram linearProgram={response.linearProgram} />
            )}
          </div>
        </div>
      </div>
      <div className='flex flex-grow'>
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
