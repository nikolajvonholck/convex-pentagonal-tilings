import React from 'react'
import { AppProps } from 'next/app'
import '../styles/globals.css'

// eslint-disable-next-line @typescript-eslint/explicit-module-boundary-types
function App({ Component, pageProps }: AppProps) {
  return <Component {...pageProps} />
}

export default App
