import React from 'react'
import { AppProps } from 'next/app'
import { NextPage } from 'next'
import 'tailwindcss/tailwind.css'

const App: NextPage<AppProps> = ({ Component, pageProps }) => (
  <Component {...pageProps} />
)

export default App
