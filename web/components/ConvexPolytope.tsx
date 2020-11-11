import React, { FC } from 'react'
import 'katex/dist/katex.min.css'
import TeX from '@matejmazur/react-katex'

type Term = {
  v: number
  isNegative: boolean
  x?: string
}

type Ord = '<' | '='

type ConvexPolytopeProps = {
  cp: ConvexPolytope
}

const ConvexPolytope: FC<ConvexPolytopeProps> = ({ cp }) => {
  const eqs = cp.ass.hps.reverse().map((eq) => equationTeX(eq, '='))
  const ineqs = cp.cs.reverse().map((ineq) => equationTeX(ineq, '<'))
  const lines = [...eqs, ...ineqs]
  const math = `\\begin{aligned} ${lines.join('\\\\')} \\end{aligned}`
  return <TeX block math={math} />
}

const scaleEquation = ({ t, c }: Equation): ScaledEquation => {
  const denominators = [...t.map(({ q }) => q), c.q]
  const lcd = denominators.reduce((acc, x) => lcm(acc, x), 1)
  return {
    t: t.map(({ p, q }) => (p * lcd) / q),
    c: (c.p * lcd) / c.q
  }
}

const equationTeX = (equation: Equation, relation: Ord): string => {
  const { t, c } = scaleEquation(equation)
  const allTerms: Term[] = t.map((v, i) => term(v, `l_{${i + 1}}`))
  const includedTerms = allTerms.filter(({ v }) => v !== 0)
  const lhs = includedTerms.filter(({ isNegative }) => !isNegative)
  const rhs = includedTerms.filter(({ isNegative }) => isNegative)
  if (rhs.length === 0) {
    // Constant term must be added on rhs.
    rhs.push(constantTerm(c))
  } else if (lhs.length === 0) {
    // Constant term must be added on rhs.
    lhs.push(constantTerm(-c))
  } else if (c !== 0) {
    // Constant term is non-zero, so must be included.
    // We add it as the first term on the rhs.
    rhs.unshift(constantTerm(c))
  }
  const mathLHS = lhs.map(termTeX).join(' + ')
  const mathRHS = rhs.map(termTeX).join(' + ')
  return `${mathLHS} &${relation} ${mathRHS}`
}

const term = (v: number, x: string): Term => ({
  v: Math.abs(v),
  isNegative: v < 0,
  x
})

const constantTerm = (v: number): Term => ({ v, isNegative: v < 0 })

const termTeX = ({ v, x }: Term): string => {
  if (x) {
    return v === 1 ? x : `${v}${x}`
  } else {
    // Constant term.
    return `${v}`
  }
}

const gcd = (a: number, b: number): number => (!b ? a : gcd(b, a % b))

const lcm = (a: number, b: number): number => a * (b / gcd(a, b))

export default ConvexPolytope
