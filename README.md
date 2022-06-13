# Convex Pentagonal Tilings
This project is a reproduction of the results presented by Michael Rao in the article "Exhaustive search of convex pentagons which tile the plane" (https://arxiv.org/abs/1708.00274). It consists of a Haskell application responsible for the computations along with a Next.js React application for visualising the backtracking search.


## Requirements
To compile and run the Haskell application you will need to install Stack
(https://docs.haskellstack.org/en/stable/README/#how-to-install).

To visualize the backtracking search you will need to further install Node.js (https://nodejs.org/en/download/)

## Compiling
You can compile the Haskell application by executing:
```bash
stack build --pedantic
```

## Determining good sets
Determine all relevant maximal good sets by executing
```bash
stack exec convex-pentagonal-tilings-exe good-sets
```

## Performing backtracking search
Perform backtracking search for all convex pentagons that tile the plane by executing
```bash
stack exec convex-pentagonal-tilings-exe backtracking-search
```

## Visualising backtracking search
First, start up the Haskell application in server mode using
```bash
stack exec convex-pentagonal-tilings-exe server
```
Next, open a new terminal and run the following to start the Next.js application
```bash
cd web
npm ci
npm run dev
```

## Tests
The tests can be found in the `test` directory and can be executed using
```bash
stack test --pedantic
```
