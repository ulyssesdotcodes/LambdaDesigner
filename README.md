# λDesigner

A type-safe EDSL for TouchDesigner written in Haskell. Instead of connecting nodes by hand, use the power of algebraic data types to program TouchDesigner projects in Haskell.

## Getting Started

### Prerequisites

* TouchDesigner
* Haskell (only tested with [haskellstack](https://docs.haskellstack.org/en/stable/README/))

### Installing

Clone or download [LambdaDesigner-sample](https://github.com/ulyssesp/lambdadesigner-sample).

## Using

### Setup

This takes you through [`Main.hs`](https://github.com/ulyssesp/LambdaDesigner-sample/blob/master/app/Main.hs). Note that you must be running [`LambdaDesigner.toe`](https://github.com/ulyssesp/LambdaDesigner-sample/blob/master/TD/LambdaDesigner.toe) to see the results of running code.

The first thing you need to do is grab a reference to the runner.

```
topRunner :: IO ( Tree TOP -> IO () )

main = do
  r <- topRunner
  ...
```

This will let us run a `Tree TOP` which will show up as an output connector on the `lambda` COMP in TouchDesigner. Lets see this happening by creating a `movieFileIn` top with the sample image.

```
main = do
  r <- topRunner
  r $ movieFileIn (bstr "app.samplesFolder+'/Map/Banana.tif'")
```

We have something we can run!

```
$ stack build
$ stack exec LamdaDesigner-sample-exe
```

Take a look at it running in TouchDesigner! To experiment with different node types check out `Op.hs`. Not every TouchDesigner node is represented yet, but the most common ones are there.
