# λDesigner

A type-safe EDSL for TouchDesigner written in Haskell. Instead of connecting nodes by hand, use the power of algebraic data types to program TouchDesigner projects in Haskell.

## Getting Started

### Prerequisites

* TouchDesigner
* `dictdiffer` python3 library
* Haskell (only tested with [haskellstack](https://docs.haskellstack.org/en/stable/README/))

### Installing

1. Clone or download [LambdaDesigner-sample](https://github.com/ulyssesp/lambdadesigner-sample).
2. Run `stack build` in the created directory
3. Open `LambdaDesigner.toe` 
4. Run `stack exec LambdaDesigner-exe`
5. Check TouchDesigner - you should be seeing a banana!


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
$ stack exec LambdaDesigner-sample-exe
```

Take a look at it running in TouchDesigner! To experiment with different node types check out [the wiki](https://github.com/ulyssesp/lambdadesigner/wiki). Not every TouchDesigner node is represented yet, but the most common ones are there.

You can also experiment with LambdaDesigner in ghci. The following code will create a node with the text "Hello, World!"

```
$ stack ghci
> r <- topRunner
> r $ textT id (bstr "Hello, world!")
```

### More complex networks

* If you're new to TouchDesigner, check out the [TouchDesigner wiki](https://www.derivative.ca/wiki099/index.php?title=Main_Page) for [some examples of nodes](https://www.derivative.ca/wiki099/index.php?title=Operator)! You can play around with nodes in TouchDesigner until you get an idea of how everything works. Even if you solely use LambdaDesigner, it's useful to understand what's happening in TouchDesigner to debug. Most of the nodes are implemented, but if you need a new node or parameter, please file an issue.

* Try out [intero](https://commercialhaskell.github.io/intero/). It's a good way to compile and run hs files easily.

* Check out some of [Oscillare's node functions](https://github.com/ulyssesp/oscillare/blob/master/src/Visuals.hs), they give a good idea of what LambdaDesigner can do. Everything under "Gens" is a `Tree TOP` and everything under "Effects" is a `Tree TOP -> Tree TOP`. Note that some of them depend on the `.frag` files found in [TD/scripts/Visuals](https://github.com/ulyssesp/oscillare/tree/master/TD/scripts/Visuals)


## Troubleshooting

#### The `scripts` node has errors

Click on the red X on the top right of the node. If it says that `dictdiffer` isn't found, make sure you have installed the `dictdiffer` python library using `pip`, and that TouchDesigner knows which python module directory to use. Check out the [TouchDesigner wiki article](http://derivative.ca/wiki099/index.php?title=Introduction_to_Python_Tutorial#Importing_Modules).

If it's a different error, then please file an issue.
