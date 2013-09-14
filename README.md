# Cloud Haskell Sample

A trivial Cloud Haskell sample app.

## Installation

I've been building this using GHC 7.6.3 and Cabal 1.18.0.1.

```shell
git clone https://github.com/reiddraper/cloud-haskell-sample.git
cd cloud-haskell-sample
cabal sandbox init
cabal install --only-dependencies
cabal build
cabal run sample # this will print the usage
```
