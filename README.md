#json-to-xml

JSON to XML translation using Aeson - ECS713 Functional Programming Group Project

##How to build

If you don't want to install the dependencies on the whole system, initialize a
cabal sandbox

    cabal sandbox init

Then install the dependencies

    cabal install --only-dependencies

Then build the project with

    cabal build

##Run GHCI inside cabal sandbox

You can run this program on ghci by using the following command

    cabal repl

Cabal will load all the dependencies and the program into ghci before displaying
the prompt.
