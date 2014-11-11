#json-to-xml

JSON to XML translation using Aeson - ECS713 Functional Programming Group Project

Authors:
+ Alexandre Medeiros
+ Tom Hedges

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

##Run binary

The binary expects the name of the json files in the command line arguments and
writes the output xml to files with the same name as the json, in a `xml`
directory.
