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

##Running the converter

The converter can be run either from GHCI or the binary that was built.

The converter receives a list of filenames and read them as json files and
writes the output xml to files with the same name as the json, in a `xml`
directory.

Since Haskell is Lazy evaluated, the way it treats IO is also Lazy, which may be
a problem when you are trying to use many files at once. Currently the file
limit for this project is 240 files for a MacBook Pro (i7, 8GB RAM, ghc 7.8.3)
and 1000 files for a Dell XPS 13 (4th gen i7, 8GB RAM, ghc 7.8.3, Linux kernel
3.17.2).

###Compiled binary

The binary expects the name of the json files in the command line arguments and
can be run from the root directory of the project, as follows:

    ./dist/build/json-to-xml/json-to-xml json/002b8778-ce60-44ea-acae-81058c80bbd9.json

###GHCI

To run from GHCI, first you need to start it inside cabal sandbox, as explained
above.

Inside the interpreter, you may call the following functions:

    > runFromFileList [ "json/002b8778-ce60-44ea-acae-81058c80bbd9.json", "json/00a6ee01-14b6-4dc5-ad1e-5e8f066fcc2b.json" ]

or

    > runFromDirectory "json"

##Queries

The query functions on the internal representation are on the Query.hs file.
There is also a function with examples of queries, called doQueries, which calls
the queries on a list of Artist (output of the runFrom functions) and prints
their result.
