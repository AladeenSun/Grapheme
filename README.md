# Grapheme

Grapheme is a Scheme-like language for visualization needs in graph theory. Current version provides graph definition, graph showing, standard operations on graph, and special operations for Finite Automatons. 

## Setup

* The project is implemented in Haskell, so Cabal and GHC should be installed first.

* The GUI uses Threepenny-gui package, refer to [Threepenny-gui homepage](https://wiki.haskell.org/Threepenny-gui) for more information and installation steps.

* Graph showing module in the project needs graphviz supporting. Refer to [graphviz homepage](http://www.graphviz.org/) for installation.

* Once the you have the prerequisites above, you can run `ghc main.hs` in shell to compile the project. Then run `./main`. The application runs a simple server, you can use it by opening `localhost:8023` on any of your browsers.

## Usage

You can use it as a Scheme interpreter. Type the command in `input` field, press enter, or click `Run` button to interpret it. The result will be shown in `output` field. The command history will be shown on the right.

To use graph related features, refer to the [Documentation](Documentation.md). To show a graph G, use `(paint G)` command. The svg graph is shown at the bottom, as well as the DOT formatted graph data. If you want to access the original SVG graph, the generated `graph.dot.svg` is in the project directory.

If you need to use Grapheme to do complicated programming on graphs, refer to our sample programs. Use `load` expression to load it and call the defined functions.

