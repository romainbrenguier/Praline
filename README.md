Installation
============

To build PRALINE, ocaml (http://caml.inria.fr/), ocamlbuild
(http://brion.inria.fr/gallium/index.php/Ocamlbuild) and ocamlgraph
(ocamlgraph.lri.fr/index.fr.html) are needed. On Ubuntu systems they
can be installed using `apt` or `opam`.

After cloning this git repository on your computer, go to the Praline
directory, and run:

> make

This should build the main executable of the tool.

Basic Usage
===========

The basic way to use the tool is to give it a game file:

./praline examples/medimum_access.game

This should write on the screen, the different payoffs of the
equilibria that the tool has found.


Input Files
===========

The input files have a syntax close to C program.
A good way to start writing your own games is to look at the examples provided in the examples directory.
