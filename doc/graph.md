# POETS graph

At the abstract level POETS graphs contain _vertices_ connected by _edges_. Edges are
directed and connect an _output port_ of a vertex to an _input port_ of a vertex. Edges
are used to communicate _messages_ between ports. Messages can have different types.

A graph is _well-formed_ if the following conditions are met:

* Every input port belongs to exactly one vertex.
* Every output port belongs to exactly one vertex.
* Every edge connects an output port and an input port of matching message types.

