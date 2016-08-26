# Feasibility of graph mapping

Mapping application graphs to the POETS computation platform is one of the key
algorithmic challenges of the project. Here we derive a simple feasibility check
that can be used to determine whether a given graph can be mapped to a POETS
platform of a given size without introducing _dummy vertices_, whose only reason
for existence is to retransmit messages between vertices that perform useful
computation. Although dummy vertices may sometimes be unavoidable it is clear that
one should attempt to minimise their number as they waste computation resources
and increase communication costs.

## Graph mapping

A _graph_ is a tuple (_V_, _E_), where _V_ is a set of _vertices_ and _E_ is a set
of _edges_ that connect the vertices.

A _path_ is a sequence of vertices (_v<sub>1</sub>_, _v<sub>2</sub>_, ..., _v<sub>n</sub>_)
such that every successive pair of vertices (_v<sub>k</sub>_, _v<sub>k+1</sub>_) is an edge.
The _length_ of a path is the number of edges in it. Given two vertices _s_ and _t_ the
_distance_ between them _d_(_s_, _t_) is the length of the shortest path that connects them; by
convention the distance from a vertex to itself is defined to be zero: _d_(_v_, _v_)=0.

The _diameter_ of a graph is the maximum distance between any pair of vertices. Many real-life
graphs have small diameters (so-called _small world phenomenon_), e.g. social networks and the
Internet.

A _mapping m_ : _V_ → _D_ of a graph to a POETS platform is an assignment of each vertex to
a computation _device_, where _D_ is a set of devices. Devices are connected by a set of
_links_ _L_, forming a POETS graph (_D_, _L_), such that every original edge (_u_, _v_)
is either inside a device, if _m_(_u_) = _m_(_v_), or is mapped to a link, i.e. (_m_(_u_), _m_(_v_))
is an edge in the POETS graph (_D_, _L_).

Note that _mapping cannot increase the diameter_ of the original graph; indeed, every path from
the original graph is either preserved or shortened when two adjacent vertices in the path
map to the same POETS device.

## Feasibility check

POETS computation graphs are embedded in a physical hardware system and have limited connectivity,
typically a regular mesh, therefore each device has only a small fixed number of available links. 
For the purpose of this note, consider a two-dimensional mesh, where each device has four neighbours
connected through north, east, south and west links. More complex topologies are possible and will
be realised in the course of the project, but they do not change our argument in a significant way.

The key observation behind the presented feasibility check is that _POETS graphs have large diameters_.
For our two-dimensional mesh example, a POETS graph with _k_^2 devices has the diameter of at least
2*(_k_ - 1), where the optimum is achieved by a square _k_ by _k_ mesh with opposite corners connected by
_k_ - 1 links in the north-south dimension and _k_ - 1 links in the east-west dimension.

**Mapping feasibility check**: Given a graph with diameter _d_, it is possible to map it to a POETS
graph with at most _O_(_d_^2) devices if the underlying interconnect is two-dimensional, and _O_(_d_^3)
if the underlying interconnect is three-dimensional. Note: a physically realisable interconnect can be
at most three-dimensional.

As an example, a graph with 6 billion vertices representing people and their acquaintances is believed
to have the diameter of only 6 (the _six degrees of separation_ conjecture). The biggest
two-dimensional mesh that it can be mapped to has at most 16 devices: indeed, 16 = 4^2 = _k_^2 and
_d_ = 2*(_k_ - 1) = 6. This corresponds to mapping 375 million people per device on average, which is
clearly a very inefficient use of the POETS architecture. The situation is not much better
in the three-dimensional case: one can map the graph to a cube with 27 devices, which is 222 million
vertices per device.

## Flexible graph mapping

The described feasibility check is based on a natural but perhaps too restrictive definition of
graph mapping, where we insist on mapping adjacent graph vertices to either the same POETS device
or to a pair of neighbouring devices. This mapping guarantees that we never increase the length
of shortest paths. 

A more flexible mapping requirement would be to set a limit on distance increases, for example
to require that for any pair of vertices _s_ and _t_, the resulting distance increases at most
by a factor _F_, i.e. _dist_(_m_(_s_), _m_(_t_)) ≤ _F_ _dist_(_s_, _t_). 

Flexible graph mapping allows to increase the utilisation of the POETS platform: given a graph
with diameter _d_, it can be mapped to a POETS graph with _O_((_F*d_)^2) devices in the 
two-dimensional case and _O_((_F*d_)^3) devices in the three-dimensional case.

With _F_ = 3, for example, this leads to mapping of 6 billion vertices to 10 by 10 mesh of
100 devices, or 60 million vertices per device on average, which is a much better resource
utilisation at the cost of longer communication delays.

An inverse application of the feasibility check is useful: given a target number of POETS
devices _N_ we can determine the minimum required flexibility factor _F_, e.g. in the
two-dimensional case it is _F_ = _O_(_sqrt_(_N_)/_d_). For example, if we would like
to map 6 billion vertices to 1 million of POETS devices connected in a two-dimensional
mesh, the required flexibility factor is 333; in other words, some acquainted people
may end up being connected through a path with 332 intermediate vertices, which will lead
to extremely high communication costs, but excellent resource utilisation.

## Summary

Graphs with small diameter are challenging for the POETS architecture. Mapping them faithfully,
i.e. without increasing distances between vertices, may lead to very poor utilisation of
computational resources. Flexible mapping can be used to trade resource utilisation for
communication costs. The described diameter-based feasibility check is convenient for quick
estimation of required mapping flexibility.
