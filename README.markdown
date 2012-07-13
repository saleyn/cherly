Cherly
=======

Cherly (sher-lee) was originally developed by Cliff Moon for erlang to deal with in memory caching based on LRU.
Its functionality and performance were awesome,
but as time goes on, its implementation gradually obsoletes and it's hard to maintain.
To overcome these problems, I forked and made Cherly improve with original purposes.
Main improvements are described below.

 * Replaced the hash storing structure (originally used Judy hash) with the combination of Open addressing and Tree structure based on golang's hash implementation. This structure is very scalable and stable.

 * Implemented slab allocator on off-heap.

 * Replaced implemantations of port driver with NIFs.

 * Rebarized

Dependencies
=======

* Runtime
  
        Erlang >= R14B

Installation
========
* "Cherly" uses the "rebar" build system. Makefile so that simply running "make" at the top level should work.
  * [rebar](https://github.com/basho/rebar)
* "Cherly" requires Erlang R14B or later.
