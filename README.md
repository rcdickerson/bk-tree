bk-tree
=======

A simple [BK-Tree](http://blog.notdot.net/2007/4/Damn-Cool-Algorithms-Part-1-BK-Trees) implementation in Haskell.

The implementation of the tree structure is located in BKTree.hs. An example usage of the tree is located in SpellCorrect.hs, which uses [Levenshtein distance](http://en.wikipedia.org/wiki/Levenshtein_distance) as the tree metric to suggest spelling corrections.

For a full discussion of how this code works, see [this blog post](http://robd.io/2011/01/18/bk-trees.html).

This implementation is freely usable under the terms of the [MIT License](http://opensource.org/licenses/MIT)