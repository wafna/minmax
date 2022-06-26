# minmax

Adversarial pruned search for two player games plus some simple two player games.

## MinMax

`MinMax.search` performs a recursive descent of the game tree to a specified level.
At each level, depending on whether the player with turn in hand is the searching player or not,
the algorithm selects the move that will maximize or minimize, respectively, the value of the game state.

While searching through a group of moves (which moves derive from an antecedent game state), the best valuation
found thus far is used to prune subsequent searches.  

Pruning occurs when the maximizer sees a valuation better than its prune value and, conversely, 
when the minimizer sees a value worse than its prune value.
In the first case, the maximizer aborts its search because it knows there's a minimizer above it
(by dint of the fact that a prune value exists), which minimizer would never choose a move that would allow
the maximizer to do better than another choice would allow.
Similar logic applies to the minimizer.

## Hexapawn

This game comes from the great Martin Gardner as an example of a game so simple a machine for learning how to play it
can be implemented with pencil and paper. See the [article](https://en.wikipedia.org/wiki/Hexapawn).

The implementation here can be scaled to any number of paws (columns, must be at least 3), and rows (must be at least 3). 

This game should end in a draw.

## Mancala

This is an ancient [family of games](https://en.wikipedia.org/wiki/Mancala).
The one implemented here is very simple, without the capture rule.
