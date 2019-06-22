# polytest

Several simple examples of using the Polysemy Haskell package to implement the Guess-A-Number game.


### src/example1/Main.hs
This is the first and simplest example. It uses `Input`, `Output`, `Reader`, `State` and `Error`.

Please note that it shouldn't be considered an example of idiomatic usage - it's intended to be as simple as possible and to demonstrate usage of various parts of Polysemy.

### src/example2/Main.hs
This example is intended to show more idiomatic usage. It uses `Input`, `Output`, `Error` and `Trace`.

I'm not entirely convinced that using `Trace` here is an improvement if one wanted to modify the output, for example, to provide writing strings without the newline.

I'm also not sure using the monadic action for random numbers in the IO version is better than the infinite list from the previous version. However, this version does demonstrate a different approach!

### src/example3/Main.hs

The author of Polysemy very kindly took the time to review my code. This is their version.

*Warning*: There is a small bug in this version where if the input list to the pure version would result in
always answering "yes" to the "play another game" question, it can run forever without terminating or producing output
and will eventually consume all memory. That would only happen if you changed the input in the example to something like `[""]` or `["y"]`

* [Link to their pull request with the review and discussion](https://github.com/KerfuffleV2/haskell-polysemy-test/pull/1)

* [Link to the forked repo with their changes](https://github.com/isovector/haskell-polysemy-test)

* [Useful comments on Reddit also](https://www.reddit.com/r/haskell/comments/c23wxd/example_for_polysemy_a_simple_guessanumber_game/erhvr0a/)


## About Polysemy
Note: I have no affiliation with the author.
1. [Announcement on /r/Haskell](https://www.reddit.com/r/haskell/comments/bbqzrd/ann_polysemy_higherorder_noboilerplate_zerocost/)
2. [Github repo](https://github.com/isovector/polysemy)
3. [On Hackage](https://hackage.haskell.org/package/polysemy)


## Quickstart

You will need [Stack](https://docs.haskellstack.org/en/stable/README/)
or [cabal-install](https://www.haskell.org/cabal/).

1. Clone the repo.
2. Enter repo directory.
3. Execute `stack run example1` or `cabal new-run example1` (or example2, and so on).

## Sample Output
```
polytest [master●●] % stack run example1
====== Result from pure program
"> "
"> I'm thinking of a number between 1 and 100. Guesses allowed: 5"
"> Enter guess #1:"
"> 50 is too high."
"> Enter guess #2:"
"> 75 is too high."
"> Enter guess #3:"
"> That's not a valid number. You just wasted a guess!"
"> Enter guess #4:"
"> That's not a valid number. You just wasted a guess!"
"> Enter guess #5:"
"> That's not a valid number. You just wasted a guess!"
"> You ran out of guesses. Game over! The number was 24."
"> "
"> Play again? (Y/n)"
"> Starting a new game!"
"> "
"> I'm thinking of a number between 1 and 100. Guesses allowed: 5"
"> Enter guess #1:"
"> 75 is too high."
"> Enter guess #2:"
Completed with error: Failed to read


====== Running IO program
>
> I'm thinking of a number between 1 and 100. Guesses allowed: 5
> Enter guess #1:
50
> 50 is too high.
> Enter guess #2:
25
> 25 is too high.
> Enter guess #3:
13
> 13 is too low.
> Enter guess #4:
20
> 20 is too low.
> Enter guess #5:
23
> 23 is too low.
> You ran out of guesses. Game over! The number was 24.
>
> Play again? (Y/n)
n
> Goodbye!
====== Result from IO program
Completion with result: [False]
```
