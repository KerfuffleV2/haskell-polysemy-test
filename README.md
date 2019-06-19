# polytest

A simple example of using the Polysemy Haskell package to implement the Guess-A-Number game.

This example uses `Input`, `Output`, `Reader`, `State` and `Error`.


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
3. Execute `stack run` or `cabal new-run`.

## Sample Output
```
polytest [master●●] % stack run
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
