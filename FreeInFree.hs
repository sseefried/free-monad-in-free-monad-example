{-# LANGUAGE RankNTypes, GADTs #-}
module FreeInFree where

import Control.Monad.Free
import System.Random
import Text.Printf

{-

While I was working on Epidemic I was working on the game logic. The game logic was
all encapsulated inside a monad called [GameM] which I wanted to be pure. Its real type
was [StateT GameState (Rand StdGen) a]. However, I wanted to simulate physics using
the Hipmunk library (a binding to the Chipmunk physics library written in C). The unfortunate
thing is that the Hipmunk interface uses the IO monad, that hive of scum and villany.
Since I wanted to keep the physics state inside the GameM monad it seemed like I was going to
have to pollute the GameM monad by turning into some monad-transformed version of IO, in order
that I could evaluate the Hipmunk computations within it.

I really didn't like this idea. My first solution was to create a pure wrapper around
Hipmunk using some serious unsafePerformIO hackery. This was pretty icky but it worked and the
ickiness was confined to one module.

However, I now think I've come up with a better solution, and that is to use free monads!

First of all, I'd like to make a remark about [GameM a = StateT GameState (Rand Stdgen) a].
Yes, technically a value of this type is a pure expression, always returning the same thing
when evaluated with the same initial state.

i.e. if [game :: GameM] and [gs :: GameState] then [evalState game gs] always evaluates to the
same value. It is referentially transparent in that sense. However, we certainly can't
evaluate on paper using equational reasoning as easily as we can for the pure function
[f x = x + 1]. In truth, it would be quite hard to evaluate exactly where e.g. a game character
is in level 5 based on the (very large) stream of keyboard events that had been received up to
that point. You would have to evaluate [game] tediously to work this out. But the thing we *can*
be certain of is that our pure monad is not polluted with such concerns as exceptions, concurrency,
etc. The only thing that can happen is that the state can be altered and random numbers can
be generated.

So, now I'll explain why I want to use free monads. I want a pretty sophisticated game monad.
I want to be able to get/put/modify GameState, I want to generate random numbers and I want
to use the Hipmunk physics library... but that's it. I don't want anything else in the monad.
If I just use the IO monad all sorts of crazy stuff is possible in principle. I can forkIO,
kill threads, throw exceptions, write to disk, get input from the console. I can't prevent
someone else (or even myself) from entering such non-sequiters into my code. It will still
typecheck.

But...

If I create a free monad with only the functionality I want then I prevent other people (and myself)
from doing the nasty things I listed above.

The only thing was I wanted to wrap up just the Hipmunk physics in its own free monad, perhaps
even releasing it as its own library. I then wanted to have a handle to it inside the GameState
data structure and evaluate it separately.

This led to the question of whether you could have a free monad inside a free monad AND
evaluate them both considering the the nested one required the IO monad to evaluate it.

The answer, happily, turned out to be "yes!". The code below nests a "put string" free monad
inside a "get string" free monad and then evaluates both.

Something to also note is the use of RankNTypes inside the GetStr monad.
(In another small example [see FreeMonadTest.hs] I needed to use existential types in order
to encapsulate a type class constraint.)


-}


data PutStr next = PutStr String next
                 | PutStrLn String next

data GetStr next = GetStr     (String -> next)
                 | forall a. EvalPutStr (Free PutStr a) (a -> next)

instance Functor GetStr where
  fmap f (GetStr g) = GetStr (f . g)
  fmap f (EvalPutStr p g) = EvalPutStr p (f . g)

instance Functor PutStr where
  fmap f (PutStr s next) = PutStr s (f next)
  fmap f (PutStrLn s next) = PutStrLn s (f next)

interpGetStr :: (forall a. Free PutStr a -> IO a) -> Free GetStr b -> IO b
interpGetStr interp' p = case p of
  Impure (GetStr f)       -> getLine   >>= \s -> interpGetStr interp' (f s)
  Impure (EvalPutStr p f) -> interp' p >>= \a -> interpGetStr interp' (f a)
  Pure a                  -> return a

interpPutStr :: Free PutStr a -> IO a
interpPutStr p = case p of
  (Impure (PutStr s p'))   -> putStr s >> interpPutStr p'
  (Impure (PutStrLn s p')) -> putStrLn s >> interpPutStr p'
  Pure a                   -> return a

interp :: Free GetStr a -> IO a
interp = interpGetStr interpPutStr

getStr :: Free GetStr String
getStr = Impure (GetStr Pure)

putString :: String -> Free PutStr ()
putString s = Impure (PutStr s (Pure ()))

putStringLn s = Impure (PutStrLn s (Pure ()))

evalPutStr :: Free PutStr a -> Free GetStr a
evalPutStr p = Impure (EvalPutStr p Pure)

test :: Free GetStr ()
test = do
  evalPutStr $ do
    putString "Please type something: "
  s <- getStr
  i <- evalPutStr $ do
    putStringLn ("What you typed was '" ++ s ++ "'")
    return $ length s
  evalPutStr $ do
    putStringLn $ "What you typed was " ++ show i ++ " characters in length"