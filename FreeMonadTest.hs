{-# LANGUAGE DeriveFunctor, GADTs, ExistentialQuantification #-}
module FreeMonadTest where

import Control.Monad.Free
import System.Random
import Text.Printf


-- monad to get and print strings to console
data Lang next =
    GetString (String -> next)
  | forall a. Random a => GetRandom (a,a) (a -> next)
  | PutStringLn String next


instance Functor Lang where
  fmap f (GetString g)        = GetString (f . g)
  fmap f (GetRandom bds g)    = GetRandom bds (f . g)
  fmap f (PutStringLn s next) = PutStringLn s (f next)

getString = Impure (GetString Pure)
putStringLn s = Impure (PutStringLn s (Pure ()))

getRandom :: Random a => (a,a) -> Free Lang a
getRandom bds = Impure (GetRandom bds Pure)

showLang :: (Show b) => Free Lang b -> String
showLang p = case p of
  (Impure (GetString f))     -> printf "<s> <- GetString\n%s" (showLang $ f "<s>")
  (Impure (PutStringLn s p)) -> printf "PutStringLn \"%s\"\n%s" (s) (showLang p)
  Pure v                     -> printf "return %s" (show v)

test :: Free Lang ()
test = do
  putStringLn "Please type something:"
  s <- getString
  putStringLn ("You just typed: " ++ s)
  r <- getRandom (1,10 :: Int)
  putStringLn ("Random number is " ++ show r)
  r' <- getRandom ('a', 'z')
  putStringLn ("Random char is " ++ show r')

interp :: Free Lang b -> IO b
interp p = case p of
  (Impure (GetString f))      -> getLine >>= \s -> interp (f s)
  (Impure (PutStringLn s p')) -> putStrLn s >> interp p'
  (Impure (GetRandom bds f))  -> randomRIO bds >>= \x -> interp (f x)
  Pure v                      -> return v

----

{-
Can you put a free monad inside a free monad, and then evaluate it?

-}
----------------------------------------------------------------------------------------------------

--data R a next = GetRandom (a,a) (a -> next)


--interpR :: Random a => Free (R a) b -> IO b
--interpR p = case p of

--  Pure v                     -> return v



-- f :: * -> *
-- data Free f a = Impure (f (Free f a)) | Pure a


{-

Let's look at the backend first.
We want to supply it with a Game function written in the GameM monad

What are the kinds of things we want to do in our game DSL?

* generate a random number
* get the GameState
* update the GameState

* create a new physics space
* add/remove/modify objects in physics
* step the physics


I think the thing I like most about the free monad approach is that you know what you are
and aren't allowed to do. It's clear. It also frees us a little bit from monad transformer hell
with all its non-intuitive uses of "lift".

-}


{-

A free monad on events would allow us to create a complete trace of a player's interaction
with the game.

Why not just a list or stream of events?

Could have a "logger" which just consumes the events and writes them out to a file.

Can you have multiple consumers of events? i.e. the game and the logger are both consumers?
Yes, this would be easy.

You could have two windows open both doing the same thing! :-)

-}