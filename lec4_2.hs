
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}

module Folds where

import Prelude hiding (reverse ,  elem  , and , foldr  ,flip , (.) , id , ($) , fst , map) 


map :: (a->b) -> [a] -> [b]
map _f [] = [] 
map  f (x : xs) = f x : map f xs 


-- this is also fine
-- map f (x : xs) = f x : f x : map f xs 


-- Parametricity is a key concept in the type system of Haskell (and other strongly-typed functional languages). It refers to the idea that a function's behavior is determined solely by its type signature when the function is parametric—that is, when it works uniformly for all types without relying on the specific structure or properties of those types.

