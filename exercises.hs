{-# OPTIONS_GHC -Wno-noncanonical-monoid-instances #-}

module Exercises where

import Control.Applicative
import Data.Monoid

-- We can use the following type to simulate our own list
data List a = Empty | Value a (List a) deriving (Show)

-- Make the list a Functor
instance Functor List where
    fmap f Empty = Empty
    fmap f (Value a xs) = Value (f a) $ fmap f xs

-- Write a function which appends one list on to another
combineLists :: List a -> List a -> List a
combineLists Empty b = b
combineLists (Value x xs) b = Value x (combineLists xs b)

-- Make our list a Semigroup
instance Semigroup (List a) where
    (<>) = combineLists

-- Make our list a Monoid
instance Monoid (List a) where
    mempty = Empty
    mappend = (<>)

-- Make our list an Applicative
instance Applicative List where
    pure a = Value a Empty
    (<*>) :: List (a -> b) -> List a -> List b
    (<*>) fs vs = fmap dostuff fs
      where
        dostuff :: (a -> b) -> List b
        dostuff f = fmap f vs

-- Make sure that the List obeys the laws for Applicative and Monoid

-- Create some lists of numbers of different lengths such as:
twoValueList = Value 10 $ Value 20 Empty

-- Use <$> on the lists with a single-parameter function, such as:
plusTwo = (+ 2)

-- Use <$> and <*> on the lists with a binary function

-- Create some lists of binary functions

-- Use <*> on the binary functions list and the number lists
