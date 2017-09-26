{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import           Syntax
import           Unbound.LocallyNameless

x = s2n "x"
y = s2n "y"
z = s2n "z"
w = s2n "w"
a = s2n "a"
b = s2n "b"
c = s2n "c"

prefix :: Prefix
prefix = reverse $ reverse <$> [[w],[x],[y]] -- forall w, exists x, forall y

main :: IO ()
main = do
  putStrLn $ "The prefix \"forall w, exists x, forall y, ...\" is encoded as "
          ++ show prefix ++"."
  putStrLn $ "blockIndex w prefix = "++show (blockIndex w prefix)
  putStrLn $ "blockIndex x prefix = "++show (blockIndex x prefix)
  putStrLn $ "blockIndex y prefix = "++show (blockIndex y prefix)
  putStrLn $ "blockIndex z prefix = "++show (blockIndex z prefix)
  putStrLn "hello world"
  putStrLn "hello world"
