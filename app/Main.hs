{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Lib
import System.Environment (getArgs)
import System.IO
import Control.Monad (forM_)
import Text.Read (readMaybe, readEither)
import GT.Graph.Class (assocB1, nodeSize, edgeSize)
import GT.Graph (UndiBasicGr)

split :: Eq a => a -> [a] -> [[a]]
split d cs = sps d cs [] []
  where
    sps d [] [] rs = rs
    sps d [] r rs  = rs ++ [r]
    sps d (c:cs) r rs
      | c == d    = sps d cs r rs
      | otherwise = spe d cs (r++[c]) rs
    spe d [] [] rs = rs
    spe d [] r rs  = rs ++ [r]
    spe d (c:cs) r rs
      | c == d    = sps d cs [] (rs ++ [r])
      | otherwise = spe d cs (r++[c]) rs

parse :: forall m a. Monad m => (a -> m Bool) -> (a -> m String) -> a -> m (Either String [[Int]])
parse p f a = sequenceA <$> loop mempty a
  where
    loop :: Monad m => [Either String [Int]] -> a -> m [Either String [Int]]
    loop rs a = do
      b <- p a
      if b then return rs
      else f a >>= \s -> loop (rs <> line s) a
    line :: String -> [Either String [Int]]
    line []   = mempty
    line s@(c:_)
      | c == '%'  = mempty
      | otherwise = [traverse readEither $ split ' ' s]

main :: IO ()
main = do
  path:_ <- getArgs
  -- "testbed/soc-karate.mtx"
  p <- withFile path ReadMode $ \handle -> -- do
    parse hIsEOF hGetLine handle
  case p of
    Left err -> print err
    Right ((s:_):es) -> do
      let g :: UndiBasicGr
          g = assocB1 [] $ fmap (\(n:m:_) -> (n, m)) es
      print g
      print (nodeSize g, edgeSize g)
