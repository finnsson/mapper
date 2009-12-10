{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell #-}
{-|
  Contains useful generic functions not found elsewhere.


-}
module Web.Mapper.MiscUtils where

import Char
import Data.Maybe
import List
import Language.Haskell.TH

import Database.HDBC
import Database.HDBC
import qualified Data.Map as Map
import Control.Exception
import Data.Char
import System.IO.Unsafe
import Data.List(genericLength)

{- | A quick way to do a query.  Similar to preparing, executing, and
then calling 'fetchAllRowsAL' on a statement. See also 'quickQueryAL'' -}
quickQueryAL :: IConnection conn => conn -> String -> [SqlValue] -> IO [[(String,SqlValue)]]
quickQueryAL conn qrystr args =
    do sth <- prepare conn qrystr
       execute sth args
       fetchAllRowsAL sth

{- | Strict version of 'quickQueryAL'. -}
quickQueryAL' :: IConnection conn => conn -> String -> [SqlValue] -> IO [[(String,SqlValue)]]
quickQueryAL' conn qrystr args =
    do res <- quickQueryAL conn qrystr args
       evalAll res
       return res

evalAll :: [[a]] -> IO Integer
evalAll inp =
    do r1 <- mapM (evaluate . genericLength) inp
       evaluate (sum r1)

