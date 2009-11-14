-----------------------------------------------------------------------------
--
-- Module      :  Web.Hack.Mapper
-- Copyright   :  
-- License     :  BSD4
--
-- Maintainer  :  Oscar Finnsson
-- Stability   :  
-- Portability :  
--
-- | Defines the communication-protocoll from/to hack-derivatives and database/function-mappers.
--
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -fglasgow-exts -XExistentialQuantification #-}
module Web.Hack.Mapper where 

import Data.Generics
import Hack

-- | Data from Hack-derivative (e.g. restful or json-rpc).
data MapperInput =
  MapperInputData DataInput | MapperInputEmpty | MapperInputError { mapperInputError :: String }
  deriving (Show,Eq)
  
data DataInput = DataInput {
    dataInputVerb :: MapperVerb,
    dataInputNS :: String, -- namespace
    dataInputName :: String, -- full name
    dataInputValue :: [(String,String)], -- key/value-pairs
    dataInputFilter :: [(String,String)] -- key/value-pairs
  }
  deriving (Show, Eq)

data MapperVerb =
  Create | Read | Update | Delete | Info
  deriving (Show, Eq)

-- | Data from mapped functionality (e.g. haskell-function or db-layer).
data MapperOutput =
  MapperOutput { mapperOutputData :: [[(String,String)]] } -- DataBox }
  | MapperOutputError { mapperOutputErrorMessage :: String }
  | MapperOutputNotFound
  deriving (Show, Eq)
 
data DataBox = forall d. (Data d, Show d, Eq d) => DataBox d
 -- deriving (Show,Eq)

class MapperOutputter a where
  getMapperOutput :: a -> DataInput -> IO MapperOutput

class MapperInputter a where
  getMapperInput :: a -> Hack.Env -> MapperInput

-- restful :: MapperInputter m => m
-- restful = SomeRestful
-- functions :: MapperOutputter m => m
-- functions = $(functionMapper ["Web.Hack.Foo", "Web.Hack.Bar"])
-- functions (getMapperInput restful)

-- Web.Hack.FunctionMapper (functionMapperCall)
-- Web.Hack.RuntimeDbMapper (dbMapperCall)
-- Web.Hack.Mapper (MapperInput / MapperOutput)
-- Web.Hack.MapperRestful (getRestfulMapperInput)
-- Web.Hack.MapperJsonRpc (getJsonRpcMapperInput)

