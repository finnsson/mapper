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

-- | Data from Hack-derivative (e.g. restful or json-rpc).
data MapperInput =
  MapperInput {
    mapperInputName :: String, -- full name, including namespace
    mapperInputValue :: [(String,DataBox)], -- key/value-pairs
    mapperInputFilter :: [(String,String)] -- key/value-pairs
  }

-- | Data from mapped functionality (e.g. haskell-function or db-layer).
data MapperOutput =
  MapperOutput { mapperOutputData :: DataBox }
  | MapperOutputError { mapperOutputErrorMessage :: String }
  | MapperOutputNotFound
 
data DataBox = forall d. (Data d) => DataBox d

class MapperOutputter a where
  getMapperOutput :: a -> MapperInput -> MapperOutput

class MapperInputter a where
  getMapperInput :: a -> MapperInput

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

