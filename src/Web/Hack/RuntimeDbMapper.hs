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
-- |
--
-----------------------------------------------------------------------------

module Web.Hack.Mapper (
  RuntimeDbMapper (..)

) where


data RuntimeDbMapper =
  RuntimeDbMapper {
    runtimeDbMapperConnectionString :: String
  }
  derives (Eq, Show)

instance MapperOutputter RuntimeDbMapper =
  getMapperOutput (RuntimeDbMapper cs) = getDbMap cs

getDbMap :: String -> MapperInput -> MapperOutput
getDbMap cs =
  
