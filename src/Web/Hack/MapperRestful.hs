-----------------------------------------------------------------------------
--
-- Module      :  Web.Hack.MapperRestful
-- Copyright   :  
-- License     :  BSD4
--
-- Maintainer  :  Oscar Finnsson, Emil Nordling
-- Stability   :  
-- Portability :  
--
-- |
--
-----------------------------------------------------------------------------

module Web.Hack.MapperRestful (
  resourceParser
) where

import Text.ParserCombinators.Parsec

resourceParser :: GenParser Char st [Char]
resourceParser = do
  char '/'
  name <- many1 letter
  return name

filterParser = many filterPair
  
filterPair = do
  char '&'
  key <- many1 letter
  char '='
  char '"'
  value <- many1 letter
  char '"'
  return (key,value)