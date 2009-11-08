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
-- | MapperRestful maps URL-strings to MapperInputs. 
--   Example: /resource_name&key="value"&key="value"?key="value"&key="value"/key&key
--   '/' symbol ('&' kv)* ('?' kv ('&' kv)*)? (¨'/' symbol ('&' symbol)*)?
--   kv = symbol '="' string '"'
-----------------------------------------------------------------------------

module Web.Hack.MapperRestful (
  envParser,
  pathParser,
  queryParser,
  keyValue,
  manyKeyValues,
  andKeyValue
) where

import Text.ParserCombinators.Parsec
import Web.Hack.Mapper
import Data.Char
import qualified Hack
import Maybe
import Network.URI (unEscapeString)


envParser :: Hack.Env -> MapperInput
envParser env =
  let parsedPath = parse pathParser "url" (unEscapeString $ Hack.pathInfo env)
      parsedQuery = parse queryParser "query" (unEscapeString $ Hack.queryString env)
  in case parsedPath of
      Right Nothing -> MapperInputEmpty
      Right val -> 
        case parsedQuery of
          Right q -> MapperInput (fromEnvVerb $ Hack.requestMethod env) (fst $ fromJust val) q (snd $ fromJust val)
          Left qErr -> MapperInputError
      Left err -> MapperInputError

pathParser :: GenParser Char st (Maybe(String, [(String, String)]))
pathParser = do 
  char '/'
  optionMaybe pathParser'
  where
    pathParser' = do
      resourceName <- symbol
      filters <- manyKeyValues
      return $ (resourceName, filters)

queryParser :: GenParser Char st [(String, String)]
queryParser = do
  values <- valuesParser
  return $ stripMaybe values
  where
    stripMaybe (Just a) = a
    stripMaybe Nothing = []

    valuesParser :: GenParser Char st (Maybe [(String, String)])
    valuesParser = optionMaybe valuesParser' 
      where
        valuesParser' = do 
          first <- keyValue
          rest <- manyKeyValues
          return $ first : rest





manyKeyValues :: GenParser Char st [(String,String)]
manyKeyValues = many andKeyValue
  
andKeyValue :: GenParser Char st (String,String)
andKeyValue = do
  char '&'
  keyValue
  
-- Matches &key="value"
keyValue :: GenParser Char st (String,String)
keyValue = do
  key <- symbol
  string "=\""
  value <- valueParser
  char '"'
  return (key,value)
  where
    valueParser :: CharParser st String
    valueParser = many1 (satisfy (/= '"'))
  
symbol :: CharParser st String
symbol = many1 ( satisfy isId )
  where
    isId :: Char -> Bool
    isId c = isAlphaNum c || c == '_'

fromEnvVerb Hack.GET = Read
fromEnvVerb Hack.PUT = Update
fromEnvVerb Hack.POST = Create
fromEnvVerb Hack.DELETE = Delete
fromEnvVerb Hack.HEAD = Info

-- Data.ByteString.Lazy