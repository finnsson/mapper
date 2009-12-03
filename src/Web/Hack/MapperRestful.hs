{-|
   MapperRestful maps URL-strings to MapperInputs. 

   Backus-Naur-form of the url:

   > (meta '/')? namespace resource ('.' format)? ('&' key '="' value)* ('/?' key '="' value (key '="' value )*  )?

   Example of valid URLs:

   > /public/monkeys.xml/
   > /_/public/monkeys
   > /public/monkeys&age="9"
   
-}
module Web.Hack.MapperRestful (
  EnvParser (..),
  envParser,
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
import qualified Data.ByteString.Lazy as L
import Codec.Binary.UTF8.String (decode)
import Web.Hack.Util
import Text.ParserCombinators.Parsec.Error

instance MapperInputter EnvParser where  
  getMapperInput = envParser

data EnvParser =
  EnvParser {
    viewNSs :: [String],
    functionNSs :: [String],
    metaSymbol :: String
  }


envParser :: EnvParser -> Hack.Env -> MapperInput
envParser config env =
  case parsed' of
    Right v -> v
    Left err -> MapperInputError "Parse error" 
  where parsed' = parse (envParser' config) "url" url
        url = (unEscapeString $ Hack.pathInfo env) ++ "?" ++
                (decode $ L.unpack $ Hack.hackInput env) ++ (Hack.queryString env)


envParser' config = do
  meta' <- maybeMeta config
  namespace' <- namespaceParser config
  resource' <- resourceParser config
  format' <- formatParser config
  filter' <- filterParser config
  optionMaybe $ char '/'
  query' <- try $ queryParser config
  return $ MapperInputData $
    dataInput {
      dataInputMeta = meta',
      dataInputFormat = format',
      dataInputNS = namespace',
      dataInputName =  resource',
      dataInputFilter =  filter',
      dataInputValue = query'
    }

maybeMeta ::EnvParser -> GenParser Char st Bool 
maybeMeta config =
  try (metaParse config) <|> (return $ False)

metaParse config = do
  string $ "/" ++ metaSymbol config
  return $ True 


namespaceParser :: EnvParser -> GenParser Char st String
namespaceParser config = do
  char '/'
  ns <- symbol <?> "namespace"
  let isNs =
        if elem ns (viewNSs config)
        then return $ ns
        else fail ns
  isNs

resourceParser config = do
  char '/'
  symbol

filterParser config = do
  manyKeyValues

formatParser :: EnvParser -> GenParser Char st String
formatParser config = do
  (try formatParser') <|> (return "")
  where
    formatParser' = do
      char '.'
      symbol

queryParser :: EnvParser -> GenParser Char st [(String, String)]
queryParser config = do
  char '?'
  values <- optionMaybe valuesParser'
  return $ stripMaybe values 
  where
    stripMaybe (Just a) = a
    stripMaybe Nothing = []

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
symbol = many1 ( satisfy (isAlphaNum ||* (== '_')) )

inList :: [String] -> String -> CharParser st String
inList list s = do
  let elemOf =
        if elem s list
        then return $ s
        else fail s
  elemOf

fromEnvVerb verb =
  case verb of
    Hack.GET -> Read
    Hack.PUT -> Update
    Hack.POST -> Create
    Hack.DELETE -> Delete
    Hack.HEAD -> Info
