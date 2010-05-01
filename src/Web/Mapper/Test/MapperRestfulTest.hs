
{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell #-}
module Web.Mapper.Test.MapperRestfulTest where 

import Hack
import Web.Mapper.Mapper
import qualified Web.Mapper.MapperRestful as M
import qualified Data.ByteString.Lazy as L
import System.IO
import Text.ParserCombinators.Parsec
import Codec.Binary.UTF8.String (encode)

import TestGenerator
import Utilities.Misc
import Utilities.HDBC

import Test.HUnit
import Test.Framework (defaultMain)

import Language.Haskell.TH

main = defaultMain [mapperRestfulTests]

mapperRestfulTests = $testGroupGenerator

envFixtureGet = Env {
  requestMethod = GET
  , scriptName = ""
  , pathInfo = "/apa$name=%22pelle%22/salary&id"
  , queryString = ""
  , serverName = "localhost"
  , serverPort = 3001
  , http = [
    ("Host","localhost:3001")
    ,("Connection","keep-alive")
    ,("User-Agent","Mozilla/5.0 (Windows; U; Windows NT 6.0; en-US) AppleWebKit/532.0 (KHTML, like Gecko) Chrome/3.0.195.27 Safari/532.0")
    ,("Cache-Control","max-age=0")
    ,("Accept","application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5")
    ,("Accept-Encoding","gzip,deflate,sdch")
    ,("Accept-Language","en-US,en;q=0.8")
    ,("Accept-Charset","ISO-8859-1,utf-8;q=0.7,*;q=0.3")
    ]
  , hackVersion = [2009,7,15]
  , hackUrlScheme = HTTP
  , hackInput = L.empty
  , hackErrors = putStrLn
  , hackHeaders = []
  , hackCache = []
  , remoteHost = "some host"
}

envFixturePost = envFixtureGet { requestMethod = POST }

readApa = MapperInputData ^.. (DataInput Read False "" "monkey_business" "apa")

configApa = M.RestfulParser ["monkey_business","tree"] ["eat_banana"] "_"

envFixtureGetWith path query = 
  M.envParser configApa envFixtureGet{
    pathInfo=path
    , hackInput=L.pack $ Codec.Binary.UTF8.String.encode query
  }

testEnvParserSlashApa =
  do let expected = readApa [] [("key1", "value1"), ("key2", "value2")]
         actual = envFixtureGetWith"/monkey_business/apa&key1=\"value1\"&key2=\"value2\"?" ""
     expected @=? actual

-- testEnvParserNsMeta =
--  do let expected = MapperInputData $ DataInput Read True "" "public" ""
--         actual = envFixtureGetWith "/_/public" ""
--     expected @=? actual

testEnvParserSlashApaNoKeyValues =
  do let expected = readApa [] []
         actual = envFixtureGetWith "/monkey_business/apa" ""
     expected @=? actual

testEnvParserWithInput =
  do let expected = readApa [("key1","3&/%#"),("k_e4","()()??=")] []
         actual = envFixtureGetWith "/monkey_business/apa" "key1=\"3&/%#\"&k_e4=\"()()??=\""
     expected @=? actual

testEnvParserErrorInPath =
  do let expected = True -- MapperInputError "foo"
         actual = envFixtureGetWith "apa" ""
     expected @=? (isInputError $ actual)

testEnvParserErrorInQuery =
  do let expected = True -- MapperInputError "foo"
         actual = envFixtureGetWith "/monkey_business/apa" "47"
     expected @=? (isInputError $ actual)

testEnvParserRoot =
  do let expected = MapperInputError "Parse error" 
         actual = envFixtureGetWith "/" ""
     expected @=? actual

testEnvParserEscaping =
  do let expected = readApa [("key", "bike")] [("nyckel", "cykel")] 
         actual =
          M.envParser configApa envFixtureGet{
            pathInfo="/monkey_business/apa&nyckel=%22cykel%22", 
            hackInput= L.pack $ Codec.Binary.UTF8.String.encode "key=\"bike\""}
     expected @=? actual

testEnvParserPostData =
  do let expected = readApa [("key", "bike")] [("nyckel", "cykel")]
         actual = 
          M.envParser configApa envFixtureGet{
            pathInfo="/monkey_business/apa&nyckel=%22cykel%22", 
            queryString="key=\"bike\""}
     expected @=? actual

testEnvParserMeta =
  do let expected = MapperInputData $ DataInput Read True "" "monkey_business" "apa" [("key","bike")] [("nyckel","cykel")]
         actual =
          M.envParser configApa envFixtureGet{
            pathInfo="/_/monkey_business/apa&nyckel=%22cykel%22", 
            hackInput= L.pack $ Codec.Binary.UTF8.String.encode "key=\"bike\""}
     expected @=? actual

testEnvParserFormatXml =
  do let expected = MapperInputData $ DataInput Read True "xml" "monkey_business" "apa" [("key","bike")] [("nyckel","cykel")]
         actual =
          M.envParser configApa envFixtureGet{
            pathInfo="/_/monkey_business/apa.xml&nyckel=%22cykel%22", 
            hackInput= L.pack $ Codec.Binary.UTF8.String.encode "key=\"bike\""}
     expected @=? actual

testEnvParserFaultyNS =
  do let expected = MapperInputError "Parse error"
         actual = envFixtureGetWith "/apverksamhet/apa" ""
     expected @=? actual

testEnvParserOnlyNS =
  do let expected = MapperInputData $ DataInput Read False "" "monkey_business" "" [] []
         actual = M.envParser configApa envFixtureGet{
          pathInfo="/monkey_business"
          }
     expected @=? actual

testEnvParserOnlyPostNS =
  do let expected = MapperInputData $ DataInput Create True "" "monkey_business" "" [] []
         actual = M.envParser configApa envFixturePost{
          pathInfo="/_/monkey_business"
          }
     expected @=? actual


isInputError (MapperInputError _) = True
isInputError _ = False

-- testPathParser =
--   do let expected = MapperInputData $ DataInput Read False "monkey_business", "apa", [("key1", "value1"), ("key2", "value2")] []
--          actual = envFixtureGetWith "/monkey_business/apa&key1=\"value1\"&key2=\"value2\"" ""
--      expected @=? actual

testPathParserForEmptyPath =
  do let expected = MapperInputError "Parse error" 
         actual = envFixtureGetWith "" ""
     expected @=? actual

testQueryParser =
  do let expected = Just [("key1", "value1"), ("key2", "value2")]
         actual = parser (M.queryParser configApa) "?key1=\"value1\"&key2=\"value2\""
     expected @=? actual

testQueryParserForEmptyQuery =
  do let expected = Just []
         actual = parser (M.queryParser configApa) "?"
     expected @=? actual

testKeyValue =
  do let expected = Just ("key","value")
         actual = parser M.keyValue "key=\"value\""
     expected @=? actual
     
-- testAndKeyValue =
--   do let expected = Just ("key","value")
--          actual = parser M.andKeyValue "&key=\"value\""
--      expected @=? actual
     
testFilter =
  do let expected = Just [("key1","value1")]
         actual = parser M.manyKeyValues "&key1=\"value1\""
     expected @=? actual

testFilterVerbose =
  do let expected = (Right [("key1","value1")]) :: Either ParseError [(String,String)]
         actual = parse M.manyKeyValues "test" "&key1=\"value1\""
     show expected @=? show actual

testMultiFilter =
  do let expected = Just [("key1","value1"),("key2","value2")]
         actual = parser M.manyKeyValues "&key1=\"value1\"&key2=\"value2\""
     expected @=? actual

parser f v =
  case parse f "test" v of
    Right val -> Just val
    Left err -> Nothing
