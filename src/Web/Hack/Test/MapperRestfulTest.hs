
{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell #-}
module Web.Hack.Test.MapperRestfulTest where 

import Hack
import Web.Hack.Mapper
import qualified Web.Hack.MapperRestful as M
import qualified Data.ByteString.Lazy as L
import System.IO
import Text.ParserCombinators.Parsec
import Codec.Binary.UTF8.String (encode)

import TestGenerator
import Web.Hack.MiscUtils
import Web.Hack.Util

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
}

readApa = MapperInputData .^.. (DataInput Read "monkey_business" "apa")

testEnvParserSlashApa =
  do let expected = readApa [] [("key1", "value1"), ("key2", "value2")]
         actual = M.envParser envFixtureGet{pathInfo="/monkey_business/apa&key1=\"value1\"&key2=\"value2\""}
     expected @=? actual

testEnvParserSlashApaNoKeyValues =
  do let expected = readApa [] []
         actual = M.envParser envFixtureGet{pathInfo="/monkey_business/apa"}
     expected @=? actual

testEnvParserWithInput =
  do let expected = readApa [("key1","3&/%#"),("k_e4","()()??=")] []
         actual = M.envParser envFixtureGet{pathInfo="/monkey_business/apa", queryString="key1=\"3&/%#\"&k_e4=\"()()??=\""}
     expected @=? actual

testEnvParserErrorInPath =
  do let expected = True -- MapperInputError "foo"
         actual = M.envParser envFixtureGet{pathInfo="apa"}
     expected @=? (isInputError $ actual)

testEnvParserErrorInQuery =
  do let expected = True -- MapperInputError "foo"
         actual = M.envParser envFixtureGet{pathInfo="/monkey_business/apa", queryString="47"}
     expected @=? (isInputError $ actual)

testEnvParserRoot =
  do let expected = MapperInputEmpty
         actual = M.envParser envFixtureGet{pathInfo="/"}
     expected @=? actual

testEnvParserEscaping =
  do let expected = readApa [("nyckel", "cykel")] [("nyckel", "cykel")]
         actual = M.envParser envFixtureGet{pathInfo="/monkey_business/apa&nyckel=%22cykel%22", queryString="nyckel=%22cykel%22"}
     expected @=? actual

testEnvParserPostData =
  do let expected = readApa [("nyckel", "cykel")] [("nyckel", "cykel")]
         actual = 
          M.envParser envFixtureGet{
            pathInfo="/monkey_business/apa&nyckel=%22cykel%22", 
            hackInput= L.pack $ Codec.Binary.UTF8.String.encode "nyckel=\"cykel\""}
     expected @=? actual


isInputError (MapperInputError _) = True
isInputError _ = False

testPathParser =
  do let expected = Just $ Just ("monkey_business", "apa", [("key1", "value1"), ("key2", "value2")])
         actual = parser M.pathParser "/monkey_business/apa&key1=\"value1\"&key2=\"value2\""
     expected @=? actual

testPathParserForEmptyPath =
  do let expected = Nothing
         actual = parser M.pathParser ""
     expected @=? actual

testQueryParser =
  do let expected = Just [("key1", "value1"), ("key2", "value2")]
         actual = parser M.queryParser "key1=\"value1\"&key2=\"value2\""
     expected @=? actual

testQueryParserForEmptyQuery =
  do let expected = Just []
         actual = parser M.queryParser ""
     expected @=? actual


testKeyValue =
  do let expected = Just ("key","value")
         actual = parser M.keyValue "key=\"value\""
     expected @=? actual
     
testAndKeyValue =
  do let expected = Just ("key","value")
         actual = parser M.andKeyValue "&key=\"value\""
     expected @=? actual
     
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
