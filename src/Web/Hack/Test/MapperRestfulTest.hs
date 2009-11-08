
{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell #-}
module Web.Hack.MapperRestfulTest where 

import Hack
import Web.Hack.Mapper
import qualified Web.Hack.MapperRestful as M
import qualified Data.ByteString.Lazy as L
import System.IO
import Text.ParserCombinators.Parsec

import TestGenerator

import Test.HUnit
import Language.Haskell.TH

main = $defaultMainGenerator

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


testEnvParserSlashApa =
  do let expected = MapperInput Read "apa" [] [("key1", "value1"), ("key2", "value2")]
         actual = M.envParser envFixtureGet{pathInfo="/apa&key1=\"value1\"&key2=\"value2\""}
     expected @=? actual

testEnvParserSlashApaNoKeyValues =
  do let expected = MapperInput Read "apa" [] []
         actual = M.envParser envFixtureGet{pathInfo="/apa"}
     expected @=? actual

testEnvParserWithInput =
  do let expected = MapperInput Read "apa2" [("key1","3&/%#"),("k_e4","()()??=")] []
         actual = M.envParser envFixtureGet{pathInfo="/apa2", queryString="key1=\"3&/%#\"&k_e4=\"()()??=\""}
     expected @=? actual

testEnvParserErrorInPath =
  do let expected = MapperInputError
         actual = M.envParser envFixtureGet{pathInfo="apa"}
     expected @=? actual

testEnvParserErrorInQuery =
  do let expected = MapperInputError
         actual = M.envParser envFixtureGet{pathInfo="/apa", queryString="47"}
     expected @=? actual

testEnvParserRoot =
  do let expected = MapperInputEmpty
         actual = M.envParser envFixtureGet{pathInfo="/"}
     expected @=? actual

testEnvParserEscaping =
  do let expected = MapperInput Read "sven" [("nyckel", "cykel")] [("nyckel", "cykel")]
         actual = M.envParser envFixtureGet{pathInfo="/sven&nyckel=%22cykel%22", queryString="nyckel=%22cykel%22"}
     expected @=? actual






testPathParser =
  do let expected = Just $ Just ("apa", [("key1", "value1"), ("key2", "value2")])
         actual = parser M.pathParser "/apa&key1=\"value1\"&key2=\"value2\""
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