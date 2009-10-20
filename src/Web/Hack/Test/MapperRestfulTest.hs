
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


testResourceParserSlashApa =
  do let expected = "apa" -- Right "apa" :: Either ParseError String
         actual = parseApa "/apa"
     expected @=? actual

testResourceParserApa =
  do let expected = "err" -- Right "apa" :: Either ParseError String
         actual = parseApa "apa"
     expected @=? actual

parseApa v =
  case parse M.resourceParser "test" v of
    Right val -> val
    Left err -> "err"