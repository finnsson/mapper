{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell #-}
module Web.Hack.Test.MapperSerializerTest where

import Web.Hack.MapperSerializer
import Web.Hack.Mapper

import TestGenerator
import Test.HUnit
import Test.Framework (defaultMain)

main = defaultMain [mapperSerializerTests]

mapperSerializerTests = $testGroupGenerator

testMapperSerializer = do
  let mapperOutput = MapperOutput [[("key","value")]]
      expected = "<view><row><key>value</key></row></view>"
      actual = serializeToXml mapperOutput
  expected @=? actual
