{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell #-}
module Web.Mapper.Test.MapperSerializerTest where

import Web.Mapper.MapperSerializer
import Web.Mapper.Mapper

import TestGenerator
import Test.HUnit
import Test.Framework (defaultMain)

main = defaultMain [mapperSerializerTest]

mapperSerializerTest = $testGroupGenerator

testMapperSerializer = do
  let mapperOutput = MapperOutput [[("key","value")]]
      expected = "<view><row><key>value</key></row></view>"
      actual = serializeToXml mapperOutput
  expected @=? actual
