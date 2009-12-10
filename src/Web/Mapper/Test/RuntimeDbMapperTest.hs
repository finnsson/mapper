{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell #-}
module Web.Mapper.Test.RuntimeDbMapperTest where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Data.List
import Data.Char
import Database.HDBC
import Database.HDBC.PostgreSQL
import Maybe
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Control.Exception as E

import TestGenerator
import Web.Mapper.RuntimeDbMapper
import Web.Mapper.Mapper
import Web.Mapper.Util

main = defaultMain [runtimeDbMapperTests] 

runtimeDbMapperTests = $testGroupGenerator

-- Fixtures
name = "foo"
whereParams = [("arg1","val1"),("arg2","val2")]
valueParams = [("argA","valA"),("argB","valB")]
cs = "dbname=RuntimeDbMapperTest user=test password=test"

-- Select

testSelectSql =
  do let actual = selectSql name whereParams
         expected = "select * from foo where arg1=? and arg2=?"
     expected @=? actual

testSelectSqlEmpty =
  do let params = []
         actual = selectSql name params
         expected = "select * from foo"
     expected @=? actual

testSelect =
  do actual <- select' cs (DataInput Read False "xml" "public" "int_int" [] [])
     let expected = MapperOutput [[("fst","11"), ("snd","22")]]
     expected @=? actual

testSelectWithFilter =
  do actual <- select' cs (dataInput {dataInputVerb = Read, dataInputNS = "public", dataInputName = "int_int", dataInputValue = [], dataInputFilter = [("fst","11")]})
     let expected = MapperOutput [[("fst","11"), ("snd","22")]]
     expected @=? actual

testSelectWithNegativeFilter =
  do actual <- select' cs (dataInput {dataInputVerb = Read, dataInputNS = "public", dataInputName = "int_int", dataInputValue = [], dataInputFilter = [("fst","12")]})
     let expected = MapperOutput []
     expected @=? actual

testFilter =
  do let actual = map ( sqlInjectionProtection . fst) [("fst","11")]
         expected = [Nothing] :: [Maybe String]
     expected @=? actual

-- Insert

testInsertSql =
  do let actual = insertSql name valueParams
         expected = "insert into foo(argA,argB) values(valA,valB)"
     expected @=? actual

-- Update

testUpdateSql =
  do let actual = updateSql name [] valueParams
         expected = "update foo set argA=?, argB=?"
     expected @=? actual

testUpdateSqlWithWhere =
  do let actual = updateSql name whereParams valueParams
         expected = "update foo set argA=?, argB=? where arg1=? and arg2=?"
     expected @=? actual


-- Delete
testDeleteSql =
  do let actual = deleteSql name whereParams
         expected = "delete from foo where arg1=? and arg2=?"
     expected @=? actual

-- Exec
testMethodSql =
  do let actual = methodSql name valueParams
         expected = "exec foo valA valB"
     expected @=? actual

-- Test Helpers

testSelectSqlInjection =
  do let actual1 = isJust $ sqlInjectionProtection "[ "
         expected1 = True
     expected1 @=? actual1



