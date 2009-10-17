
{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell #-}
module Web.Hack.FunctionMapperTest where 

import Web.Hack.Mapper
import Web.Hack.FunctionMapper

import TestGenerator

import Test.HUnit
import Language.Haskell.TH
import Language.Haskell.Exts

--main = $defaultMainGenerator

foo :: String -> String
foo n = "hej"

main = do apa2

-- $(functionMapper "SomeName" ["hej", "nej"])

--main =
--  do let foo = SomeName
--     2 @=? 2


testSimpleMapper :: IO ()
testSimpleMapper =
  do -- let funMap = FunctionMapper $(functionMapper ["/Users/devcode/Documents/Code/github/mapper/src/Web/Hack/Test/FunctionMapperTest.hs"])
     2 @=? 2

testNumberOfFunctions =
  do --let funcs =  $(functionMapper ["/Users/devcode/Documents/Code/github/mapper/src/Web/Hack/Test/FunctionMapperTest.hs"])
     3 @=? 3 -- length funcs

apa =
  do moduleCode <- readFile $ "/Users/devcode/Documents/Code/github/mapper/src/Web/Hack/Test/FunctionMapperTest.hs"
     mod <- return $ parsedModule moduleCode
     putStrLn $ show mod

apa2 =
  do putStrLn (show $ head $(functionMapper ["/Users/devcode/Documents/Code/github/mapper/src/Web/Hack/FunctionMapper.hs"]))

