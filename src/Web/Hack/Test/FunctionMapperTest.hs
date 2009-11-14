
{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell -XNoMonomorphismRestriction #-}
module Web.Hack.FunctionMapperTest where 

import Web.Hack.Mapper
import Web.Hack.FunctionMapper

import TestGenerator

import Test.HUnit
import qualified Language.Haskell.TH as TH
import Language.Haskell.Exts

import Maybe
import Outputable
import Control.Monad.Trans
import HscTypes
import System.IO (fixIO)
import GHC (runGhc, idType, Id (..))
import GHC.Paths (libdir)
import TypeRep (Type (..))
import Var (Var (..), varName)
import Name (Name, nameOccName, nameModule)
--main = $defaultMainGenerator
import FastString
import OccName (OccName (..), occNameFS)
import TyCon (TyCon (..), tyConName)
import Module (moduleName, moduleNameString)

foo :: String -> String
foo n = "hej"

main = runGhc (Just libdir) apa2 

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

-- apa2 :: HscTypes.GhcMonad m => m ()
apa2 :: Ghc () 
apa2 =
  do funcs <- getFunctions2 "hack-2009.7.15" "Hack"
     things <- return $ map fromJust $ filter isJust funcs
     types <- return $ getTypes things
     infos <- return $ getIdInfos things
     -- res <- HscTypes.liftIO $ mapM (\sdoc -> printSDoc (Outputable.ppr sdoc) defaultUserStyle) $ types

     -- typesString <- return $ foldr1 (++) $ map type2String types
     infosString <- return $ foldr1 (++) $ map (\v -> "(" ++ fst v ++ "," ++ (show $ snd v) ++ ")\n\r") $ map idInfo2NameType infos
     HscTypes.liftIO $ putStrLn infosString -- typesString

     return () -- "ok"
-- do putStrLn (show $ head $(functionMapper ["/Users/devcode/Documents/Code/github/mapper/src/Web/Hack/FunctionMapper.hs"]))

getTypes :: [TyThing] -> [TypeRep.Type]
getTypes things = map fromJust $ filter isJust $ map getType things

getType :: TyThing -> Maybe TypeRep.Type
getType (AnId id) = Just $ idType id
getType _ = Nothing

getIdInfos :: [TyThing] -> [(String,TypeRep.Type)]
getIdInfos things = map fromJust $ filter isJust $ map getIdInfo things

getIdInfo :: TyThing -> Maybe (String, TypeRep.Type)
getIdInfo (AnId id) =
  Just (n , idType id)
  where n = unpackFS $ occNameFS $  nameOccName $ varName id
getIdInfo _ = Nothing

idInfo2String :: (String,TypeRep.Type) -> (String,String)
idInfo2String (a,b) = (a, type2String b)

idInfo2NameType :: (String,TypeRep.Type) -> (String,TH.Type)
idInfo2NameType (a,b) = (a, fromJust $ type2type b)

type2String :: TypeRep.Type -> String
type2String (AppTy t1 t2) = "AppTy" ++ (type2String t1) ++ (type2String t2)
type2String (TyVarTy v) = "TyVarTy" ++ (var2String v)
type2String (TyConApp t ts) = tyCon2String t ++ (foldr (++) "" . (map type2String)) ts
type2String (FunTy t1 t2) = (type2String t1) ++ "->" ++ (type2String t2)
type2String (ForAllTy tyVar t) = "[" ++ "ForAllTy" ++ " TyVar " ++ type2String t ++ "]"
type2String (PredTy predType) = "PredTy"

type2type :: TypeRep.Type -> Maybe TH.Type
type2type (AppTy t1 t2) = Nothing
type2type (TyVarTy v) = Nothing
type2type (TyConApp t ts) = 
  if null rest
  then Just first
  else Just $ TH.AppT first (foldr1 TH.AppT rest) -- . (map type2type)) ts
  where first = tyCon2type t
        rest = map fromJust $ filter isJust $ map type2type ts
type2type (FunTy t1 t2) = 
  if isNothing first
  then second
  else if isNothing second
       then first
       else Just $ TH.AppT (fromJust first) (fromJust second)
  where first = type2type t1
        second = type2type t2


type2type (ForAllTy tyVar t) = Nothing
type2type (PredTy predType) = Nothing

var2String :: Var -> String
var2String v =  unpackFS $ occNameFS $  nameOccName $ varName v

tyCon2String :: TyCon.TyCon -> String
tyCon2String tc =
  moduleName' ++ "." ++ (unpackFS $ occNameFS $  nameOccName $ name) -- tyConName tc)
  where name = tyConName tc
        mName = Name.nameModule name
        moduleName' = moduleNameString $ moduleName $ mName

tyCon2type :: TyCon.TyCon -> TH.Type
tyCon2type tyCon =
  TH.ConT $ TH.mkName $ unpackFS $ occNameFS $  nameOccName $ tyConName tyCon

--occName2String :: OccName -> String
--occName2String  = unpackFS occ
