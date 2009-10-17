-----------------------------------------------------------------------------
--
-- Module      :  Web.Hack.Mapper
-- Copyright   :  
-- License     :  BSD4
--
-- Maintainer  :  Oscar Finnsson
-- Stability   :  
-- Portability :  
--
-- |
--
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell #-}
module Web.Hack.FunctionMapper (
  functionMapper,
  FunctionMapper (..),
  parsedModule
) where

import Web.Hack.Mapper

import Language.Haskell.TH
import Language.Haskell.Exts -- .Parser
-- import Language.Haskell.Exts.Syntax
-- import Language.Haskell.Parser
-- import Language.Haskell.Exts.Extension
import List
import Maybe

-- | Takes list of namespaces/modules to map.
--   Generates as 'data FunctionMapper' that is
--   an instance of MapperOutputter
functionMapper' :: String -> [String] -> Q [Dec]
functionMapper' sName namespaces =
  do let name = mkName sName
         functionMapperD = DataD [] name [] [NormalC name []] []
         -- getMapperOutput
         mapperOutputterD = InstanceD [] (AppT (ConT (mkName "MapperOutputter")) (ConT name)) [] 
     return $ [functionMapperD, mapperOutputterD]

functionMapper :: [String] -> Q Language.Haskell.TH.Exp
functionMapper namespaces =
  do moduleCode <- runIO $ readFile $ head namespaces
     funcs <- return $ allFunctions $ parsedModule moduleCode -- "Web.Hack.FunctionMapperTest"
     allFuncs <- mapM createFunctionTuple funcs -- Exp
     return $ ListE $ allFuncs

createFunctionTuple :: String -> Q Language.Haskell.TH.Exp
createFunctionTuple name =
  do let name' = mkName name
     info <- reify name'
     return $ LitE $ StringL $ show info -- TupE [VarE name', LitE $ StringL $ show info]

parsedModule :: String -> Module
parsedModule moduleCode = 
  let pMod = parseModuleWithMode ( ParseMode "test" [TemplateHaskell] False preludeFixities ) moduleCode
      moduleOrDefault (ParseFailed _ _) = Module (SrcLoc "unknown" 1 1) (ModuleName "unknown") [] Nothing Nothing [] []
      moduleOrDefault (ParseOk m) = m
  in moduleOrDefault pMod

allFunctions :: Module -> [String] -- [Language.Haskell.TH.Exp] map (VarE . mkName) .
allFunctions =
  onlyJust hsIdent . onlyJust hsPVar . onlyJust hsPatBind . hsModuleDecls
  where onlyJust f = map fromJust . filter isJust . map f

        hsModuleDecls (Module _ _ _ _ _ _ d) = d
        hsPatBind (PatBind _ p _ _ _) = Just p
        hsPatBind _ = Nothing

        hsPVar (PVar n) = Just n
        hsPVar _ = Nothing
  
        hsIdent (Ident n) = Just n
        hsIdent _ = Nothing

data FunctionMapper = FunctionMapper [DataFunction]

type DataFunction = (String,MapperInput -> MapperOutput)


instance MapperOutputter FunctionMapper where
  getMapperOutput (FunctionMapper dfs) = getFunctionMapperOutput dfs

getFunctionMapperOutput :: [DataFunction]-> MapperInput -> MapperOutput
getFunctionMapperOutput dfs mapperInput =
  if isNothing df
  then MapperOutputNotFound
  else MapperOutputError "Not implemented yet."
  where df = find (\f -> fst f == mapperInputName mapperInput) dfs


-- http://www.haskell.org/ghc/docs/latest/html/libraries/template-haskell/Language-Haskell-TH.html
-- http://www.haskell.org/bz/th3.htm
-- http://www.haskell.org/th/
  
