-----------------------------------------------------------------------------
--
-- Module      :  Web.Hack.Mapper
-- Copyright   :  
-- License     :  BSD4
--
-- Maintainer  :  Oscar Finnsson, Emil Nordling
-- Stability   :  
-- Portability :  
--
-- |
--
-----------------------------------------------------------------------------

module Web.Hack.RuntimeDbMapper (
  RuntimeViewMapper (..),
  select',
  insert',
  update',
  delete',
  selectSql,
  insertSql,
  deleteSql,
  updateSql,
  methodSql,
  sqlInjectionProtection,
  getViewMap
) where

import Web.Hack.MiscUtils
import Web.Hack.Mapper
import Web.Hack.Util

import Database.HDBC
import Database.HDBC.PostgreSQL
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Char
import Data.Maybe
import List
import Control.Monad
import Maybe
import Data.Generics

data RuntimeViewMapper =
  RuntimeViewMapper {
    runtimeViewMapperConnectionString :: String
  }
  deriving (Eq, Show)

data RuntimeFunctionMapper =
  RuntimeFunctionMapper {
    runtimeFunctionMapperConnectionString :: String
  }
  deriving (Eq, Show)

instance MapperOutputter RuntimeFunctionMapper where
  getMapperOutput (RuntimeFunctionMapper cs) = getFunctionMap cs

instance MapperOutputter RuntimeViewMapper where
  getMapperOutput (RuntimeViewMapper cs) = getViewMap cs

getFunctionMap :: String -> DataInput -> IO MapperOutput
getFunctionMap cs input = error "Not implemented"

getViewMap :: String -> DataInput -> IO MapperOutput
getViewMap cs input =
  func cs input
  where
    func =
      case dataInputVerb input of
        Create -> insert'
        Read -> select'
        Update -> update'
        Delete -> delete'

type KVs = [(String,String)]




select' :: String -> DataInput -> IO MapperOutput 
select' = query' (\n w v -> selectSql n w) 

update' :: String -> DataInput -> IO MapperOutput
update' = query' updateSql

insert' :: String -> DataInput -> IO MapperOutput
insert' = query' (\n w v -> insertSql n v)

delete' :: String -> DataInput -> IO MapperOutput
delete' = query' (\n w v -> deleteSql n w)


query' :: (String -> KVs -> KVs -> String) -> String -> DataInput -> IO MapperOutput
query' sql cs callData =
     if all isNothing errors
     then do connection <- connectPostgreSQL cs 
             queryResult <- quickQueryALsafe connection (sql name paramsWhere paramsValue) 
                (map (SqlString . snd) (paramsWhere ++ paramsValue))
             let convertQR :: [(String,SqlValue)] -> [(String,String)]
                 convertQR = map (\r -> (fst r, sqlValue2String $ snd r))
             return $ MapperOutput $ map convertQR queryResult
             --   >>* (\x-> Right $ map (\i -> (fst i, sqlValue2String $ snd i)) x )
     else -- res :: Data d => d
          return $ MapperOutputError (foldr (++) "" (map fromJust errors) ) -- "hej" -- $ foldr1 (++) $ map fromJust $ filter isJust errors
          -- return res -- $ () :: Data d => d
     where  errors = map ( sqlInjectionProtection . fst) ( dataInputFilter callData )
                      ++ map ( sqlInjectionProtection . fst) ( dataInputValue callData )
            paramsWhere = surrounds $ dataInputFilter callData
            paramsValue = surrounds $ dataInputValue callData
            name = (surround $ dataInputNS callData ) 
                    ++ "." 
                    ++ (surround $ dataInputName callData)

quickQueryALsafe conn sql sqlParams = catchSql (quickQueryAL conn sql sqlParams) (\e -> return [[("error", SqlString "sql error")]])

method' :: String -> DataInput -> IO MapperOutput
method' = query' (\n w v -> methodSql n v)

-- SQL generator (select,update,insert,delete,exec)

selectSql :: String -> KVs -> String
selectSql name whereParams =
  sqlBase ++ whereSql whereParams
  where sqlBase = "select * from " ++ name

insertSql :: String -> KVs -> String
insertSql name params =
  "insert into " ++ name ++ "(" ++  ( foldr1With "," $ map fst params) ++ ")"
    ++ " values(" ++ ( foldr1With "," $ map snd params ) ++ ")"

updateSql :: String -> KVs -> KVs -> String
updateSql name whereParams valueParams =
  sqlBase ++ setSql ++ whereSql whereParams
  where sqlBase = "update " ++ name
        setSql = if (null valueParams)
                 then ""
                 else " set " ++ foldr1With ", "
                        (map (\p -> fst p ++ "=?") valueParams)

deleteSql :: String -> KVs -> String
deleteSql name whereParams =
  sqlBase ++ whereSql whereParams
  where sqlBase = "delete from " ++ name

methodSql :: String -> KVs -> String
methodSql name valueParams =
  "exec " ++ name ++ " " ++ (foldr1With " " $ map snd valueParams)

-- Helpers
--

whereSql :: KVs -> String
whereSql whereParams =
  if null whereParams
  then ""
  else " where " ++ foldr1With " and "
        (map (\p -> fst p  ++ "=?") whereParams)

quote = "\""

surround v = quote ++ v ++ quote

surrounds  = map (\kv -> (surround $ fst kv, snd kv)) 

sqlInjectionProtection :: String -> Maybe String
sqlInjectionProtection value =
  if any (turn elem "'[];\"") value
  then Just $ "Value " ++ value ++ " is illegal."
  else Nothing

sqlValue2String :: SqlValue -> String
sqlValue2String (SqlString s) = surround s
sqlValue2String (SqlChar c) = ['\'' , c, '\'']
sqlValue2String (SqlInteger i) = show i
sqlValue2String a = show a
    
