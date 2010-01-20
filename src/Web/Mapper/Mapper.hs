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
-- | Defines the communication-protocoll from/to hack-derivatives and database/function-mappers.
--
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -fglasgow-exts -XExistentialQuantification #-}
module Web.Mapper.Mapper where 

import Data.Generics
import Hack

-- | Data from Hack-derivative (e.g. restful or json-rpc).
data MapperInput =
  MapperInputData DataInput 
  -- | MapperInputEmpty 
  | MapperInputError { mapperInputError :: String }
  deriving (Show,Eq)
  
data DataInput = DataInput {
    dataInputVerb :: MapperVerb,
    dataInputMeta :: Bool,
    dataInputFormat :: String,
    dataInputNS :: String, -- namespace
    dataInputName :: String, -- full name
    dataInputValue :: [(String,String)], -- key/value-pairs
    dataInputFilter :: [(String,String)] -- key/value-pairs
  }
  deriving (Show, Eq)

dataInput :: DataInput
dataInput = DataInput Read False "" "" "" [] []


data MapperVerb =
  Create | Read | Update | Delete | Info
  deriving (Show, Eq)

-- | Data from mapped functionality (e.g. haskell-function or db-layer).
data MapperOutput =
  MapperOutput { mapperOutputData :: [[(String,String)]] } -- DataBox }
  | MapperOutputError { mapperOutputErrorMessage :: String }
  | MapperOutputMeta { mapperOutputMetaInfo :: MetaInfo }
  deriving (Show, Eq)
 
-- Meta

data MetaInfo = MetaInfo [TypeInfo] [FuncInfo]
  deriving (Show,Eq)

data FuncInfo = ProcInfo {
                  procInfoReturnType::TypeInfo,
                  procInfoName::String,
                  procInfoNS::String,
                  procInfoArguments::[(String,TypeInfo)],
                  procInfoComment::Maybe String
                }
  deriving (Show,Eq)

data TypeInfo =
     TableInfo String String [ColumnInfo] [(PrivilegeType, Bool)] 
     | PrimInfo String String
 deriving (Show,Eq)

data PrivilegeType = InsertPrivilege | UpdatePrivilege | SelectPrivilege | DeletePrivilege
  deriving (Show,Eq)

data ColumnInfo = ColumnInfo String TypeInfo
   deriving (Show,Eq)

-- end Meta

data DataBox = forall d. (Data d, Show d, Eq d) => DataBox d
 -- deriving (Show,Eq)

class MapperOutputter a where
  getMapperOutput :: a -> DataInput -> IO MapperOutput

class MapperInputter a where
  getMapperInput :: a -> Hack.Env -> MapperInput
