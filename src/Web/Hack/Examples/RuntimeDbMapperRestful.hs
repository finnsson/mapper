{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell #-}
--module Web.Hack.EnvTest where 

import TestGenerator
import Test.HUnit
import Language.Haskell.TH

import Prelude hiding (log)

import qualified Hack 
import qualified Hack.Handler.SimpleServer as Handler
import Web.Encodings (encodeHtml, parsePost)

import qualified Control.Concurrent.MVar as M
import Data.Maybe (fromMaybe)

import System.Directory (doesFileExist)
import qualified System.IO.UTF8 as U
import System.IO (withBinaryFile, IOMode (AppendMode), Handle)
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import Text.ParserCombinators.Parsec

import qualified Web.Hack.MapperRestful as M
import Web.Hack.Mapper
import Web.Hack.RuntimeDbMapper

main :: IO ()
main = do
        Handler.run 3001 appGet

-- strictLines :: String -> IO [String]
-- strictLines s = length s `seq` return (lines s)

-- | Read in the log and format it in HTML.
appGet :: Hack.Env -> IO Hack.Response
appGet env = do
    dbOutput <- getViewMap "dbname=RuntimeDbMapperTest user=test password=test" dataInput
    return $ Hack.Response
        200
        [("Content-Type", "text/plain; charset=utf-8")]
        $ BSLU.fromString $ (show dataInput) ++ "\n\r\n\r"
          ++ (show $ dbOutput) ++ "\n\n\n" ++ (show env)
    where dataInput = getDataInput $ M.envParser config env

config = M.EnvParser ["public"] ["func"] "_"

getDataInput (MapperInputData v) = v
getDataInput _ = DataInput Create False "xml" "apa" "apa" [] []
