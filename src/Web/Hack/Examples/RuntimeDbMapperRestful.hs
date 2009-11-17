
{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell #-}
--module Web.Hack.EnvTest where 

import TestGenerator
import Test.HUnit
import Language.Haskell.TH
-- import Language.Haskell.Exts

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

-- | Main will initialize some MVars, one for the log and one for a Handle
-- to the log. Then it will initialize the handle with the curried
-- application. I use the SimpleServer handler here, but you could use
-- whatever you want.
main :: IO ()
main = do
        Handler.run 3001 app

-- | Strictly turn a string into lines
-- FIXME: I would like this to be a pure function, but that doesn't work
-- for some reason. Any ideas anyone?
strictLines :: String -> IO [String]
strictLines s = length s `seq` return (lines s)

-- | Very simple application structure: any POST request adds another line
-- to the log. Every request (including a POST one) sends back the last
-- five message in the log.
app :: Hack.Env -> IO Hack.Response
app env
    | Hack.requestMethod env == Hack.POST = appGet env
    | otherwise = appGet env

-- | Read in the log and format it in HTML.
appGet :: Hack.Env -> IO Hack.Response
appGet env = do
    dbOutput <- getViewMap "dbname=RuntimeDbMapperTest user=test password=test" dataInput
    return $ Hack.Response
        200
        [("Content-Type", "text/plain; charset=utf-8")]
        $ BSLU.fromString $ (show dataInput) ++ "\n\r\n\r"
          ++ (show $ dbOutput) ++ "\n\n\n" ++ (show env)
    where dataInput = getDataInput $ M.envParser env

getDataInput (MapperInputData v) = v
getDataInput _ = DataInput Create "apa" "apa" [] []