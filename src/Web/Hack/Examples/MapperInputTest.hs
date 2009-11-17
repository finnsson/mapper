
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
import qualified Web.Hack.MapperRestful as M
import Text.ParserCombinators.Parsec

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
    return $ Hack.Response
        200
        [("Content-Type", "text/html; charset=utf-8")]
        $ BSLU.fromString $ (show $ M.envParser env) ++ "\n\n\n" ++ (show env)