import qualified Hack
import qualified Hack.Handler.SimpleServer as Handler

import qualified Data.ByteString.Lazy.UTF8 as BSLU

main :: IO ()
main = Handler.run 3001 app

app :: Hack.Env -> IO Hack.Response
app env =
    return $ Hack.Response
        200
        [("Content-Type", "text/plain; charset=utf-8")]
        $ BSLU.fromString $ (show $ env) ++ "\n\n\n" ++ (show env)
