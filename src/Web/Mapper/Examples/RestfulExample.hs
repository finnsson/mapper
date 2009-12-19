import qualified Hack
import qualified Hack.Handler.SimpleServer as Handler

import qualified Data.ByteString.Lazy.UTF8 as BSLU
import qualified Web.Mapper.MapperRestful as R

main :: IO ()
main = Handler.run 3002 app

app :: Hack.Env -> IO Hack.Response
app env = return $
  Hack.Response
    200
    [("Content-Type", "text/plain; charset=utf-8")]
    $ BSLU.fromString $ instruction ++ "\n\n\n" ++ (show $ R.envParser (R.EnvParser ["public"] [] "_") env) ++ "\n\n\n" ++ (show env)

instruction = "Test the query http://localhost:3002/_/public/foo&apa=\"nej\"&monkey=\"21\"/?hej=\"89\"&foo=\"bar\""
