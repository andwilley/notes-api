module Main where

import           Lib
import qualified Data.ByteString.Lazy.Char8    as B
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import           Web.Scotty
import           Control.Monad.IO.Class
import           Web.Scotty.Internal.Types

main :: IO ()
main =
    scotty 3000
        $   post (Literal $ TL.pack "/api")
        $   raw
        =<< (liftIO . runApi =<< body)
