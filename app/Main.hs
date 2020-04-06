{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Lib
import           Apikeys
import qualified Data.Set as Set
import           Web.Scotty
import           Network.HTTP.Types
import           Control.Monad.IO.Class
import           Web.Scotty.Internal.Types

main :: IO ()
main =
    scotty 8080
        $ post "/notes" $ do
            apikey <- param "apikey"
            case apikey of
                _ | Set.member apikey apikeys -> raw =<< (liftIO . runApi =<< body)
                _      -> do
                    status status500
                    html $ mconcat ["<h1>500 Internal Server Error</h1>", "Bad API Key: ", apikey]
