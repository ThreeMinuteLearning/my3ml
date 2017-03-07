{-# LANGUAGE OverloadedStrings #-}
module Main where

main :: IO ()
main = do
    ref <- newIORef 0
    spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
    runSpock 8080 (spock spockCfg app)

app :: SpockM () MySession MyAppState ()
app = do
    get root $
        text "Hello, from the 3ML API server v1."

    post "authenticate" $

    get ("hello" <//> var) $ \name -> do
        (DummyAppState ref) <- getState
        text ("Hello " <> name <> ", you are visitor number " <> T.pack (show visitorNumber))
