{-# LANGUAGE OverloadedStrings #-}

import Network.Wai (responseLBS, Application)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)

main :: IO ()
main = do
    let port = 6968
    putStrLn $ "Listening on port " ++ show port
    run port app

app :: Application
app _ f =
    f $ responseLBS status200 [(hContentType, "application/json")] "{ version: \"213d9f1\" }"
