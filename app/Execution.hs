module Execution (
    runRequests
) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Method
import Network.HTTP.Types.Header (HeaderName)

runRequests :: [Request -> Request] -> IO ()
runRequests reqs = do
    manager <- newManager tlsManagerSettings
    mapM_ (\f -> do
        let req = f defaultRequest
        print req  -- Print request for debugging
        res <- httpLbs req manager
        print res  -- Print response for debugging
        ) reqs
