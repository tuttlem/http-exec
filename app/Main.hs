import Text.Parsec
import qualified Data.Map as M
import System.IO (hSetBuffering, stdin, BufferMode(..))
import Execution (runRequests)
import Parsing (parseHttpFile)

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    content <- getContents
    putStrLn content

    case parse (parseHttpFile M.empty) "" content of
        Left err -> print err
        Right requests -> do
            print $ "Number of requests parsed: " ++ show (length requests)
            runRequests requests
