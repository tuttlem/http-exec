module Parsing (
    parseComment,
    parseVariable,
    parseVerb,
    parseRequestLine,
    parseHeaderLine,
    parseHttpRequest,
    parseBody,
    parseRequestBlock,
    parseHttpFile,
    replaceVarsInString,
) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Method
import Network.HTTP.Types.Header (HeaderName)
import qualified Data.CaseInsensitive as CI
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Network.URI as URI
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

import Types (Variables)


-- Parse a comment line, ignoring it
parseComment :: Parser ()
parseComment = void $ char '#' >> many (noneOf "\n") >> newline

-- Parse variable assignments like @var = value
parseVariable :: Parser (String, String)
parseVariable = do
    void $ char '@'
    varName <- many1 alphaNum
    spaces
    void $ char '='
    spaces
    varValue <- manyTill anyChar newline
    return (varName, varValue)

-- Parse HTTP verbs like GET, POST
parseVerb :: Parser ByteString
parseVerb = do
    verb <- many1 upper
    void space
    return $ BS.pack verb

-- Replace variables in the input string
replaceVarsInString :: Variables -> String -> String
replaceVarsInString vars [] = []
replaceVarsInString vars ('$':'{':xs) =
    let (var, rest) = break (== '}') xs
        varName = takeWhile (/= '}') var
    in fromMaybe ("${" ++ varName ++ "}") (M.lookup varName vars) ++ replaceVarsInString vars (drop 1 rest)
replaceVarsInString vars (x:xs) = x : replaceVarsInString vars xs

-- Parse the request line: VERB URL
parseRequestLine :: Variables -> Parser (ByteString, String, String, Bool, Int)
parseRequestLine vars = do
    verb <- parseVerb  -- HTTP method (e.g., GET)
    url <- replaceVarsInString vars <$> many1 (noneOf " \n")
    let uri = URI.parseURI url
    case uri of
        Just parsedUri -> do
            let authority = URI.uriAuthority parsedUri
            let host = fromMaybe "" $ URI.uriRegName <$> authority
            let port = case URI.uriPort <$> authority of
                          Just (':' : p) -> read p
                          _ -> if URI.uriScheme(parsedUri) == "https:" then 443 else 80
            let path = if null (URI.uriPath parsedUri) then "/" else URI.uriPath parsedUri
            let secure = URI.uriScheme(parsedUri) == "https:"
            void newline  -- Move to the next line after parsing the request line
            return (verb, host, path, secure, port)
        Nothing -> fail $ "Invalid URL: " ++ url

-- Parse individual header lines
parseHeaderLine :: Variables -> Parser (HeaderName, ByteString)
parseHeaderLine vars = do
    name <- replaceVarsInString vars <$> many1 (noneOf ":")
    void $ string ": "
    value <- replaceVarsInString vars <$> manyTill anyChar newline
    return (CI.mk $ BS.pack name, BS.pack value)

-- Parse the complete HTTP request
parseHttpRequest :: Variables -> Parser (Request -> Request)
parseHttpRequest vars = do
    (verb, host, path, secure, port) <- parseRequestLine vars
    headers <- many $ try (parseHeaderLine vars)
    void newline -- Move past the blank line separating headers from the body or the next request
    body <- parseBody vars
    return $ \req -> req
        { method = verb
        , host = BS.pack host
        , path = BS.pack path
        , secure = secure
        , port = port
        , requestHeaders = headers ++ requestHeaders req
        , requestBody = maybe (requestBody req) RequestBodyBS body
        }

-- Parse the body of the HTTP request
parseBody :: Variables -> Parser (Maybe ByteString)
parseBody vars = optionMaybe (BS.pack . replaceVarsInString vars <$> manyTill anyChar (try newline))

-- Parse an entire block (a single request) and accumulate variables
parseRequestBlock :: Variables -> Parser (Variables, Request -> Request)
parseRequestBlock vars = do
    skipMany (void newline <|> parseComment)  -- Skip leading newlines/comments
    newVars <- M.union vars . M.fromList <$> many (parseVariable <* skipMany (void newline <|> parseComment))
    req <- parseHttpRequest newVars
    skipMany (void newline <|> parseComment)  -- Skip trailing newlines/comments
    return (newVars, req)

-- Parse the entire file, handling multiple requests and carrying over variables
parseHttpFile :: Variables -> Parser [Request -> Request]
parseHttpFile vars = go vars []
  where
    go accVars accReqs = do
        eof <- optionMaybe eof
        case eof of
            Just _ -> return $ reverse accReqs
            Nothing -> do
                (newVars, req) <- parseRequestBlock accVars
                go newVars (req : accReqs)
