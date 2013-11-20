{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings, BangPatterns #-}

module Pitch.Network (runServer)
       
where       
  
import Control.Monad  
import Control.Monad.Writer
import Control.Exception
import Control.Concurrent.Chan
import Control.Concurrent
import Data.Maybe
import Data.Char
import Data.List
import System.IO
import Network
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS

data RequestType = GET | POST deriving (Show)
data Request = Request { rtype :: RequestType, path :: String, options :: [(String,String)] }

getData :: Request -> String
getData Request{options=os} = fromMaybe "" $ lookup "Data" os

data Response = Response { version :: String, statuscode :: Int, contentsize :: Int }
 
instance Show Request where
  show r = "Request {" ++ show (rtype r) ++ " " ++ path r ++ foldl (\acc (k, v) -> acc ++ "\n " ++ k ++ ": " ++ v) "" (options r) ++ "\n}"
  
instance Show Response where  
  show r = version r ++ " " ++ show(statuscode r) ++ " " ++ (case statuscode r of
                                                                  100 -> "Continue"
                                                                  200 -> "OK"
                                                                  404 -> "Not Found") ++ "\r\nContent-Length: " ++ show (contentsize r) ++ "\r\n\r\n"
           
fromString :: String -> RequestType           
fromString t = case t of
  "GET" -> GET
  "POST" -> POST

respond :: Request -> String -> Handle -> IO ()
respond request response handle = do
  -- print request
  let responseHeader = Response {version = "HTTP/1.1", statuscode = 200, contentsize=length response}
  hPutStr handle $ show responseHeader
  hPutStr handle response
    
--- This should really validate input or something. Separate validator? Or as-we-go?
parseRequestHelper :: ([String], [(String,String)]) -> [(String,String)]
parseRequestHelper ([], accum) = accum
parseRequestHelper (l:rest, accum) 
        | length (words l) < 2 = accum ++ [("Data", take contentLength $ concat rest)]
        | otherwise = parseRequestHelper(rest, accum ++ [(init . head $ words l, unwords . tail . words $ l)] )
  where contentLength = read contentLengthString
        (_, contentLengthString) = fromMaybe ("Content-Length", "0") $ find ((== "Content-Length") . fst) accum
                      
parseRequest :: [String] -> Request
parseRequest lns = case words (head lns) of
  [t,p,_] -> Request {rtype=fromString t, path=p, options=parseRequestHelper (tail lns, [])}

handleAccept :: Handle -> String -> IO Request
handleAccept handle hostname = do 
  -- putStrLn $ "Handling request from " ++ hostname
  response <- hGetContents handle
  --print $ lines response 
  let request = parseRequest . lines $ response
  -- print (path request)
  -- respond request handle
  return request
                                             
bindToPort :: PortNumber -> IO (Socket, PortNumber)
bindToPort min = do result <- try (listenOn $ PortNumber min) :: IO (Either SomeException Socket)
                    case result of 
                      Left _ -> bindToPort (min + 1)
                      Right s -> return (s, min)
                                                            
handleGetRequest :: (ToJSON a, ToJSON b, Monoid a) => Request -> MVar (Writer a b) -> IO String
handleGetRequest r state = do responseData <- tryTakeMVar state
                              case responseData of 
                                Just r -> do putMVar state (mapWriter (\(s, _) -> (s, mempty)) r)
                                             return . BS.unpack $ encode responseData
                                nr  -> return . BS.unpack $ encode nr
                                    

handlePostRequest :: ToJSON a => Request -> Chan a -> IO String
handlePostRequest r readChannel = do responseData <- readChan readChannel
                                     return . BS.unpack $ encode responseData

instance (ToJSON a, ToJSON b) => ToJSON (Writer a b) where
  toJSON w = let (state, messages) = runWriter w 
             in object ["messages" .= toJSON messages
                       ,"gamestate" .= toJSON state
                       ]

runServer :: (ToJSON a, ToJSON b, ToJSON c, Monoid b) => (Chan String, Chan a, MVar (Writer b c), MVar (Bool, String)) -> IO ()    
runServer (wch, rch, state, auth) = withSocketsDo $ do
  (sock, port) <- bindToPort 9000
  putStrLn $ "Listening on port " ++ show port
  forever $ do (handle, hostname, port) <- accept sock               
               request <- handleAccept handle hostname
               --request' <- receiveHTTP handle
               let !key = tail $ path request
               (hasAuthenticated, authKey) <- takeMVar auth
               putMVar auth (True, authKey)
               response <- if hasAuthenticated == False
                           then return (authKey ++ "\n")
                           else if key /= authKey
                                then return "You ain't comin' in\n"
                                else case rtype request of 
                                          GET -> handleGetRequest request state
                                          POST -> do writeChan wch $ getData request
                                                     handlePostRequest request rch
               {--response <- case rtype request of 
                                  GET -> handleGetRequest request state
                                  POST -> do writeChan wch $ getData request
                                             handlePostRequest request rch --}
               respond request response handle
               hClose handle