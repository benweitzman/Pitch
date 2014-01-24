{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

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
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

data RequestType = GET | POST deriving (Show)
data Request = Request { rtype :: RequestType
                       , path :: BS.ByteString
                       , headers :: [(BS.ByteString, BS.ByteString)]
                       , body :: BS.ByteString
                       }

data Response = Response { version :: String, statuscode :: Int, contentsize :: Int }

instance Show Request where
  show r = "Request {"
           ++ show (rtype r) ++ " " ++ BS.unpack (path r)
           ++ foldl (\acc (k, v) -> acc ++ "\n " ++ BS.unpack k ++ ": " ++ BS.unpack v) 
                    ""
                    (headers r)
           ++ "\n" ++ BS.unpack (body r) ++"\n}"

instance Show Response where
  show r = version r ++ " " ++ show(statuscode r) ++ " "
           ++ (case statuscode r of
                  100 -> "Continue"
                  200 -> "OK"
                  404 -> "Not Found")
           ++ "\r\nContent-Length: " ++ show (contentsize r) ++ "\r\n\r\n"

fromString :: BS.ByteString -> RequestType
fromString t = case t of
  "GET" -> GET
  "POST" -> POST

respond :: Request -> LBS.ByteString -> Handle -> IO ()
respond request response handle = do
  let responseHeader = Response {version = "HTTP/1.1", statuscode = 200, contentsize=fromIntegral $ LBS.length response}
  LBS.hPut handle (LBS.pack $ show responseHeader)
  LBS.hPut handle response

hGetHTTPRequest :: Handle -> IO Request
hGetHTTPRequest handle = do requestLine' <- BS.hGetLine handle
                            let requestLine = BS.takeWhile (/= '\r') requestLine'
                            let [method, path, protocol] = BS.split ' ' requestLine
                            headers <- hGetHTTPHeaders handle
                            let contentLength = fromMaybe 0 $
                                  do contentString <- lookup "Content-Length" headers
                                     (cl, rest) <- BS.readInt contentString
                                     return cl
                            body <- B.hGetSome handle contentLength
                            return $ Request (fromString method) path headers body
  where hGetHTTPHeaders :: Handle -> IO [(BS.ByteString, BS.ByteString)]
        hGetHTTPHeaders handle = do line' <- BS.hGetLine handle
                                    let line = BS.takeWhile (/= '\r') line'
                                    case line of
                                      "" -> return []
                                      _ -> do hs <- hGetHTTPHeaders handle
                                              let (front, back') =  BS.breakSubstring ": " line
                                              let back = BS.drop 2 back'
                                              return $ (front, back) : hs

bindToPort :: PortNumber -> IO (Socket, PortNumber)
bindToPort min = do result <- try (listenOn $ PortNumber min) :: IO (Either SomeException Socket)
                    case result of
                      Left _ -> bindToPort (min + 1)
                      Right s -> return (s, min)

handleGetRequest :: (ToJSON a, ToJSON b, Monoid a) => Request -> MVar (Writer a b) -> IO LBS.ByteString
handleGetRequest r state = do responseData <- tryTakeMVar state
                              case responseData of
                                Just r -> do putMVar state (mapWriter (\(s, _) -> (s, mempty)) r)
                                             return $ encode responseData
                                nr  -> return $ encode nr

handlePostRequest :: ToJSON a => Request -> Chan a -> IO LBS.ByteString
handlePostRequest r readChannel = do responseData <- readChan readChannel
                                     return $ encode responseData

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
               request <- hGetHTTPRequest handle
               let key = BS.tail $ path request
               (hasAuthenticated, authKey) <- takeMVar auth
               putMVar auth (True, authKey)
               response <- if not hasAuthenticated
                           then return (LBS.pack $ authKey ++ "\n")
                           else if key /= BS.pack authKey
                                then return "You ain't comin' in\n"
                                else case rtype request of
                                          GET -> handleGetRequest request state
                                          POST -> do writeChan wch $ BS.unpack (body request)
                                                     handlePostRequest request rch
               respond request response handle
               hClose handle