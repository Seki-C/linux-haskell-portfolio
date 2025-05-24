{-|
Module      : Network.Client
Description : Linuxネットワーククライアントプログラミング
Copyright   : (c) Portfolio, 2025
License     : MIT
Maintainer  : portfolio@example.com

Linuxネットワーククライアントプログラミングの基本概念を学習するためのモジュールです。
HTTPクライアント、TCPクライアント、UDPクライアントの実装について理解できます。
-}

module Network.Client
  ( -- * HTTPクライアント
    HttpRequest(..)
  , HttpResponse(..)
  , HttpMethod(..)
  , httpGet
  , httpPost
  , httpPut
  , httpDelete
    -- * TCPクライアント
  , TcpClient(..)
  , connectTcp
  , sendTcpData
  , receiveTcpData
  , closeTcpConnection
    -- * UDPクライアント
  , UdpClient(..)
  , createUdpClient
  , sendUdpData
  , receiveUdpData
  , closeUdpClient
    -- * WebSocketクライアント
  , WebSocketClient(..)
  , connectWebSocket
  , sendWebSocketMessage
  , receiveWebSocketMessage
  , closeWebSocket
    -- * FTPクライアント
  , FtpClient(..)
  , connectFtp
  , ftpLogin
  , ftpUpload
  , ftpDownload
  , ftpList
  , disconnectFtp
    -- * エラー処理
  , ClientError(..)
  ) where

import qualified Network.Socket as Net
import qualified Network.Socket.ByteString as NetBS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Control.Exception (Exception, try, IOException)
import Data.Map (Map)
import qualified Data.Map as Map

-- | クライアントエラー型
data ClientError
  = ConnectionFailed String
  | SendFailed String
  | ReceiveFailed String
  | AuthenticationFailed String
  | InvalidResponse String
  | ProtocolError String
  | TimeoutError String
  | IOError String
  deriving (Show, Eq)

instance Exception ClientError

-- | HTTPメソッド
data HttpMethod
  = GET
  | POST
  | PUT
  | DELETE
  | HEAD
  | OPTIONS
  | PATCH
  deriving (Show, Eq)

-- | HTTPリクエスト
data HttpRequest = HttpRequest
  { httpMethod  :: HttpMethod
  , httpUrl     :: String
  , httpHeaders :: Map String String
  , httpBody    :: Maybe ByteString
  } deriving (Show, Eq)

-- | HTTPレスポンス
data HttpResponse = HttpResponse
  { statusCode     :: Int
  , statusMessage  :: String
  , responseHeaders :: Map String String
  , responseBody   :: ByteString
  } deriving (Show, Eq)

-- | TCPクライアント
data TcpClient = TcpClient
  { tcpSocket :: Net.Socket
  , tcpHost   :: String
  , tcpPort   :: Int
  } deriving Eq

instance Show TcpClient where
  show client = "TcpClient{host=" ++ tcpHost client ++ 
                ", port=" ++ show (tcpPort client) ++ "}"

-- | UDPクライアント
data UdpClient = UdpClient
  { udpSocket :: Net.Socket
  , udpHost   :: String
  , udpPort   :: Int
  } deriving Eq

instance Show UdpClient where
  show client = "UdpClient{host=" ++ udpHost client ++ 
                ", port=" ++ show (udpPort client) ++ "}"

-- | WebSocketクライアント
data WebSocketClient = WebSocketClient
  { wsSocket :: Net.Socket
  , wsHost   :: String
  , wsPort   :: Int
  , wsPath   :: String
  } deriving Eq

instance Show WebSocketClient where
  show client = "WebSocketClient{host=" ++ wsHost client ++ 
                ", port=" ++ show (wsPort client) ++
                ", path=" ++ wsPath client ++ "}"

-- | FTPクライアント
data FtpClient = FtpClient
  { ftpSocket     :: Net.Socket
  , ftpHost       :: String
  , ftpPort       :: Int
  , ftpUsername   :: Maybe String
  , ftpLoggedIn   :: Bool
  } deriving Eq

instance Show FtpClient where
  show client = "FtpClient{host=" ++ ftpHost client ++ 
                ", port=" ++ show (ftpPort client) ++
                ", logged=" ++ show (ftpLoggedIn client) ++ "}"

-- | HTTP GETリクエスト
httpGet :: String -> Map String String -> IO (Either ClientError HttpResponse)
httpGet url headers = do
  let request = HttpRequest GET url headers Nothing
  executeHttpRequest request

-- | HTTP POSTリクエスト
httpPost :: String -> Map String String -> ByteString -> IO (Either ClientError HttpResponse)
httpPost url headers body = do
  let request = HttpRequest POST url headers (Just body)
  executeHttpRequest request

-- | HTTP PUTリクエスト
httpPut :: String -> Map String String -> ByteString -> IO (Either ClientError HttpResponse)
httpPut url headers body = do
  let request = HttpRequest PUT url headers (Just body)
  executeHttpRequest request

-- | HTTP DELETEリクエスト
httpDelete :: String -> Map String String -> IO (Either ClientError HttpResponse)
httpDelete url headers = do
  let request = HttpRequest DELETE url headers Nothing
  executeHttpRequest request

-- | HTTPリクエストを実行（簡易実装）
executeHttpRequest :: HttpRequest -> IO (Either ClientError HttpResponse)
executeHttpRequest request = do
  -- 実際の実装では、URLをパースし、ソケット接続を確立し、HTTPプロトコルでデータを送受信
  -- ここでは簡易的なシミュレーション
  let response = HttpResponse
        { statusCode = 200
        , statusMessage = "OK"
        , responseHeaders = Map.fromList [("Content-Type", "text/html")]
        , responseBody = BS8.pack "<html><body>Hello World</body></html>"
        }
  return $ Right response

-- | TCPクライアント接続
connectTcp :: String -> Int -> IO (Either ClientError TcpClient)
connectTcp host port = do
  result <- try $ do
    addrInfo <- Net.getAddrInfo Nothing (Just host) (Just $ show port)
    case addrInfo of
      [] -> fail $ "Cannot resolve host: " ++ host
      (ai:_) -> do
        sock <- Net.socket (Net.addrFamily ai) Net.Stream Net.defaultProtocol
        Net.connect sock (Net.addrAddress ai)
        return $ TcpClient sock host port
  
  case result of
    Left err -> return $ Left (ConnectionFailed $ show (err :: IOException))
    Right client -> return $ Right client

-- | TCPデータ送信
sendTcpData :: TcpClient -> ByteString -> IO (Either ClientError Int)
sendTcpData client dat = do
  result <- try $ NetBS.send (tcpSocket client) dat
  case result of
    Left err -> return $ Left (SendFailed $ show (err :: IOException))
    Right sent -> return $ Right sent

-- | TCPデータ受信
receiveTcpData :: TcpClient -> Int -> IO (Either ClientError ByteString)
receiveTcpData client bufSize = do
  result <- try $ NetBS.recv (tcpSocket client) bufSize
  case result of
    Left err -> return $ Left (ReceiveFailed $ show (err :: IOException))
    Right received -> return $ Right received

-- | TCP接続を閉じる
closeTcpConnection :: TcpClient -> IO (Either ClientError ())
closeTcpConnection client = do
  result <- try $ Net.close (tcpSocket client)
  case result of
    Left err -> return $ Left (IOError $ show (err :: IOException))
    Right _ -> return $ Right ()

-- | UDPクライアント作成
createUdpClient :: String -> Int -> IO (Either ClientError UdpClient)
createUdpClient host port = do
  result <- try $ do
    sock <- Net.socket Net.AF_INET Net.Datagram Net.defaultProtocol
    return $ UdpClient sock host port
  
  case result of
    Left err -> return $ Left (ConnectionFailed $ show (err :: IOException))
    Right client -> return $ Right client

-- | UDPデータ送信
sendUdpData :: UdpClient -> ByteString -> IO (Either ClientError Int)
sendUdpData client dat = do
  result <- try $ do
    addrInfo <- Net.getAddrInfo Nothing (Just $ udpHost client) (Just $ show $ udpPort client)
    case addrInfo of
      [] -> fail $ "Cannot resolve host: " ++ udpHost client
      (ai:_) -> NetBS.sendTo (udpSocket client) dat (Net.addrAddress ai)
  
  case result of
    Left err -> return $ Left (SendFailed $ show (err :: IOException))
    Right sent -> return $ Right sent

-- | UDPデータ受信
receiveUdpData :: UdpClient -> Int -> IO (Either ClientError (ByteString, String, Int))
receiveUdpData client bufSize = do
  result <- try $ NetBS.recvFrom (udpSocket client) bufSize
  case result of
    Left err -> return $ Left (ReceiveFailed $ show (err :: IOException))
    Right (received, senderAddr) -> 
      let (host, port) = parseSocketAddress senderAddr
      in return $ Right (received, host, port)

-- | UDPクライアントを閉じる
closeUdpClient :: UdpClient -> IO (Either ClientError ())
closeUdpClient client = do
  result <- try $ Net.close (udpSocket client)
  case result of
    Left err -> return $ Left (IOError $ show (err :: IOException))
    Right _ -> return $ Right ()

-- | WebSocket接続（簡易実装）
connectWebSocket :: String -> Int -> String -> IO (Either ClientError WebSocketClient)
connectWebSocket host port path = do
  result <- try $ do
    addrInfo <- Net.getAddrInfo Nothing (Just host) (Just $ show port)
    case addrInfo of
      [] -> fail $ "Cannot resolve host: " ++ host
      (ai:_) -> do
        sock <- Net.socket (Net.addrFamily ai) Net.Stream Net.defaultProtocol
        Net.connect sock (Net.addrAddress ai)
        -- WebSocketハンドシェイクを実行（簡易版）
        let handshake = "GET " ++ path ++ " HTTP/1.1\r\n" ++
                       "Host: " ++ host ++ "\r\n" ++
                       "Upgrade: websocket\r\n" ++
                       "Connection: Upgrade\r\n" ++
                       "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n" ++
                       "Sec-WebSocket-Version: 13\r\n\r\n"
        _ <- NetBS.send sock (BS8.pack handshake)
        return $ WebSocketClient sock host port path
  
  case result of
    Left err -> return $ Left (ConnectionFailed $ show (err :: IOException))
    Right client -> return $ Right client

-- | WebSocketメッセージ送信
sendWebSocketMessage :: WebSocketClient -> ByteString -> IO (Either ClientError ())
sendWebSocketMessage client message = do
  -- 実際の実装では WebSocket フレーミングプロトコルを使用
  result <- try $ NetBS.send (wsSocket client) message
  case result of
    Left err -> return $ Left (SendFailed $ show (err :: IOException))
    Right _ -> return $ Right ()

-- | WebSocketメッセージ受信
receiveWebSocketMessage :: WebSocketClient -> IO (Either ClientError ByteString)
receiveWebSocketMessage client = do
  -- 実際の実装では WebSocket フレーミングプロトコルを解析
  result <- try $ NetBS.recv (wsSocket client) 4096
  case result of
    Left err -> return $ Left (ReceiveFailed $ show (err :: IOException))
    Right received -> return $ Right received

-- | WebSocket接続を閉じる
closeWebSocket :: WebSocketClient -> IO (Either ClientError ())
closeWebSocket client = do
  result <- try $ Net.close (wsSocket client)
  case result of
    Left err -> return $ Left (IOError $ show (err :: IOException))
    Right _ -> return $ Right ()

-- | FTP接続
connectFtp :: String -> Int -> IO (Either ClientError FtpClient)
connectFtp host port = do
  result <- try $ do
    addrInfo <- Net.getAddrInfo Nothing (Just host) (Just $ show port)
    case addrInfo of
      [] -> fail $ "Cannot resolve host: " ++ host
      (ai:_) -> do
        sock <- Net.socket (Net.addrFamily ai) Net.Stream Net.defaultProtocol
        Net.connect sock (Net.addrAddress ai)
        return $ FtpClient sock host port Nothing False
  
  case result of
    Left err -> return $ Left (ConnectionFailed $ show (err :: IOException))
    Right client -> return $ Right client

-- | FTPログイン
ftpLogin :: FtpClient -> String -> String -> IO (Either ClientError FtpClient)
ftpLogin client username password = do
  -- 実際の実装では FTP プロトコルでUSER/PASSコマンドを送信
  return $ Right $ client { ftpUsername = Just username, ftpLoggedIn = True }

-- | FTPファイルアップロード
ftpUpload :: FtpClient -> FilePath -> FilePath -> IO (Either ClientError ())
ftpUpload client localPath remotePath = do
  -- 実際の実装では FTP プロトコルでSTORコマンドとデータ転送を実行
  return $ Right ()

-- | FTPファイルダウンロード
ftpDownload :: FtpClient -> FilePath -> FilePath -> IO (Either ClientError ())
ftpDownload client remotePath localPath = do
  -- 実際の実装では FTP プロトコルでRETRコマンドとデータ転送を実行
  return $ Right ()

-- | FTPディレクトリ一覧
ftpList :: FtpClient -> FilePath -> IO (Either ClientError [String])
ftpList client path = do
  -- 実際の実装では FTP プロトコルでLISTコマンドを実行
  return $ Right ["file1.txt", "file2.txt", "subdir/"]

-- | FTP接続を切断
disconnectFtp :: FtpClient -> IO (Either ClientError ())
disconnectFtp client = do
  result <- try $ Net.close (ftpSocket client)
  case result of
    Left err -> return $ Left (IOError $ show (err :: IOException))
    Right _ -> return $ Right ()

-- | ソケットアドレスをパース
parseSocketAddress :: Net.SockAddr -> (String, Int)
parseSocketAddress (Net.SockAddrInet port hostAddr) = 
  (show hostAddr, fromIntegral port)
parseSocketAddress (Net.SockAddrInet6 port _ hostAddr _) = 
  (show hostAddr, fromIntegral port)
parseSocketAddress (Net.SockAddrUnix path) = 
  (path, 0)

-- * 使用例とデモンストレーション

-- | HTTPクライアントの使用例
httpClientExample :: IO ()
httpClientExample = do
  putStrLn "HTTP GET リクエストを送信中..."
  response <- httpGet "http://example.com" Map.empty
  case response of
    Left err -> putStrLn $ "エラー: " ++ show err
    Right resp -> do
      putStrLn $ "ステータス: " ++ show (statusCode resp)
      putStrLn $ "レスポンス: " ++ BS8.unpack (responseBody resp)

-- | TCPクライアントの使用例
tcpClientExample :: IO ()
tcpClientExample = do
  putStrLn "TCP接続を確立中..."
  clientResult <- connectTcp "127.0.0.1" 8080
  case clientResult of
    Left err -> putStrLn $ "接続エラー: " ++ show err
    Right client -> do
      putStrLn "TCP接続が確立されました"
      _ <- sendTcpData client (BS8.pack "Hello TCP Server!")
      response <- receiveTcpData client 1024
      case response of
        Left err -> putStrLn $ "受信エラー: " ++ show err
        Right dat -> putStrLn $ "サーバーからの応答: " ++ BS8.unpack dat
      _ <- closeTcpConnection client
      putStrLn "TCP接続を閉じました"

-- | UDPクライアントの使用例
udpClientExample :: IO ()
udpClientExample = do
  putStrLn "UDPクライアントを作成中..."
  clientResult <- createUdpClient "127.0.0.1" 8081
  case clientResult of
    Left err -> putStrLn $ "クライアント作成エラー: " ++ show err
    Right client -> do
      putStrLn "UDPクライアントが作成されました"
      _ <- sendUdpData client (BS8.pack "Hello UDP Server!")
      response <- receiveUdpData client 1024
      case response of
        Left err -> putStrLn $ "受信エラー: " ++ show err
        Right (dat, host, port) -> 
          putStrLn $ "応答: " ++ BS8.unpack dat ++ " from " ++ host ++ ":" ++ show port
      _ <- closeUdpClient client
      putStrLn "UDPクライアントを閉じました"