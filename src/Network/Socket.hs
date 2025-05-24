{-|
Module      : Network.Socket
Description : Linuxネットワークプログラミングの基礎
Copyright   : (c) Portfolio, 2025
License     : MIT
Maintainer  : portfolio@example.com

Linuxネットワークプログラミングの基本概念を学習するためのモジュールです。
ソケットプログラミング、TCP/UDP通信について理解できます。
-}

module Network.Socket
  ( -- * ソケット型
    SocketType(..)
  , Protocol(..)
  , SocketAddress(..)
  , NetworkSocket(..)
    -- * ソケット操作
  , createSocket
  , bindSocket
  , listenSocket
  , acceptConnection
  , connectSocket
  , closeSocket
    -- * データ送受信
  , sendData
  , receiveData
  , sendDataTo
  , receiveDataFrom
    -- * ソケット設定
  , setSocketOption
  , getSocketOption
  , SocketOption(..)
    -- * アドレス操作
  , parseAddress
  , formatAddress
  , resolveHostname
    -- * エラー処理
  , NetworkError(..)
  ) where

import qualified Network.Socket as Net
import qualified Network.Socket.ByteString as NetBS
import Control.Exception (Exception, try, IOException)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8

-- | ネットワークエラー型
data NetworkError
  = SocketCreationFailed String
  | BindFailed String
  | ListenFailed String
  | AcceptFailed String
  | ConnectFailed String
  | SendFailed String
  | ReceiveFailed String
  | AddressResolutionFailed String
  | SocketOptionFailed String
  | IOError String
  deriving (Show, Eq)

instance Exception NetworkError

-- | ソケットの種類
data SocketType
  = StreamSocket    -- ^ TCP（信頼性のある接続型）
  | DatagramSocket  -- ^ UDP（非信頼性のあるデータグラム型）
  | RawSocket       -- ^ RAW（低レベルソケット）
  deriving (Show, Eq)

-- | プロトコル
data Protocol
  = TCP   -- ^ Transmission Control Protocol
  | UDP   -- ^ User Datagram Protocol
  | ICMP  -- ^ Internet Control Message Protocol
  deriving (Show, Eq)

-- | ソケットアドレス
data SocketAddress
  = IPv4Address String Int      -- ^ IPv4アドレスとポート
  | IPv6Address String Int      -- ^ IPv6アドレスとポート
  | UnixAddress FilePath        -- ^ Unixドメインソケット
  deriving (Show, Eq)

-- | ネットワークソケット
data NetworkSocket = NetworkSocket
  { socketHandle :: Net.Socket
  , socketType   :: SocketType
  , socketProto  :: Protocol
  , localAddr    :: Maybe SocketAddress
  , remoteAddr   :: Maybe SocketAddress
  } deriving (Eq)

instance Show NetworkSocket where
  show sock = "NetworkSocket{type=" ++ show (socketType sock) ++
              ", proto=" ++ show (socketProto sock) ++ "}"

-- | ソケットオプション
data SocketOption
  = ReuseAddr Bool              -- ^ アドレス再利用
  | KeepAlive Bool              -- ^ キープアライブ
  | NoDelay Bool                -- ^ Nagleアルゴリズム無効化
  | Broadcast Bool              -- ^ ブロードキャスト許可
  | ReceiveBuffer Int           -- ^ 受信バッファサイズ
  | SendBuffer Int              -- ^ 送信バッファサイズ
  | ReceiveTimeout Int          -- ^ 受信タイムアウト（秒）
  | SendTimeout Int             -- ^ 送信タイムアウト（秒）
  deriving (Show, Eq)

-- | ソケットを作成
createSocket :: SocketType -> Protocol -> IO (Either NetworkError NetworkSocket)
createSocket sockType proto = do
  let (family, netSockType) = case sockType of
        StreamSocket -> (Net.AF_INET, Net.Stream)
        DatagramSocket -> (Net.AF_INET, Net.Datagram)
        RawSocket -> (Net.AF_INET, Net.Raw)
  
  result <- try $ Net.socket family netSockType Net.defaultProtocol
  case result of
    Left err -> return $ Left (SocketCreationFailed $ show (err :: IOException))
    Right sock -> return $ Right $ NetworkSocket
      { socketHandle = sock
      , socketType = sockType
      , socketProto = proto
      , localAddr = Nothing
      , remoteAddr = Nothing
      }

-- | ソケットをアドレスにバインド
bindSocket :: NetworkSocket -> SocketAddress -> IO (Either NetworkError NetworkSocket)
bindSocket sock addr = do
  netAddr <- case addr of
    IPv4Address host port -> do
      addrInfo <- Net.getAddrInfo Nothing (Just host) (Just $ show port)
      case addrInfo of
        [] -> return $ Left (AddressResolutionFailed $ "Cannot resolve " ++ host)
        (ai:_) -> return $ Right (Net.addrAddress ai)
    _ -> return $ Left (AddressResolutionFailed "Unsupported address type")
  
  case netAddr of
    Left err -> return $ Left err
    Right netAddr' -> do
      result <- try $ Net.bind (socketHandle sock) netAddr'
      case result of
        Left err -> return $ Left (BindFailed $ show (err :: IOException))
        Right _ -> return $ Right $ sock { localAddr = Just addr }

-- | ソケットを待ち受け状態にする
listenSocket :: NetworkSocket -> Int -> IO (Either NetworkError NetworkSocket)
listenSocket sock backlog = do
  result <- try $ Net.listen (socketHandle sock) backlog
  case result of
    Left err -> return $ Left (ListenFailed $ show (err :: IOException))
    Right _ -> return $ Right sock

-- | 接続を受け入れる
acceptConnection :: NetworkSocket -> IO (Either NetworkError (NetworkSocket, SocketAddress))
acceptConnection sock = do
  result <- try $ Net.accept (socketHandle sock)
  case result of
    Left err -> return $ Left (AcceptFailed $ show (err :: IOException))
    Right (clientSock, clientAddr) -> do
      let clientAddress = parseNetworkAddress clientAddr
      let clientSocket = NetworkSocket
            { socketHandle = clientSock
            , socketType = socketType sock
            , socketProto = socketProto sock
            , localAddr = Nothing
            , remoteAddr = Just clientAddress
            }
      return $ Right (clientSocket, clientAddress)

-- | ソケットで接続する
connectSocket :: NetworkSocket -> SocketAddress -> IO (Either NetworkError NetworkSocket)
connectSocket sock addr = do
  netAddr <- case addr of
    IPv4Address host port -> do
      addrInfo <- Net.getAddrInfo Nothing (Just host) (Just $ show port)
      case addrInfo of
        [] -> return $ Left (AddressResolutionFailed $ "Cannot resolve " ++ host)
        (ai:_) -> return $ Right (Net.addrAddress ai)
    _ -> return $ Left (AddressResolutionFailed "Unsupported address type")
  
  case netAddr of
    Left err -> return $ Left err
    Right netAddr' -> do
      result <- try $ Net.connect (socketHandle sock) netAddr'
      case result of
        Left err -> return $ Left (ConnectFailed $ show (err :: IOException))
        Right _ -> return $ Right $ sock { remoteAddr = Just addr }

-- | ソケットを閉じる
closeSocket :: NetworkSocket -> IO (Either NetworkError ())
closeSocket sock = do
  result <- try $ Net.close (socketHandle sock)
  case result of
    Left err -> return $ Left (IOError $ show (err :: IOException))
    Right _ -> return $ Right ()

-- | データを送信
sendData :: NetworkSocket -> ByteString -> IO (Either NetworkError Int)
sendData sock dat = do
  result <- try $ NetBS.send (socketHandle sock) dat
  case result of
    Left err -> return $ Left (SendFailed $ show (err :: IOException))
    Right sent -> return $ Right sent

-- | データを受信
receiveData :: NetworkSocket -> Int -> IO (Either NetworkError ByteString)
receiveData sock bufSize = do
  result <- try $ NetBS.recv (socketHandle sock) bufSize
  case result of
    Left err -> return $ Left (ReceiveFailed $ show (err :: IOException))
    Right received -> return $ Right received

-- | データを指定アドレスに送信（UDP用）
sendDataTo :: NetworkSocket -> ByteString -> SocketAddress -> IO (Either NetworkError Int)
sendDataTo sock dat addr = do
  netAddr <- case addr of
    IPv4Address host port -> do
      addrInfo <- Net.getAddrInfo Nothing (Just host) (Just $ show port)
      case addrInfo of
        [] -> return $ Left (AddressResolutionFailed $ "Cannot resolve " ++ host)
        (ai:_) -> return $ Right (Net.addrAddress ai)
    _ -> return $ Left (AddressResolutionFailed "Unsupported address type")
  
  case netAddr of
    Left err -> return $ Left err
    Right netAddr' -> do
      result <- try $ NetBS.sendTo (socketHandle sock) dat netAddr'
      case result of
        Left err -> return $ Left (SendFailed $ show (err :: IOException))
        Right sent -> return $ Right sent

-- | 指定アドレスからデータを受信（UDP用）
receiveDataFrom :: NetworkSocket -> Int -> IO (Either NetworkError (ByteString, SocketAddress))
receiveDataFrom sock bufSize = do
  result <- try $ NetBS.recvFrom (socketHandle sock) bufSize
  case result of
    Left err -> return $ Left (ReceiveFailed $ show (err :: IOException))
    Right (received, senderAddr) -> 
      return $ Right (received, parseNetworkAddress senderAddr)

-- | ソケットオプションを設定
setSocketOption :: NetworkSocket -> SocketOption -> IO (Either NetworkError ())
setSocketOption sock opt = do
  result <- try $ case opt of
    ReuseAddr val -> Net.setSocketOption (socketHandle sock) Net.ReuseAddr (if val then 1 else 0)
    KeepAlive val -> Net.setSocketOption (socketHandle sock) Net.KeepAlive (if val then 1 else 0)
    _ -> return () -- 他のオプションは簡易実装
  
  case result of
    Left err -> return $ Left (SocketOptionFailed $ show (err :: IOException))
    Right _ -> return $ Right ()

-- | ソケットオプションを取得
getSocketOption :: NetworkSocket -> SocketOption -> IO (Either NetworkError Int)
getSocketOption sock opt = do
  result <- try $ case opt of
    ReuseAddr _ -> Net.getSocketOption (socketHandle sock) Net.ReuseAddr
    KeepAlive _ -> Net.getSocketOption (socketHandle sock) Net.KeepAlive
    _ -> return 0 -- 他のオプションは簡易実装
  
  case result of
    Left err -> return $ Left (SocketOptionFailed $ show (err :: IOException))
    Right val -> return $ Right val

-- | 文字列からアドレスを解析
parseAddress :: String -> Maybe SocketAddress
parseAddress str = 
  case break (== ':') str of
    (host, ':':portStr) -> 
      case reads portStr of
        [(port, "")] -> Just $ IPv4Address host port
        _ -> Nothing
    _ -> Nothing

-- | アドレスを文字列にフォーマット
formatAddress :: SocketAddress -> String
formatAddress (IPv4Address host port) = host ++ ":" ++ show port
formatAddress (IPv6Address host port) = "[" ++ host ++ "]:" ++ show port
formatAddress (UnixAddress path) = path

-- | ホスト名を解決
resolveHostname :: String -> IO (Either NetworkError [SocketAddress])
resolveHostname hostname = do
  result <- try $ Net.getAddrInfo Nothing (Just hostname) Nothing
  case result of
    Left err -> return $ Left (AddressResolutionFailed $ show (err :: IOException))
    Right addrInfos -> 
      let addresses = map (parseNetworkAddress . Net.addrAddress) addrInfos
      in return $ Right addresses

-- | ネットワークアドレスをSocketAddressに変換
parseNetworkAddress :: Net.SockAddr -> SocketAddress
parseNetworkAddress (Net.SockAddrInet port hostAddr) = 
  IPv4Address (show hostAddr) (fromIntegral port)
parseNetworkAddress (Net.SockAddrInet6 port _ hostAddr _) = 
  IPv6Address (show hostAddr) (fromIntegral port)
parseNetworkAddress (Net.SockAddrUnix path) = 
  UnixAddress path