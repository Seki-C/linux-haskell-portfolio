{-|
Module      : Process.Communication
Description : Linuxプロセス間通信（IPC）の基本概念
Copyright   : (c) Portfolio, 2025
License     : MIT
Maintainer  : portfolio@example.com

Linuxプロセス間通信（IPC）の基本概念と操作を学習するためのモジュールです。
パイプ、共有メモリ、メッセージキューなどのIPC機能について理解できます。
-}

module Process.Communication
  ( -- * パイプ通信
    Pipe(..)
  , createPipe
  , readPipe
  , writePipe
  , closePipe
    -- * 名前付きパイプ（FIFO）
  , NamedPipe(..)
  , createNamedPipe
  , openNamedPipe
  , deleteNamedPipe
    -- * 共有メモリ
  , SharedMemory(..)
  , createSharedMemory
  , attachSharedMemory
  , detachSharedMemory
  , removeSharedMemory
    -- * メッセージキュー
  , MessageQueue(..)
  , Message(..)
  , createMessageQueue
  , sendMessage
  , receiveMessage
  , deleteMessageQueue
    -- * セマフォ
  , Semaphore(..)
  , createSemaphore
  , waitSemaphore
  , postSemaphore
  , destroySemaphore
    -- * ソケットペア
  , SocketPair(..)
  , createSocketPair
  , sendSocketData
  , receiveSocketData
  , closeSocketPair
    -- * エラー処理
  , IPCError(..)
  ) where

import Foreign.Ptr (Ptr)
import Foreign.C.Types (CInt, CSize)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Control.Exception (Exception)
import Linux.Core (ProcessID)

-- | IPC操作のエラー型
data IPCError
  = PipeCreationFailed String
  | FifoCreationFailed String
  | SharedMemoryFailed String
  | MessageQueueFailed String
  | SemaphoreFailed String
  | SocketPairFailed String
  | PermissionDenied String
  | ResourceBusy String
  | NoSuchResource String
  | IOError String
  deriving (Show, Eq)

instance Exception IPCError

-- | パイプ
data Pipe = Pipe
  { pipeReadFd  :: CInt    -- ^ 読み取り用ファイルディスクリプタ
  , pipeWriteFd :: CInt    -- ^ 書き込み用ファイルディスクリプタ
  } deriving (Show, Eq)

-- | 名前付きパイプ（FIFO）
data NamedPipe = NamedPipe
  { fifoPath :: FilePath   -- ^ FIFOのパス
  , fifoFd   :: CInt       -- ^ ファイルディスクリプタ
  } deriving (Show, Eq)

-- | 共有メモリ
data SharedMemory = SharedMemory
  { shmId   :: CInt        -- ^ 共有メモリID
  , shmAddr :: Ptr ()      -- ^ 共有メモリアドレス
  , shmSize :: CSize       -- ^ 共有メモリサイズ
  } deriving (Show, Eq)

-- | メッセージキュー
data MessageQueue = MessageQueue
  { mqId :: CInt           -- ^ メッセージキューID
  } deriving (Show, Eq)

-- | メッセージ
data Message = Message
  { msgType :: Int         -- ^ メッセージタイプ
  , msgData :: ByteString  -- ^ メッセージデータ
  } deriving (Show, Eq)

-- | セマフォ
data Semaphore = Semaphore
  { semId :: CInt          -- ^ セマフォID
  } deriving (Show, Eq)

-- | ソケットペア
data SocketPair = SocketPair
  { socketFd1 :: CInt      -- ^ ソケット1のファイルディスクリプタ
  , socketFd2 :: CInt      -- ^ ソケット2のファイルディスクリプタ
  } deriving (Show, Eq)

-- | パイプを作成
createPipe :: IO (Either IPCError Pipe)
createPipe = do
  -- 実際の実装では pipe() システムコールを呼び出す
  -- ここでは簡易的なシミュレーション
  let readFd = 3
      writeFd = 4
  return $ Right $ Pipe readFd writeFd

-- | パイプからデータを読み取り
readPipe :: Pipe -> Int -> IO (Either IPCError ByteString)
readPipe pipe bufSize = do
  -- 実際の実装では read() システムコールを呼び出す
  return $ Right $ BS8.pack "sample pipe data"

-- | パイプにデータを書き込み
writePipe :: Pipe -> ByteString -> IO (Either IPCError Int)
writePipe pipe dat = do
  -- 実際の実装では write() システムコールを呼び出す
  return $ Right $ BS8.length dat

-- | パイプを閉じる
closePipe :: Pipe -> IO (Either IPCError ())
closePipe pipe = do
  -- 実際の実装では close() システムコールを両方のFDに対して呼び出す
  return $ Right ()

-- | 名前付きパイプ（FIFO）を作成
createNamedPipe :: FilePath -> IO (Either IPCError NamedPipe)
createNamedPipe path = do
  -- 実際の実装では mkfifo() システムコールを呼び出す
  return $ Right $ NamedPipe path 5

-- | 名前付きパイプを開く
openNamedPipe :: FilePath -> String -> IO (Either IPCError NamedPipe)
openNamedPipe path mode = do
  -- 実際の実装では open() システムコールを呼び出す
  return $ Right $ NamedPipe path 6

-- | 名前付きパイプを削除
deleteNamedPipe :: NamedPipe -> IO (Either IPCError ())
deleteNamedPipe namedPipe = do
  -- 実際の実装では unlink() システムコールを呼び出す
  return $ Right ()

-- | 共有メモリを作成
createSharedMemory :: CSize -> IO (Either IPCError SharedMemory)
createSharedMemory size = do
  -- 実際の実装では shmget() システムコールを呼び出す
  let shmId = 12345
      shmAddr = Ptr 0x10000000
  return $ Right $ SharedMemory shmId shmAddr size

-- | 共有メモリをプロセスにアタッチ
attachSharedMemory :: CInt -> IO (Either IPCError SharedMemory)
attachSharedMemory shmId = do
  -- 実際の実装では shmat() システムコールを呼び出す
  let shmAddr = Ptr 0x10000000
      shmSize = 4096
  return $ Right $ SharedMemory shmId shmAddr shmSize

-- | 共有メモリをプロセスからデタッチ
detachSharedMemory :: SharedMemory -> IO (Either IPCError ())
detachSharedMemory shm = do
  -- 実際の実装では shmdt() システムコールを呼び出す
  return $ Right ()

-- | 共有メモリを削除
removeSharedMemory :: SharedMemory -> IO (Either IPCError ())
removeSharedMemory shm = do
  -- 実際の実装では shmctl() システムコールを呼び出す
  return $ Right ()

-- | メッセージキューを作成
createMessageQueue :: IO (Either IPCError MessageQueue)
createMessageQueue = do
  -- 実際の実装では msgget() システムコールを呼び出す
  return $ Right $ MessageQueue 54321

-- | メッセージを送信
sendMessage :: MessageQueue -> Message -> IO (Either IPCError ())
sendMessage mq msg = do
  -- 実際の実装では msgsnd() システムコールを呼び出す
  return $ Right ()

-- | メッセージを受信
receiveMessage :: MessageQueue -> Int -> IO (Either IPCError Message)
receiveMessage mq msgType = do
  -- 実際の実装では msgrcv() システムコールを呼び出す
  let msg = Message msgType (BS8.pack "sample message")
  return $ Right msg

-- | メッセージキューを削除
deleteMessageQueue :: MessageQueue -> IO (Either IPCError ())
deleteMessageQueue mq = do
  -- 実際の実装では msgctl() システムコールを呼び出す
  return $ Right ()

-- | セマフォを作成
createSemaphore :: Int -> IO (Either IPCError Semaphore)
createSemaphore initialValue = do
  -- 実際の実装では semget() システムコールを呼び出す
  return $ Right $ Semaphore 98765

-- | セマフォを待機（P操作）
waitSemaphore :: Semaphore -> IO (Either IPCError ())
waitSemaphore sem = do
  -- 実際の実装では semop() システムコールを呼び出す
  return $ Right ()

-- | セマフォを解放（V操作）
postSemaphore :: Semaphore -> IO (Either IPCError ())
postSemaphore sem = do
  -- 実際の実装では semop() システムコールを呼び出す
  return $ Right ()

-- | セマフォを破棄
destroySemaphore :: Semaphore -> IO (Either IPCError ())
destroySemaphore sem = do
  -- 実際の実装では semctl() システムコールを呼び出す
  return $ Right ()

-- | ソケットペアを作成
createSocketPair :: IO (Either IPCError SocketPair)
createSocketPair = do
  -- 実際の実装では socketpair() システムコールを呼び出す
  return $ Right $ SocketPair 7 8

-- | ソケットペアでデータを送信
sendSocketData :: SocketPair -> CInt -> ByteString -> IO (Either IPCError Int)
sendSocketData socketPair fd dat = do
  -- 実際の実装では send() システムコールを呼び出す
  return $ Right $ BS8.length dat

-- | ソケットペアでデータを受信
receiveSocketData :: SocketPair -> CInt -> Int -> IO (Either IPCError ByteString)
receiveSocketData socketPair fd bufSize = do
  -- 実際の実装では recv() システムコールを呼び出す
  return $ Right $ BS8.pack "socket pair data"

-- | ソケットペアを閉じる
closeSocketPair :: SocketPair -> IO (Either IPCError ())
closeSocketPair socketPair = do
  -- 実際の実装では close() システムコールを両方のFDに対して呼び出す
  return $ Right ()

-- * 高レベルな通信パターン

-- | プロデューサー・コンシューマーパターン
producerConsumer :: IO ()
producerConsumer = do
  pipeResult <- createPipe
  case pipeResult of
    Left err -> putStrLn $ "パイプ作成エラー: " ++ show err
    Right pipe -> do
      -- プロデューサー
      _ <- writePipe pipe (BS8.pack "Hello from producer")
      -- コンシューマー
      dataResult <- readPipe pipe 1024
      case dataResult of
        Left err -> putStrLn $ "読み取りエラー: " ++ show err
        Right dat -> putStrLn $ "受信データ: " ++ BS8.unpack dat
      _ <- closePipe pipe
      return ()

-- | 共有メモリを使った通信例
sharedMemoryExample :: IO ()
sharedMemoryExample = do
  shmResult <- createSharedMemory 4096
  case shmResult of
    Left err -> putStrLn $ "共有メモリ作成エラー: " ++ show err
    Right shm -> do
      putStrLn "共有メモリを作成しました"
      -- 実際のアプリケーションでは、ここで共有メモリにデータを書き込み・読み取り
      _ <- detachSharedMemory shm
      _ <- removeSharedMemory shm
      putStrLn "共有メモリを削除しました"

-- | メッセージキューを使った通信例
messageQueueExample :: IO ()
messageQueueExample = do
  mqResult <- createMessageQueue
  case mqResult of
    Left err -> putStrLn $ "メッセージキュー作成エラー: " ++ show err
    Right mq -> do
      let msg = Message 1 (BS8.pack "Hello from message queue")
      _ <- sendMessage mq msg
      msgResult <- receiveMessage mq 1
      case msgResult of
        Left err -> putStrLn $ "メッセージ受信エラー: " ++ show err
        Right receivedMsg -> putStrLn $ "受信メッセージ: " ++ BS8.unpack (msgData receivedMsg)
      _ <- deleteMessageQueue mq
      return ()

-- | セマフォを使った同期例
semaphoreExample :: IO ()
semaphoreExample = do
  semResult <- createSemaphore 1
  case semResult of
    Left err -> putStrLn $ "セマフォ作成エラー: " ++ show err
    Right sem -> do
      putStrLn "クリティカルセクションに入ります"
      _ <- waitSemaphore sem  -- P操作
      putStrLn "クリティカルセクション内で作業中..."
      _ <- postSemaphore sem  -- V操作
      putStrLn "クリティカルセクションを出ました"
      _ <- destroySemaphore sem
      return ()