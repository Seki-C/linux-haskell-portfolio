{-|
Module      : Process.Management
Description : Linuxプロセス管理の基本操作
Copyright   : (c) Portfolio, 2025
License     : MIT
Maintainer  : portfolio@example.com

Linuxプロセス管理の基本的な概念と操作を学習するためのモジュールです。
プロセスの作成、監視、制御について理解できます。
-}

module Process.Management
  ( -- * プロセス情報
    ProcessInfo(..)
  , ProcessList
  , Signal(..)
    -- * プロセス操作
  , listProcesses
  , getProcessInfo
  , createProcess'
  , killProcess
  , sendSignal
  , waitForProcess'
    -- * プロセス監視
  , monitorProcess
  , getProcessChildren
  , getProcessParent
  , isProcessRunning
    -- * プロセス統計
  , getProcessStats
  , ProcessStats(..)
  , getCpuUsage
  , getMemoryUsage
    -- * プロセス制御
  , pauseProcess
  , resumeProcess
  , setProcessPriority
    -- * エラー処理
  , ProcessError(..)
  ) where

import Linux.Core (ProcessID, Process(..), ProcessState(..), UserID)
import qualified System.Process as Proc
import System.Exit (ExitCode(..))
import Control.Exception (Exception, try, IOException)
import Data.Time (UTCTime, getCurrentTime)
import Data.Word (Word64)

-- | プロセス管理のエラー型
data ProcessError
  = ProcessNotFound ProcessID
  | ProcessAccessDenied ProcessID
  | ProcessAlreadyExists String
  | InvalidSignal String
  | ProcessCreationFailed String
  | IOError String
  deriving (Show, Eq)

instance Exception ProcessError

-- | 詳細なプロセス情報
data ProcessInfo = ProcessInfo
  { procId          :: ProcessID
  , procParent      :: Maybe ProcessID
  , procName        :: String
  , procCommand     :: String
  , procState       :: ProcessState
  , procOwner       :: UserID
  , procStartTime   :: UTCTime
  , procCpuTime     :: Word64        -- CPU使用時間（ミリ秒）
  , procMemoryUsage :: Word64        -- メモリ使用量（KB）
  , procPriority    :: Int           -- プロセス優先度
  } deriving (Show, Eq)

-- | プロセスリスト
type ProcessList = [ProcessInfo]

-- | Linuxシグナル
data Signal
  = SIGHUP     -- ^ ハングアップ
  | SIGINT     -- ^ 割り込み（Ctrl+C）
  | SIGQUIT    -- ^ 終了（Ctrl+\）
  | SIGILL     -- ^ 不正命令
  | SIGABRT    -- ^ アボート
  | SIGFPE     -- ^ 浮動小数点例外
  | SIGKILL    -- ^ 強制終了
  | SIGSEGV    -- ^ セグメンテーション違反
  | SIGPIPE    -- ^ パイプ破壊
  | SIGALRM    -- ^ アラーム
  | SIGTERM    -- ^ 終了要求
  | SIGUSR1    -- ^ ユーザー定義1
  | SIGUSR2    -- ^ ユーザー定義2
  | SIGCHLD    -- ^ 子プロセス状態変化
  | SIGCONT    -- ^ 実行継続
  | SIGSTOP    -- ^ 停止
  | SIGTSTP    -- ^ 端末停止（Ctrl+Z）
  deriving (Show, Eq, Enum, Bounded)

-- | プロセス統計情報
data ProcessStats = ProcessStats
  { statsCpuPercent    :: Double     -- ^ CPU使用率（%）
  , statsMemoryPercent :: Double     -- ^ メモリ使用率（%）
  , statsRuntime       :: Word64     -- ^ 実行時間（秒）
  , statsFileHandles   :: Int        -- ^ オープンファイル数
  , statsThreads       :: Int        -- ^ スレッド数
  } deriving (Show, Eq)

-- | 現在実行中のプロセス一覧を取得（簡易版）
listProcesses :: IO (Either ProcessError ProcessList)
listProcesses = do
  currentTime <- getCurrentTime
  -- 実際の実装では /proc ディレクトリを読み取る
  -- ここでは簡易的なサンプルデータを返す
  let sampleProcesses = 
        [ ProcessInfo 1 Nothing "init" "/sbin/init" Running 0 currentTime 100 1024 0
        , ProcessInfo 2 (Just 1) "kthreadd" "[kthreadd]" Running 0 currentTime 50 512 0
        , ProcessInfo 100 (Just 1) "systemd" "/lib/systemd/systemd --user" Running 1000 currentTime 200 2048 0
        , ProcessInfo 1234 (Just 100) "bash" "/bin/bash" Running 1000 currentTime 150 1536 0
        ]
  return $ Right sampleProcesses

-- | 特定のプロセス情報を取得
getProcessInfo :: ProcessID -> IO (Either ProcessError ProcessInfo)
getProcessInfo pid = do
  processList <- listProcesses
  case processList of
    Left err -> return $ Left err
    Right procs -> 
      case filter (\p -> procId p == pid) procs of
        [] -> return $ Left (ProcessNotFound pid)
        (proc:_) -> return $ Right proc

-- | 新しいプロセスを作成
createProcess' :: String -> [String] -> IO (Either ProcessError ProcessID)
createProcess' command args = do
  result <- try $ Proc.spawnProcess command args
  case result of
    Left err -> return $ Left (ProcessCreationFailed $ show (err :: IOException))
    Right procHandle -> do
      -- 実際の実装では ProcessHandle からPIDを取得
      -- ここでは簡易的にランダムなPIDを返す
      return $ Right 9999

-- | プロセスを終了
killProcess :: ProcessID -> IO (Either ProcessError ())
killProcess pid = do
  result <- try $ Proc.terminateProcess =<< Proc.spawnProcess "kill" [show pid]
  case result of
    Left err -> return $ Left (IOError $ show (err :: IOException))
    Right _ -> return $ Right ()

-- | プロセスにシグナルを送信
sendSignal :: ProcessID -> Signal -> IO (Either ProcessError ())
sendSignal pid signal = do
  let signalNum = case signal of
        SIGHUP  -> "1"
        SIGINT  -> "2"
        SIGQUIT -> "3"
        SIGKILL -> "9"
        SIGTERM -> "15"
        SIGSTOP -> "19"
        SIGCONT -> "18"
        _ -> "15"  -- デフォルトはSIGTERM
  
  result <- try $ Proc.spawnProcess "kill" ["-" ++ signalNum, show pid]
  case result of
    Left err -> return $ Left (IOError $ show (err :: IOException))
    Right _ -> return $ Right ()

-- | プロセスの終了を待機
waitForProcess' :: ProcessID -> IO (Either ProcessError ExitCode)
waitForProcess' pid = do
  -- 実際の実装では waitpid システムコールを使用
  -- ここでは簡易的にExitSuccessを返す
  return $ Right ExitSuccess

-- | プロセスを監視（状態変化を検出）
monitorProcess :: ProcessID -> IO (Either ProcessError ProcessState)
monitorProcess pid = do
  processInfo <- getProcessInfo pid
  case processInfo of
    Left err -> return $ Left err
    Right info -> return $ Right (procState info)

-- | プロセスの子プロセス一覧を取得
getProcessChildren :: ProcessID -> IO (Either ProcessError [ProcessID])
getProcessChildren parentPid = do
  processList <- listProcesses
  case processList of
    Left err -> return $ Left err
    Right procs -> 
      let children = [procId p | p <- procs, procParent p == Just parentPid]
      in return $ Right children

-- | プロセスの親プロセスを取得
getProcessParent :: ProcessID -> IO (Either ProcessError (Maybe ProcessID))
getProcessParent pid = do
  processInfo <- getProcessInfo pid
  case processInfo of
    Left err -> return $ Left err
    Right info -> return $ Right (procParent info)

-- | プロセスが実行中かチェック
isProcessRunning :: ProcessID -> IO Bool
isProcessRunning pid = do
  processInfo <- getProcessInfo pid
  case processInfo of
    Left _ -> return False
    Right info -> return $ procState info == Running

-- | プロセス統計情報を取得
getProcessStats :: ProcessID -> IO (Either ProcessError ProcessStats)
getProcessStats pid = do
  processInfo <- getProcessInfo pid
  case processInfo of
    Left err -> return $ Left err
    Right info -> 
      let stats = ProcessStats
            { statsCpuPercent = 5.5      -- サンプル値
            , statsMemoryPercent = 2.1   -- サンプル値
            , statsRuntime = 3600        -- サンプル値
            , statsFileHandles = 10      -- サンプル値
            , statsThreads = 1           -- サンプル値
            }
      in return $ Right stats

-- | CPU使用率を取得
getCpuUsage :: ProcessID -> IO (Either ProcessError Double)
getCpuUsage pid = do
  stats <- getProcessStats pid
  case stats of
    Left err -> return $ Left err
    Right s -> return $ Right (statsCpuPercent s)

-- | メモリ使用量を取得
getMemoryUsage :: ProcessID -> IO (Either ProcessError Word64)
getMemoryUsage pid = do
  processInfo <- getProcessInfo pid
  case processInfo of
    Left err -> return $ Left err
    Right info -> return $ Right (procMemoryUsage info)

-- | プロセスを一時停止
pauseProcess :: ProcessID -> IO (Either ProcessError ())
pauseProcess pid = sendSignal pid SIGSTOP

-- | プロセスを再開
resumeProcess :: ProcessID -> IO (Either ProcessError ())
resumeProcess pid = sendSignal pid SIGCONT

-- | プロセス優先度を設定
setProcessPriority :: ProcessID -> Int -> IO (Either ProcessError ())
setProcessPriority pid priority = do
  result <- try $ Proc.spawnProcess "renice" [show priority, show pid]
  case result of
    Left err -> return $ Left (IOError $ show (err :: IOException))
    Right _ -> return $ Right ()