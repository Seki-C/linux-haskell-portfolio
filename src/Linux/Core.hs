{-|
Module      : Linux.Core
Description : Linux OS の基本概念と定義
Copyright   : (c) Portfolio, 2025
License     : MIT
Maintainer  : portfolio@example.com

Linux オペレーティングシステムの基本的な概念と型定義を提供します。
-}

module Linux.Core
  ( -- * ユーザー管理
    User(..)
  , Group(..)
  , UserID
  , GroupID
    -- * ファイルシステム
  , FilePermissions(..)
  , FileType(..)
  , FileStat(..)
    -- * プロセス管理
  , ProcessID
  , Process(..)
  , ProcessState(..)
    -- * システム情報
  , SystemInfo(..)
  , MemoryInfo(..)
    -- * 便利な関数
  , showPermissions
  , isExecutable
  , isDirectory
  ) where

import Data.Time (UTCTime)
import Data.Word (Word64)

-- | ユーザーID（通常は数値）
type UserID = Int

-- | グループID（通常は数値）
type GroupID = Int

-- | プロセスID
type ProcessID = Int

-- | システムユーザーの情報
data User = User
  { userId       :: UserID      -- ^ ユーザーID
  , userName     :: String      -- ^ ユーザー名
  , userGroup    :: GroupID     -- ^ 主グループID
  , userHome     :: FilePath    -- ^ ホームディレクトリ
  , userShell    :: FilePath    -- ^ デフォルトシェル
  } deriving (Show, Eq)

-- | グループの情報
data Group = Group
  { groupId      :: GroupID     -- ^ グループID
  , groupName    :: String      -- ^ グループ名
  , groupMembers :: [UserID]    -- ^ グループメンバー
  } deriving (Show, Eq)

-- | ファイルの権限
data FilePermissions = FilePermissions
  { ownerRead    :: Bool        -- ^ 所有者読み取り権限
  , ownerWrite   :: Bool        -- ^ 所有者書き込み権限
  , ownerExecute :: Bool        -- ^ 所有者実行権限
  , groupRead    :: Bool        -- ^ グループ読み取り権限
  , groupWrite   :: Bool        -- ^ グループ書き込み権限
  , groupExecute :: Bool        -- ^ グループ実行権限
  , otherRead    :: Bool        -- ^ その他読み取り権限
  , otherWrite   :: Bool        -- ^ その他書き込み権限
  , otherExecute :: Bool        -- ^ その他実行権限
  } deriving (Show, Eq)

-- | ファイルの種類
data FileType 
  = RegularFile                 -- ^ 通常ファイル
  | Directory                   -- ^ ディレクトリ
  | SymbolicLink                -- ^ シンボリックリンク
  | BlockDevice                 -- ^ ブロックデバイス
  | CharacterDevice             -- ^ キャラクターデバイス
  | NamedPipe                   -- ^ 名前付きパイプ
  | Socket                      -- ^ ソケット
  deriving (Show, Eq)

-- | ファイルの統計情報
data FileStat = FileStat
  { fileType        :: FileType
  , fileSize        :: Word64
  , filePermissions :: FilePermissions
  , fileOwner       :: UserID
  , fileGroup       :: GroupID
  , fileModified    :: UTCTime
  , fileAccessed    :: UTCTime
  } deriving (Show, Eq)

-- | プロセスの状態
data ProcessState
  = Running                     -- ^ 実行中
  | Sleeping                    -- ^ 待機中
  | Stopped                     -- ^ 停止中
  | Zombie                      -- ^ ゾンビプロセス
  | Uninterruptible             -- ^ 割り込み不可能な待機
  deriving (Show, Eq)

-- | プロセスの情報
data Process = Process
  { processId       :: ProcessID
  , parentProcessId :: Maybe ProcessID
  , processName     :: String
  , processState    :: ProcessState
  , processOwner    :: UserID
  , processCommand  :: String
  } deriving (Show, Eq)

-- | システム情報
data SystemInfo = SystemInfo
  { systemKernel    :: String   -- ^ カーネルバージョン
  , systemUptime    :: Word64   -- ^ 稼働時間（秒）
  , systemLoad      :: (Double, Double, Double) -- ^ ロードアベレージ
  , systemUsers     :: Int      -- ^ ログインユーザー数
  } deriving (Show, Eq)

-- | メモリ情報
data MemoryInfo = MemoryInfo
  { totalMemory     :: Word64   -- ^ 総メモリ量
  , freeMemory      :: Word64   -- ^ 空きメモリ量
  , usedMemory      :: Word64   -- ^ 使用メモリ量
  , bufferMemory    :: Word64   -- ^ バッファメモリ量
  , cacheMemory     :: Word64   -- ^ キャッシュメモリ量
  } deriving (Show, Eq)

-- | ファイル権限を文字列形式で表示
-- 例: "rwxr-xr--"
showPermissions :: FilePermissions -> String
showPermissions perms = 
  [if ownerRead perms then 'r' else '-'] ++
  [if ownerWrite perms then 'w' else '-'] ++
  [if ownerExecute perms then 'x' else '-'] ++
  [if groupRead perms then 'r' else '-'] ++
  [if groupWrite perms then 'w' else '-'] ++
  [if groupExecute perms then 'x' else '-'] ++
  [if otherRead perms then 'r' else '-'] ++
  [if otherWrite perms then 'w' else '-'] ++
  [if otherExecute perms then 'x' else '-']

-- | ファイルが実行可能かどうかチェック
isExecutable :: FilePermissions -> Bool
isExecutable perms = ownerExecute perms || groupExecute perms || otherExecute perms

-- | ファイルタイプがディレクトリかどうかチェック
isDirectory :: FileType -> Bool
isDirectory Directory = True
isDirectory _         = False