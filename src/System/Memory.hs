{-|
Module      : System.Memory
Description : Linuxメモリ管理の基本概念
Copyright   : (c) Portfolio, 2025
License     : MIT
Maintainer  : portfolio@example.com

Linuxメモリ管理の基本概念と操作を学習するためのモジュールです。
仮想メモリ、物理メモリ、スワップ、メモリマッピングなどについて理解できます。
-}

module System.Memory
  ( -- * メモリ情報
    MemoryInfo(..)
  , SwapInfo(..)
  , ProcessMemory(..)
  , MemoryMap(..)
    -- * メモリ統計
  , getMemoryInfo
  , getSwapInfo
  , getProcessMemory
  , getMemoryMaps
    -- * メモリ操作
  , allocateMemory
  , freeMemory
  , reallocateMemory
  , protectMemory
    -- * 仮想メモリ
  , VirtualAddress(..)
  , PhysicalAddress(..)
  , PageSize(..)
  , translateAddress
  , getPageSize
    -- * メモリマッピング
  , MemoryMapping(..)
  , mapMemory
  , unmapMemory
  , syncMemory
  , lockMemory
  , unlockMemory
    -- * メモリプール
  , MemoryPool(..)
  , createMemoryPool
  , allocateFromPool
  , deallocateFromPool
  , destroyMemoryPool
    -- * メモリ監視
  , MemoryUsage(..)
  , monitorMemoryUsage
  , detectMemoryLeaks
  , getMemoryPressure
    -- * エラー処理
  , MemoryError(..)
  ) where

import Foreign.Ptr (Ptr, nullPtr, plusPtr)
import Foreign.C.Types (CSize(..), CInt(..))
import Data.Word (Word64, Word32)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Exception (Exception)
import Linux.Core (ProcessID)

-- | メモリ管理のエラー型
data MemoryError
  = OutOfMemory String
  | InvalidAddress String
  | PermissionDenied String
  | AddressInUse String
  | InvalidSize String
  | MappingFailed String
  | AllocationFailed String
  | IOError String
  deriving (Show, Eq)

instance Exception MemoryError

-- | システムメモリ情報
data MemoryInfo = MemoryInfo
  { totalMemory     :: Word64   -- ^ 総メモリ量（バイト）
  , availableMemory :: Word64   -- ^ 利用可能メモリ量（バイト）
  , freeMemory      :: Word64   -- ^ 空きメモリ量（バイト）
  , usedMemory      :: Word64   -- ^ 使用中メモリ量（バイト）
  , bufferMemory    :: Word64   -- ^ バッファメモリ量（バイト）
  , cacheMemory     :: Word64   -- ^ キャッシュメモリ量（バイト）
  , sharedMemory    :: Word64   -- ^ 共有メモリ量（バイト）
  } deriving (Show, Eq)

-- | スワップ情報
data SwapInfo = SwapInfo
  { totalSwap :: Word64         -- ^ 総スワップ量（バイト）
  , usedSwap  :: Word64         -- ^ 使用中スワップ量（バイト）
  , freeSwap  :: Word64         -- ^ 空きスワップ量（バイト）
  } deriving (Show, Eq)

-- | プロセスメモリ情報
data ProcessMemory = ProcessMemory
  { processId       :: ProcessID
  , virtualSize     :: Word64   -- ^ 仮想メモリサイズ
  , residentSize    :: Word64   -- ^ 常駐メモリサイズ
  , sharedSize      :: Word64   -- ^ 共有メモリサイズ
  , textSize        :: Word64   -- ^ テキストセグメントサイズ
  , dataSize        :: Word64   -- ^ データセグメントサイズ
  , stackSize       :: Word64   -- ^ スタックサイズ
  , heapSize        :: Word64   -- ^ ヒープサイズ
  } deriving (Show, Eq)

-- | メモリマップ情報
data MemoryMap = MemoryMap
  { mapStart       :: VirtualAddress  -- ^ 開始仮想アドレス
  , mapEnd         :: VirtualAddress  -- ^ 終了仮想アドレス
  , mapPermissions :: String          -- ^ 権限（rwx-）
  , mapOffset      :: Word64          -- ^ オフセット
  , mapDevice      :: String          -- ^ デバイス
  , mapInode       :: Word64          -- ^ inode番号
  , mapPath        :: Maybe FilePath  -- ^ ファイルパス
  } deriving (Show, Eq)

-- | 仮想アドレス
newtype VirtualAddress = VirtualAddress Word64 deriving (Show, Eq, Ord)

-- | 物理アドレス
newtype PhysicalAddress = PhysicalAddress Word64 deriving (Show, Eq, Ord)

-- | ページサイズ
newtype PageSize = PageSize Word64 deriving (Show, Eq)

-- | メモリマッピング
data MemoryMapping = MemoryMapping
  { mappingAddress :: Ptr ()      -- ^ マッピングアドレス
  , mappingSize    :: CSize       -- ^ マッピングサイズ
  , mappingProt    :: [String]    -- ^ 保護フラグ
  , mappingFlags   :: [String]    -- ^ マッピングフラグ
  } deriving (Show, Eq)

-- | メモリプール
data MemoryPool = MemoryPool
  { poolAddress    :: Ptr ()      -- ^ プールのベースアドレス
  , poolSize       :: CSize       -- ^ プールサイズ
  , poolUsed       :: CSize       -- ^ 使用済みサイズ
  , poolBlocks     :: Map Word64 CSize  -- ^ 割り当て済みブロック
  } deriving (Show, Eq)

-- | メモリ使用状況
data MemoryUsage = MemoryUsage
  { usageTimestamp :: Word64      -- ^ タイムスタンプ
  , usageTotal     :: Word64      -- ^ 総使用量
  , usageResident  :: Word64      -- ^ 常駐使用量
  , usageShared    :: Word64      -- ^ 共有使用量
  , usageGrowth    :: Double      -- ^ 増加率
  } deriving (Show, Eq)

-- | システムメモリ情報を取得
getMemoryInfo :: IO (Either MemoryError MemoryInfo)
getMemoryInfo = do
  -- 実際の実装では /proc/meminfo を読み取る
  let memInfo = MemoryInfo
        { totalMemory = 8 * 1024 * 1024 * 1024      -- 8GB
        , availableMemory = 4 * 1024 * 1024 * 1024  -- 4GB
        , freeMemory = 2 * 1024 * 1024 * 1024       -- 2GB
        , usedMemory = 4 * 1024 * 1024 * 1024       -- 4GB
        , bufferMemory = 512 * 1024 * 1024          -- 512MB
        , cacheMemory = 1024 * 1024 * 1024          -- 1GB
        , sharedMemory = 256 * 1024 * 1024          -- 256MB
        }
  return $ Right memInfo

-- | スワップ情報を取得
getSwapInfo :: IO (Either MemoryError SwapInfo)
getSwapInfo = do
  -- 実際の実装では /proc/swaps や /proc/meminfo を読み取る
  let swapInfo = SwapInfo
        { totalSwap = 2 * 1024 * 1024 * 1024  -- 2GB
        , usedSwap = 512 * 1024 * 1024        -- 512MB
        , freeSwap = 1536 * 1024 * 1024       -- 1.5GB
        }
  return $ Right swapInfo

-- | プロセスメモリ情報を取得
getProcessMemory :: ProcessID -> IO (Either MemoryError ProcessMemory)
getProcessMemory pid = do
  -- 実際の実装では /proc/[pid]/status や /proc/[pid]/statm を読み取る
  let procMem = ProcessMemory
        { processId = pid
        , virtualSize = 100 * 1024 * 1024   -- 100MB
        , residentSize = 50 * 1024 * 1024   -- 50MB
        , sharedSize = 10 * 1024 * 1024     -- 10MB
        , textSize = 5 * 1024 * 1024        -- 5MB
        , dataSize = 20 * 1024 * 1024       -- 20MB
        , stackSize = 8 * 1024 * 1024       -- 8MB
        , heapSize = 25 * 1024 * 1024       -- 25MB
        }
  return $ Right procMem

-- | プロセスのメモリマップを取得
getMemoryMaps :: ProcessID -> IO (Either MemoryError [MemoryMap])
getMemoryMaps pid = do
  -- 実際の実装では /proc/[pid]/maps を読み取る
  let maps = 
        [ MemoryMap (VirtualAddress 0x400000) (VirtualAddress 0x401000) "r-xp" 0 "08:01" 12345 (Just "/usr/bin/example")
        , MemoryMap (VirtualAddress 0x600000) (VirtualAddress 0x601000) "rw-p" 0 "08:01" 12345 (Just "/usr/bin/example")
        , MemoryMap (VirtualAddress 0x7fff00000000) (VirtualAddress 0x7fff00021000) "rw-p" 0 "00:00" 0 Nothing
        ]
  return $ Right maps

-- | メモリを割り当て
allocateMemory :: CSize -> IO (Either MemoryError (Ptr ()))
allocateMemory size = do
  -- 実際の実装では malloc() や mmap() を呼び出す
  if size == 0
    then return $ Left (InvalidSize "サイズは0より大きくする必要があります")
    else return $ Right $ Ptr 0x1000000  -- 仮のアドレス

-- | メモリを解放
freeMemory :: Ptr () -> IO (Either MemoryError ())
freeMemory ptr = do
  -- 実際の実装では free() や munmap() を呼び出す
  if ptr == nullPtr
    then return $ Left (InvalidAddress "NULL ポインタは解放できません")
    else return $ Right ()

-- | メモリを再割り当て
reallocateMemory :: Ptr () -> CSize -> IO (Either MemoryError (Ptr ()))
reallocateMemory ptr newSize = do
  -- 実際の実装では realloc() を呼び出す
  if newSize == 0
    then return $ Left (InvalidSize "サイズは0より大きくする必要があります")
    else return $ Right ptr

-- | メモリ保護を設定
protectMemory :: Ptr () -> CSize -> [String] -> IO (Either MemoryError ())
protectMemory ptr size prots = do
  -- 実際の実装では mprotect() を呼び出す
  return $ Right ()

-- | 仮想アドレスを物理アドレスに変換
translateAddress :: VirtualAddress -> IO (Either MemoryError PhysicalAddress)
translateAddress (VirtualAddress vaddr) = do
  -- 実際の実装では /proc/[pid]/pagemap を読み取る
  return $ Right $ PhysicalAddress (vaddr + 0x1000000)  -- 仮の変換

-- | システムのページサイズを取得
getPageSize :: IO PageSize
getPageSize = do
  -- 実際の実装では sysconf(_SC_PAGESIZE) を呼び出す
  return $ PageSize 4096  -- 一般的な4KBページサイズ

-- | メモリをマップ
mapMemory :: CSize -> [String] -> [String] -> IO (Either MemoryError MemoryMapping)
mapMemory size prots flags = do
  -- 実際の実装では mmap() を呼び出す
  let mapping = MemoryMapping
        { mappingAddress = Ptr 0x2000000
        , mappingSize = size
        , mappingProt = prots
        , mappingFlags = flags
        }
  return $ Right mapping

-- | メモリマッピングを解除
unmapMemory :: MemoryMapping -> IO (Either MemoryError ())
unmapMemory mapping = do
  -- 実際の実装では munmap() を呼び出す
  return $ Right ()

-- | メモリマッピングを同期
syncMemory :: MemoryMapping -> IO (Either MemoryError ())
syncMemory mapping = do
  -- 実際の実装では msync() を呼び出す
  return $ Right ()

-- | メモリをロック（スワップアウト禁止）
lockMemory :: Ptr () -> CSize -> IO (Either MemoryError ())
lockMemory ptr size = do
  -- 実際の実装では mlock() を呼び出す
  return $ Right ()

-- | メモリロックを解除
unlockMemory :: Ptr () -> CSize -> IO (Either MemoryError ())
unlockMemory ptr size = do
  -- 実際の実装では munlock() を呼び出す
  return $ Right ()

-- | メモリプールを作成
createMemoryPool :: CSize -> IO (Either MemoryError MemoryPool)
createMemoryPool size = do
  allocResult <- allocateMemory size
  case allocResult of
    Left err -> return $ Left err
    Right ptr -> do
      let pool = MemoryPool
            { poolAddress = ptr
            , poolSize = size
            , poolUsed = 0
            , poolBlocks = Map.empty
            }
      return $ Right pool

-- | メモリプールから割り当て
allocateFromPool :: MemoryPool -> CSize -> IO (Either MemoryError (MemoryPool, Ptr ()))
allocateFromPool pool size = do
  if poolUsed pool + size > poolSize pool
    then return $ Left (OutOfMemory "プールの容量不足")
    else do
      let newOffset = poolUsed pool
          newPtr = poolAddress pool `plusPtr` fromIntegral newOffset
          newBlocks = Map.insert (fromIntegral newOffset) size (poolBlocks pool)
          newPool = pool { poolUsed = poolUsed pool + size, poolBlocks = newBlocks }
      return $ Right (newPool, newPtr)

-- | メモリプールから解放
deallocateFromPool :: MemoryPool -> Ptr () -> IO (Either MemoryError MemoryPool)
deallocateFromPool pool ptr = do
  -- 簡易実装：実際にはフラグメンテーション管理が必要
  return $ Right pool

-- | メモリプールを破棄
destroyMemoryPool :: MemoryPool -> IO (Either MemoryError ())
destroyMemoryPool pool = freeMemory (poolAddress pool)

-- | メモリ使用量を監視
monitorMemoryUsage :: ProcessID -> IO (Either MemoryError MemoryUsage)
monitorMemoryUsage pid = do
  memResult <- getProcessMemory pid
  case memResult of
    Left err -> return $ Left err
    Right procMem -> do
      let usage = MemoryUsage
            { usageTimestamp = 1640995200  -- 仮のタイムスタンプ
            , usageTotal = virtualSize procMem
            , usageResident = residentSize procMem
            , usageShared = sharedSize procMem
            , usageGrowth = 0.5  -- 0.5% 増加
            }
      return $ Right usage

-- | メモリリークを検出
detectMemoryLeaks :: ProcessID -> IO (Either MemoryError [String])
detectMemoryLeaks pid = do
  -- 実際の実装では valgrind や AddressSanitizer のような仕組みを使用
  let leaks = ["0x1234567: 100 bytes leaked at function foo()"
              ,"0x2345678: 50 bytes leaked at function bar()"]
  return $ Right leaks

-- | メモリプレッシャーを取得
getMemoryPressure :: IO (Either MemoryError Double)
getMemoryPressure = do
  memResult <- getMemoryInfo
  case memResult of
    Left err -> return $ Left err
    Right memInfo -> do
      let pressure = fromIntegral (usedMemory memInfo) / fromIntegral (totalMemory memInfo) * 100
      return $ Right pressure

-- * メモリ最適化とデバッグ機能

-- | メモリフラグメンテーション情報
data FragmentationInfo = FragmentationInfo
  { largestFreeBlock :: Word64    -- ^ 最大空きブロックサイズ
  , fragmentationRatio :: Double  -- ^ フラグメンテーション率
  , freeBlockCount :: Int         -- ^ 空きブロック数
  } deriving (Show, Eq)

-- | フラグメンテーション情報を取得
getFragmentationInfo :: IO (Either MemoryError FragmentationInfo)
getFragmentationInfo = do
  -- 実際の実装では /proc/buddyinfo などを読み取る
  let fragInfo = FragmentationInfo
        { largestFreeBlock = 64 * 1024 * 1024  -- 64MB
        , fragmentationRatio = 15.5            -- 15.5%
        , freeBlockCount = 1024                -- 1024ブロック
        }
  return $ Right fragInfo

-- | メモリ統計情報
data MemoryStats = MemoryStats
  { allocationsCount :: Word64    -- ^ 総割り当て回数
  , deallocationsCount :: Word64  -- ^ 総解放回数
  , bytesAllocated :: Word64      -- ^ 総割り当てバイト数
  , bytesDeallocated :: Word64    -- ^ 総解放バイト数
  , peakUsage :: Word64           -- ^ ピーク使用量
  } deriving (Show, Eq)

-- | メモリ統計情報を取得
getMemoryStats :: ProcessID -> IO (Either MemoryError MemoryStats)
getMemoryStats pid = do
  let stats = MemoryStats
        { allocationsCount = 12345
        , deallocationsCount = 12340
        , bytesAllocated = 1024 * 1024 * 1024  -- 1GB
        , bytesDeallocated = 1020 * 1024 * 1024 -- 1020MB
        , peakUsage = 512 * 1024 * 1024        -- 512MB
        }
  return $ Right stats