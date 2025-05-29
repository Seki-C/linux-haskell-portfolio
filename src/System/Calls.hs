{-|
Module      : System.Calls
Description : Linuxシステムコールの基本概念
Copyright   : (c) Portfolio, 2025
License     : MIT
Maintainer  : portfolio@example.com

Linuxシステムコールの基本概念と操作を学習するためのモジュールです。
ファイルI/O、プロセス制御、メモリ管理などのシステムコールについて理解できます。
-}

module System.Calls
  ( -- * ファイルI/Oシステムコール
    FileDescriptor(..)
  , open'
  , close'
  , read'
  , write'
  , lseek'
  , SeekMode(..)
    -- * プロセス制御システムコール
  , fork'
  , exec'
  , wait'
  , exit'
  , getpid'
  , getppid'
    -- * ファイルシステムシステムコール
  , stat'
  , chmod'
  , chown'
  , mkdir_sys
  , rmdir_sys
  , unlink'
    -- * ディレクトリ操作
  , opendir'
  , readdir'
  , closedir'
  , DirHandle(..)
  , DirEntry(..)
    -- * シグナル処理
  , signal'
  , kill_sys
  , alarm'
  , pause'
    -- * メモリ管理
  , mmap'
  , munmap'
  , brk'
  , sbrk'
  , MemoryProtection(..)
  , MapFlags(..)
    -- * ネットワークシステムコール
  , socket_sys
  , bind_sys
  , listen_sys
  , accept_sys
  , connect_sys
    -- * エラー処理
  , SystemCallError(..)
  , errno
  ) where

import Foreign.C.Types (CInt(..), CSize(..), COff(..))
import Foreign.Ptr (Ptr)
import Data.Word (Word64, Word32)
import Linux.Core (ProcessID, UserID, GroupID, FilePermissions)
import Process.Management (Signal)

-- | システムコールエラー型
data SystemCallError
  = EACCES    -- ^ Permission denied
  | EBADF     -- ^ Bad file descriptor
  | EEXIST    -- ^ File exists
  | ENOENT    -- ^ No such file or directory
  | ENOMEM    -- ^ Out of memory
  | EINVAL    -- ^ Invalid argument
  | EIO       -- ^ I/O error
  | EPERM     -- ^ Operation not permitted
  | ENOTDIR   -- ^ Not a directory
  | EISDIR    -- ^ Is a directory
  | EMFILE    -- ^ Too many open files
  | ENOSPC    -- ^ No space left on device
  | EROFS     -- ^ Read-only file system
  | CustomError Int String  -- ^ カスタムエラー
  deriving (Show, Eq)

-- | ファイルディスクリプタ
newtype FileDescriptor = FileDescriptor CInt deriving (Show, Eq)

-- | ディレクトリハンドル
newtype DirHandle = DirHandle (Ptr ()) deriving (Show, Eq)

-- | ディレクトリエントリ
data DirEntry = DirEntry
  { entryName :: String
  , entryInode :: Word64
  , entryType :: EntryType
  } deriving (Show, Eq)

-- | エントリタイプ
data EntryType
  = RegularFileEntry
  | DirectoryEntry
  | SymlinkEntry
  | BlockDeviceEntry
  | CharDeviceEntry
  | FifoEntry
  | SocketEntry
  | UnknownEntry
  deriving (Show, Eq)

-- | ファイルシーク位置
data SeekMode
  = SeekSet   -- ^ ファイル先頭からの絶対位置
  | SeekCur   -- ^ 現在位置からの相対位置
  | SeekEnd   -- ^ ファイル終端からの相対位置
  deriving (Show, Eq)

-- | メモリ保護フラグ
data MemoryProtection
  = ProtRead    -- ^ 読み取り可能
  | ProtWrite   -- ^ 書き込み可能
  | ProtExec    -- ^ 実行可能
  | ProtNone    -- ^ アクセス不可
  deriving (Show, Eq)

-- | メモリマップフラグ
data MapFlags
  = MapShared     -- ^ 共有マッピング
  | MapPrivate    -- ^ プライベートマッピング
  | MapFixed      -- ^ 固定アドレス
  | MapAnonymous  -- ^ 匿名マッピング
  deriving (Show, Eq)

-- | ファイルを開く（open システムコール）
open' :: FilePath -> [String] -> Maybe FilePermissions -> IO (Either SystemCallError FileDescriptor)
open' path flags perms = do
  -- 実際の実装では C の open() を呼び出す
  -- ここでは簡易的なシミュレーション
  let fd = 3  -- 仮のファイルディスクリプタ
  return $ Right (FileDescriptor (fromIntegral fd))

-- | ファイルを閉じる（close システムコール）
close' :: FileDescriptor -> IO (Either SystemCallError ())
close' (FileDescriptor fd) = do
  -- 実際の実装では C の close() を呼び出す
  return $ Right ()

-- | ファイルから読み取り（read システムコール）
read' :: FileDescriptor -> CSize -> IO (Either SystemCallError String)
read' (FileDescriptor fd) size = do
  -- 実際の実装では C の read() を呼び出す
  -- ここでは簡易的なシミュレーション
  return $ Right "sample data"

-- | ファイルに書き込み（write システムコール）
write' :: FileDescriptor -> String -> IO (Either SystemCallError CSize)
write' (FileDescriptor fd) content = do
  -- 実際の実装では C の write() を呼び出す
  -- ここでは簡易的なシミュレーション
  return $ Right (fromIntegral $ length content)

-- | ファイル位置を変更（lseek システムコール）
lseek' :: FileDescriptor -> COff -> SeekMode -> IO (Either SystemCallError COff)
lseek' (FileDescriptor fd) offset seekMode = do
  -- 実際の実装では C の lseek() を呼び出す
  -- ここでは簡易的なシミュレーション
  return $ Right offset

-- | プロセスを分岐（fork システムコール）
fork' :: IO (Either SystemCallError ProcessID)
fork' = do
  -- 実際の実装では C の fork() を呼び出す
  -- ここでは簡易的なシミュレーション
  -- 親プロセスでは子プロセスのPIDを返し、子プロセスでは0を返す
  return $ Right 1234  -- 仮の子プロセスPID

-- | プログラムを実行（exec システムコール）
exec' :: FilePath -> [String] -> IO (Either SystemCallError ())
exec' program args = do
  -- 実際の実装では C の execv() を呼び出す
  -- execは成功すると元のプロセスを置き換えるため、通常は戻らない
  return $ Right ()

-- | 子プロセスの終了を待機（wait システムコール）
wait' :: IO (Either SystemCallError (ProcessID, Int))
wait' = do
  -- 実際の実装では C の wait() を呼び出す
  -- ここでは簡易的なシミュレーション
  return $ Right (1234, 0)  -- (子プロセスPID, 終了ステータス)

-- | プロセス終了（exit システムコール）
exit' :: Int -> IO ()
exit' status = do
  -- 実際の実装では C の exit() を呼び出す
  -- このシステムコールは戻らない
  return ()

-- | 現在のプロセスIDを取得（getpid システムコール）
getpid' :: IO ProcessID
getpid' = do
  -- 実際の実装では C の getpid() を呼び出す
  return 1000  -- 仮のプロセスID

-- | 親プロセスIDを取得（getppid システムコール）
getppid' :: IO ProcessID
getppid' = do
  -- 実際の実装では C の getppid() を呼び出す
  return 999   -- 仮の親プロセスID

-- | ファイル情報を取得（stat システムコール）
stat' :: FilePath -> IO (Either SystemCallError (Word64, FilePermissions, UserID, GroupID))
stat' path = do
  -- 実際の実装では C の stat() を呼び出す
  -- ここでは簡易的なシミュレーション
  let defaultPerms = Linux.Core.FilePermissions True True False True False False True False False
  return $ Right (1024, defaultPerms, 1000, 1000)

-- | ファイル権限を変更（chmod システムコール）
chmod' :: FilePath -> FilePermissions -> IO (Either SystemCallError ())
chmod' path perms = do
  -- 実際の実装では C の chmod() を呼び出す
  return $ Right ()

-- | ファイル所有者を変更（chown システムコール）
chown' :: FilePath -> UserID -> GroupID -> IO (Either SystemCallError ())
chown' path uid gid = do
  -- 実際の実装では C の chown() を呼び出す
  return $ Right ()

-- | ディレクトリを作成（mkdir システムコール）
mkdir_sys :: FilePath -> FilePermissions -> IO (Either SystemCallError ())
mkdir_sys path perms = do
  -- 実際の実装では C の mkdir() を呼び出す
  return $ Right ()

-- | ディレクトリを削除（rmdir システムコール）
rmdir_sys :: FilePath -> IO (Either SystemCallError ())
rmdir_sys path = do
  -- 実際の実装では C の rmdir() を呼び出す
  return $ Right ()

-- | ファイルを削除（unlink システムコール）
unlink' :: FilePath -> IO (Either SystemCallError ())
unlink' path = do
  -- 実際の実装では C の unlink() を呼び出す
  return $ Right ()

-- | ディレクトリを開く（opendir システムコール）
opendir' :: FilePath -> IO (Either SystemCallError DirHandle)
opendir' path = do
  -- 実際の実装では C の opendir() を呼び出す
  return $ Right (DirHandle nullPtr)
  where nullPtr = Ptr 0

-- | ディレクトリエントリを読む（readdir システムコール）
readdir' :: DirHandle -> IO (Either SystemCallError (Maybe DirEntry))
readdir' (DirHandle handle) = do
  -- 実際の実装では C の readdir() を呼び出す
  -- ここでは簡易的なシミュレーション
  let entry = DirEntry "example.txt" 12345 RegularFileEntry
  return $ Right (Just entry)

-- | ディレクトリを閉じる（closedir システムコール）
closedir' :: DirHandle -> IO (Either SystemCallError ())
closedir' (DirHandle handle) = do
  -- 実際の実装では C の closedir() を呼び出す
  return $ Right ()

-- | シグナルハンドラを設定（signal システムコール）
signal' :: Signal -> (Signal -> IO ()) -> IO (Either SystemCallError ())
signal' sig handler = do
  -- 実際の実装では C の signal() を呼び出す
  return $ Right ()

-- | プロセスにシグナルを送信（kill システムコール）
kill_sys :: ProcessID -> Signal -> IO (Either SystemCallError ())
kill_sys pid sig = do
  -- 実際の実装では C の kill() を呼び出す
  return $ Right ()

-- | アラームを設定（alarm システムコール）
alarm' :: Word32 -> IO Word32
alarm' seconds = do
  -- 実際の実装では C の alarm() を呼び出す
  return 0  -- 前回設定されていたアラームの残り時間

-- | シグナル待機（pause システムコール）
pause' :: IO ()
pause' = do
  -- 実際の実装では C の pause() を呼び出す
  -- このシステムコールはシグナルを受信するまでブロックする
  return ()

-- | メモリマップ（mmap システムコール）
mmap' :: Ptr () -> CSize -> [MemoryProtection] -> [MapFlags] -> FileDescriptor -> COff -> IO (Either SystemCallError (Ptr ()))
mmap' addr len prots flags fd offset = do
  -- 実際の実装では C の mmap() を呼び出す
  return $ Right (Ptr 0x1000000)  -- 仮のアドレス

-- | メモリマップ解除（munmap システムコール）
munmap' :: Ptr () -> CSize -> IO (Either SystemCallError ())
munmap' addr len = do
  -- 実際の実装では C の munmap() を呼び出す
  return $ Right ()

-- | データセグメント終端を設定（brk システムコール）
brk' :: Ptr () -> IO (Either SystemCallError (Ptr ()))
brk' addr = do
  -- 実際の実装では C の brk() を呼び出す
  return $ Right addr

-- | データセグメント終端を変更（sbrk システムコール）
sbrk' :: Int -> IO (Either SystemCallError (Ptr ()))
sbrk' increment = do
  -- 実際の実装では C の sbrk() を呼び出す
  return $ Right (Ptr 0x2000000)  -- 仮のアドレス

-- | ソケット作成（socket システムコール）
socket_sys :: Int -> Int -> Int -> IO (Either SystemCallError FileDescriptor)
socket_sys domain sockType protocol = do
  -- 実際の実装では C の socket() を呼び出す
  return $ Right (FileDescriptor 4)  -- 仮のソケットファイルディスクリプタ

-- | ソケットバインド（bind システムコール）
bind_sys :: FileDescriptor -> String -> Int -> IO (Either SystemCallError ())
bind_sys (FileDescriptor fd) addr port = do
  -- 実際の実装では C の bind() を呼び出す
  return $ Right ()

-- | ソケット待ち受け（listen システムコール）
listen_sys :: FileDescriptor -> Int -> IO (Either SystemCallError ())
listen_sys (FileDescriptor fd) backlog = do
  -- 実際の実装では C の listen() を呼び出す
  return $ Right ()

-- | 接続受け入れ（accept システムコール）
accept_sys :: FileDescriptor -> IO (Either SystemCallError (FileDescriptor, String, Int))
accept_sys (FileDescriptor fd) = do
  -- 実際の実装では C の accept() を呼び出す
  return $ Right (FileDescriptor 5, "127.0.0.1", 12345)  -- (新しいソケット, クライアントIP, ポート)

-- | ソケット接続（connect システムコール）
connect_sys :: FileDescriptor -> String -> Int -> IO (Either SystemCallError ())
connect_sys (FileDescriptor fd) addr port = do
  -- 実際の実装では C の connect() を呼び出す
  return $ Right ()

-- | 最後のシステムコールエラー番号を取得
errno :: IO Int
errno = do
  -- 実際の実装では C の errno を読み取る
  return 0  -- エラーなし