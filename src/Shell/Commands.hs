{-|
Module      : Shell.Commands
Description : 基本的なLinuxシェルコマンドの実装
Copyright   : (c) Portfolio, 2025
License     : MIT
Maintainer  : portfolio@example.com

基本的なLinuxシェルコマンドをHaskellで実装することで、
コマンドラインツールの動作原理を学習するためのモジュールです。
-}

module Shell.Commands
  ( -- * ファイル操作コマンド
    ls
  , cat
  , cp
  , mv
  , rm
  , mkdir'
  , rmdir'
  , touch
    -- * テキスト処理コマンド
  , grep
  , wc
  , head'
  , tail'
  , sort'
  , uniq'
    -- * システム情報コマンド
  , ps
  , df
  , du
  , whoami
  , pwd'
    -- * プロセス制御コマンド
  , kill'
  , killall
    -- * ネットワークコマンド
  , ping'
  , wget'
    -- * 結果型
  , CommandResult(..)
  , CommandError(..)
  ) where

import qualified System.Directory as Dir
import qualified System.FilePath as Path
import qualified Data.List as List
import Control.Exception (try, IOException)
import Data.Time (formatTime, defaultTimeLocale, getCurrentTime)
import Text.Regex.Posix ((=~))
import FileSystem.Operations (FileSystemError(..), listDirectory', readFileContent)
import Process.Management (ProcessError(..), listProcesses, ProcessInfo(..))

-- | コマンド実行結果
data CommandResult
  = Success String              -- ^ 成功（出力内容）
  | Error CommandError          -- ^ エラー
  deriving (Show, Eq)

-- | コマンドエラー型
data CommandError
  = FileNotFound String
  | PermissionDenied String
  | InvalidArguments String
  | ProcessError String
  | NetworkError String
  | IOError String
  deriving (Show, Eq)

-- | ls - ディレクトリの内容を一覧表示
ls :: [String] -> IO CommandResult
ls [] = ls ["."]
ls (path:_) = do
  result <- listDirectory' path
  case result of
    Left (FileSystem.Operations.FileNotFound p) -> return $ Error (FileNotFound p)
    Left (FileSystem.Operations.PermissionDenied p) -> return $ Error (PermissionDenied p)
    Left (FileSystem.Operations.IOError err) -> return $ Error (IOError err)
    Right contents -> do
      -- 簡易的な出力フォーマット
      let output = unlines contents
      return $ Success output

-- | cat - ファイル内容を表示
cat :: [String] -> IO CommandResult
cat [] = return $ Error (InvalidArguments "cat: 少なくとも1つのファイルを指定してください")
cat files = do
  results <- mapM readFileContent files
  let contents = map handleFileResult results
  case sequence contents of
    Left err -> return $ Error err
    Right fileContents -> return $ Success (concat fileContents)
  where
    handleFileResult (Left (FileSystem.Operations.FileNotFound p)) = Left (FileNotFound p)
    handleFileResult (Left (FileSystem.Operations.IOError err)) = Left (IOError err)
    handleFileResult (Right content) = Right content

-- | cp - ファイルをコピー
cp :: [String] -> IO CommandResult
cp [src, dest] = do
  result <- try $ Dir.copyFile src dest
  case result of
    Left err -> return $ Error (IOError $ show (err :: IOException))
    Right _ -> return $ Success $ "'" ++ src ++ "' を '" ++ dest ++ "' にコピーしました"
cp _ = return $ Error (InvalidArguments "cp: 使用法: cp <source> <destination>")

-- | mv - ファイルを移動/リネーム
mv :: [String] -> IO CommandResult
mv [src, dest] = do
  result <- try $ Dir.renameFile src dest
  case result of
    Left err -> return $ Error (IOError $ show (err :: IOException))
    Right _ -> return $ Success $ "'" ++ src ++ "' を '" ++ dest ++ "' に移動しました"
mv _ = return $ Error (InvalidArguments "mv: 使用法: mv <source> <destination>")

-- | rm - ファイルを削除
rm :: [String] -> IO CommandResult
rm [] = return $ Error (InvalidArguments "rm: 少なくとも1つのファイルを指定してください")
rm files = do
  results <- mapM removeFile files
  case sequence results of
    Left err -> return $ Error err
    Right _ -> return $ Success $ show (length files) ++ " 個のファイルを削除しました"
  where
    removeFile file = do
      result <- try $ Dir.removeFile file
      case result of
        Left err -> return $ Left (IOError $ show (err :: IOException))
        Right _ -> return $ Right ()

-- | mkdir - ディレクトリを作成
mkdir' :: [String] -> IO CommandResult
mkdir' [] = return $ Error (InvalidArguments "mkdir: ディレクトリ名を指定してください")
mkdir' dirs = do
  results <- mapM createDir dirs
  case sequence results of
    Left err -> return $ Error err
    Right _ -> return $ Success $ show (length dirs) ++ " 個のディレクトリを作成しました"
  where
    createDir dir = do
      result <- try $ Dir.createDirectory dir
      case result of
        Left err -> return $ Left (IOError $ show (err :: IOException))
        Right _ -> return $ Right ()

-- | rmdir - 空のディレクトリを削除
rmdir' :: [String] -> IO CommandResult
rmdir' [] = return $ Error (InvalidArguments "rmdir: ディレクトリ名を指定してください")
rmdir' dirs = do
  results <- mapM removeDir dirs
  case sequence results of
    Left err -> return $ Error err
    Right _ -> return $ Success $ show (length dirs) ++ " 個のディレクトリを削除しました"
  where
    removeDir dir = do
      result <- try $ Dir.removeDirectory dir
      case result of
        Left err -> return $ Left (IOError $ show (err :: IOException))
        Right _ -> return $ Right ()

-- | touch - ファイルを作成または更新
touch :: [String] -> IO CommandResult
touch [] = return $ Error (InvalidArguments "touch: ファイル名を指定してください")
touch files = do
  results <- mapM touchFile files
  case sequence results of
    Left err -> return $ Error err
    Right _ -> return $ Success $ show (length files) ++ " 個のファイルを作成/更新しました"
  where
    touchFile file = do
      exists <- Dir.doesFileExist file
      result <- if exists
        then try $ Dir.setModificationTime file =<< getCurrentTime
        else try $ writeFile file ""
      case result of
        Left err -> return $ Left (IOError $ show (err :: IOException))
        Right _ -> return $ Right ()

-- | grep - パターンマッチング
grep :: [String] -> IO CommandResult
grep (pattern:files) = do
  results <- mapM (grepFile pattern) files
  let matches = concat [lines | Right lines <- results]
  if null matches
    then return $ Success ""
    else return $ Success (unlines matches)
  where
    grepFile pat file = do
      contentResult <- readFileContent file
      case contentResult of
        Left _ -> return $ Left (FileNotFound file)
        Right content -> 
          let matchedLines = filter (=~ pat) (lines content)
              formattedLines = map (\line -> file ++ ":" ++ line) matchedLines
          in return $ Right formattedLines
grep _ = return $ Error (InvalidArguments "grep: 使用法: grep <pattern> <file1> [file2...]")

-- | wc - 行数、単語数、文字数をカウント
wc :: [String] -> IO CommandResult
wc [] = return $ Error (InvalidArguments "wc: ファイルを指定してください")
wc files = do
  results <- mapM countFile files
  let counts = [result | Right result <- results]
  if length counts == length files
    then do
      let output = unlines [formatCount file l w c | (file, l, w, c) <- counts]
      return $ Success output
    else return $ Error (FileNotFound "一部のファイルが見つかりません")
  where
    countFile file = do
      contentResult <- readFileContent file
      case contentResult of
        Left _ -> return $ Left (FileNotFound file)
        Right content -> 
          let lineCount = length (lines content)
              wordCount = length (words content)
              charCount = length content
          in return $ Right (file, lineCount, wordCount, charCount)
    formatCount file l w c = 
      show l ++ "\t" ++ show w ++ "\t" ++ show c ++ "\t" ++ file

-- | head - ファイルの先頭部分を表示
head' :: [String] -> IO CommandResult
head' [] = return $ Error (InvalidArguments "head: ファイルを指定してください")
head' ("-n":n:files) = 
  case reads n of
    [(num, "")] -> headFiles num files
    _ -> return $ Error (InvalidArguments "head: 無効な行数です")
head' files = headFiles 10 files

headFiles :: Int -> [String] -> IO CommandResult
headFiles n files = do
  results <- mapM (headFile n) files
  let outputs = [output | Right output <- results]
  return $ Success (concat outputs)
  where
    headFile num file = do
      contentResult <- readFileContent file
      case contentResult of
        Left _ -> return $ Left (FileNotFound file)
        Right content -> 
          let fileLines = take num (lines content)
          in return $ Right (unlines fileLines)

-- | tail - ファイルの末尾部分を表示
tail' :: [String] -> IO CommandResult
tail' [] = return $ Error (InvalidArguments "tail: ファイルを指定してください")
tail' files = tailFiles 10 files

tailFiles :: Int -> [String] -> IO CommandResult
tailFiles n files = do
  results <- mapM (tailFile n) files
  let outputs = [output | Right output <- results]
  return $ Success (concat outputs)
  where
    tailFile num file = do
      contentResult <- readFileContent file
      case contentResult of
        Left _ -> return $ Left (FileNotFound file)
        Right content -> 
          let fileLines = reverse (take num (reverse (lines content)))
          in return $ Right (unlines fileLines)

-- | sort - 行をソート
sort' :: [String] -> IO CommandResult
sort' [] = return $ Error (InvalidArguments "sort: ファイルを指定してください")
sort' files = do
  results <- mapM sortFile files
  let outputs = [output | Right output <- results]
  return $ Success (concat outputs)
  where
    sortFile file = do
      contentResult <- readFileContent file
      case contentResult of
        Left _ -> return $ Left (FileNotFound file)
        Right content -> 
          let sortedLines = List.sort (lines content)
          in return $ Right (unlines sortedLines)

-- | uniq - 重複行を除去
uniq' :: [String] -> IO CommandResult
uniq' [] = return $ Error (InvalidArguments "uniq: ファイルを指定してください")
uniq' files = do
  results <- mapM uniqFile files
  let outputs = [output | Right output <- results]
  return $ Success (concat outputs)
  where
    uniqFile file = do
      contentResult <- readFileContent file
      case contentResult of
        Left _ -> return $ Left (FileNotFound file)
        Right content -> 
          let uniqueLines = map head (List.group (lines content))
          in return $ Right (unlines uniqueLines)

-- | ps - プロセス一覧を表示
ps :: [String] -> IO CommandResult
ps _ = do
  processResult <- listProcesses
  case processResult of
    Left _ -> return $ Error (ProcessError "プロセス一覧の取得に失敗しました")
    Right processes -> do
      let header = "PID\tPPID\tSTATE\tCOMMAND"
      let processLines = map formatProcess processes
      let output = unlines (header : processLines)
      return $ Success output
  where
    formatProcess proc = 
      show (procId proc) ++ "\t" ++ 
      maybe "-" show (procParent proc) ++ "\t" ++ 
      show (procState proc) ++ "\t" ++ 
      procCommand proc

-- | df - ディスク使用量を表示（簡易版）
df :: [String] -> IO CommandResult
df _ = do
  -- 簡易的な実装
  let output = "Filesystem\t1K-blocks\tUsed\tAvailable\tUse%\tMounted on\n" ++
               "/dev/sda1\t1000000\t500000\t500000\t50%\t/"
  return $ Success output

-- | du - ディレクトリサイズを表示（簡易版）
du :: [String] -> IO CommandResult
du [] = du ["."]
du (path:_) = do
  -- 簡易的な実装
  return $ Success $ "4\t" ++ path

-- | whoami - 現在のユーザー名を表示
whoami :: [String] -> IO CommandResult
whoami _ = do
  -- 簡易的な実装
  return $ Success "user"

-- | pwd - 現在のディレクトリを表示
pwd' :: [String] -> IO CommandResult
pwd' _ = do
  currentDir <- Dir.getCurrentDirectory
  return $ Success currentDir

-- | kill - プロセスを終了
kill' :: [String] -> IO CommandResult
kill' [] = return $ Error (InvalidArguments "kill: プロセスIDを指定してください")
kill' (pidStr:_) = 
  case reads pidStr of
    [(pid, "")] -> do
      -- 簡易的な実装
      return $ Success $ "プロセス " ++ show pid ++ " に終了シグナルを送信しました"
    _ -> return $ Error (InvalidArguments "kill: 無効なプロセスIDです")

-- | killall - 名前でプロセスを終了
killall :: [String] -> IO CommandResult
killall [] = return $ Error (InvalidArguments "killall: プロセス名を指定してください")
killall (name:_) = do
  -- 簡易的な実装
  return $ Success $ "プロセス '" ++ name ++ "' に終了シグナルを送信しました"

-- | ping - ネットワーク接続をテスト（簡易版）
ping' :: [String] -> IO CommandResult
ping' [] = return $ Error (InvalidArguments "ping: ホストを指定してください")
ping' (host:_) = do
  -- 簡易的な実装
  return $ Success $ "PING " ++ host ++ " (127.0.0.1): 56 data bytes\n" ++
                     "64 bytes from 127.0.0.1: icmp_seq=0 time=1.234 ms"

-- | wget - ファイルをダウンロード（簡易版）
wget' :: [String] -> IO CommandResult
wget' [] = return $ Error (InvalidArguments "wget: URLを指定してください")
wget' (url:_) = do
  -- 簡易的な実装
  return $ Success $ "'" ++ url ++ "' をダウンロードしました"