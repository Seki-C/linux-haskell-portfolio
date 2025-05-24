{-|
Module      : FileSystem.Operations
Description : Linuxファイルシステムの基本操作
Copyright   : (c) Portfolio, 2025
License     : MIT
Maintainer  : portfolio@example.com

Linuxファイルシステムの基本的な操作を提供します。
ファイルの作成、削除、コピー、移動などの操作を学習できます。
-}

module FileSystem.Operations
  ( -- * ファイル操作
    createFile
  , deleteFile
  , copyFile'
  , moveFile'
  , readFileContent
  , writeFileContent
    -- * ディレクトリ操作
  , createDirectory'
  , deleteDirectory'
  , listDirectory'
  , changeDirectory
    -- * ファイル情報
  , getFileSize
  , getFileInfo
  , doesFileExist'
  , doesDirectoryExist'
    -- * パス操作
  , absolutePath
  , relativePath
  , joinPaths
  , splitPath'
    -- * 権限操作
  , changePermissions
  , getPermissions'
    -- * エラー処理
  , FileSystemError(..)
  ) where

import qualified System.Directory as Dir
import qualified System.FilePath as Path
import System.IO (IOMode(..), hClose, hGetContents, openFile)
import Control.Exception (Exception, try, IOException)
import Data.Time (UTCTime)
import Linux.Core (FilePermissions(..), FileStat(..), FileType(..))

-- | ファイルシステム操作のエラー型
data FileSystemError
  = FileNotFound FilePath
  | PermissionDenied FilePath
  | FileExists FilePath
  | DirectoryNotEmpty FilePath
  | InvalidPath FilePath
  | IOError String
  deriving (Show, Eq)

instance Exception FileSystemError

-- | 新しいファイルを作成する
-- ファイルが既に存在する場合はFileExistsエラーを返す
createFile :: FilePath -> IO (Either FileSystemError ())
createFile path = do
  exists <- Dir.doesFileExist path
  if exists
    then return $ Left (FileExists path)
    else do
      result <- try $ writeFile path ""
      case result of
        Left err -> return $ Left (IOError $ show (err :: IOException))
        Right _  -> return $ Right ()

-- | ファイルを削除する
deleteFile :: FilePath -> IO (Either FileSystemError ())
deleteFile path = do
  exists <- Dir.doesFileExist path
  if not exists
    then return $ Left (FileNotFound path)
    else do
      result <- try $ Dir.removeFile path
      case result of
        Left err -> return $ Left (IOError $ show (err :: IOException))
        Right _  -> return $ Right ()

-- | ファイルをコピーする
copyFile' :: FilePath -> FilePath -> IO (Either FileSystemError ())
copyFile' src dest = do
  srcExists <- Dir.doesFileExist src
  if not srcExists
    then return $ Left (FileNotFound src)
    else do
      destExists <- Dir.doesFileExist dest
      if destExists
        then return $ Left (FileExists dest)
        else do
          result <- try $ Dir.copyFile src dest
          case result of
            Left err -> return $ Left (IOError $ show (err :: IOException))
            Right _  -> return $ Right ()

-- | ファイルを移動する（リネーム）
moveFile' :: FilePath -> FilePath -> IO (Either FileSystemError ())
moveFile' src dest = do
  srcExists <- Dir.doesFileExist src
  if not srcExists
    then return $ Left (FileNotFound src)
    else do
      destExists <- Dir.doesFileExist dest
      if destExists
        then return $ Left (FileExists dest)
        else do
          result <- try $ Dir.renameFile src dest
          case result of
            Left err -> return $ Left (IOError $ show (err :: IOException))
            Right _  -> return $ Right ()

-- | ファイルの内容を読み取る
readFileContent :: FilePath -> IO (Either FileSystemError String)
readFileContent path = do
  exists <- Dir.doesFileExist path
  if not exists
    then return $ Left (FileNotFound path)
    else do
      result <- try $ readFile path
      case result of
        Left err -> return $ Left (IOError $ show (err :: IOException))
        Right content -> return $ Right content

-- | ファイルに内容を書き込む
writeFileContent :: FilePath -> String -> IO (Either FileSystemError ())
writeFileContent path content = do
  result <- try $ writeFile path content
  case result of
    Left err -> return $ Left (IOError $ show (err :: IOException))
    Right _  -> return $ Right ()

-- | ディレクトリを作成する
createDirectory' :: FilePath -> IO (Either FileSystemError ())
createDirectory' path = do
  exists <- Dir.doesDirectoryExist path
  if exists
    then return $ Left (FileExists path)
    else do
      result <- try $ Dir.createDirectory path
      case result of
        Left err -> return $ Left (IOError $ show (err :: IOException))
        Right _  -> return $ Right ()

-- | ディレクトリを削除する（空の場合のみ）
deleteDirectory' :: FilePath -> IO (Either FileSystemError ())
deleteDirectory' path = do
  exists <- Dir.doesDirectoryExist path
  if not exists
    then return $ Left (FileNotFound path)
    else do
      result <- try $ Dir.removeDirectory path
      case result of
        Left err -> return $ Left (DirectoryNotEmpty path)
        Right _  -> return $ Right ()

-- | ディレクトリの内容を一覧表示
listDirectory' :: FilePath -> IO (Either FileSystemError [FilePath])
listDirectory' path = do
  exists <- Dir.doesDirectoryExist path
  if not exists
    then return $ Left (FileNotFound path)
    else do
      result <- try $ Dir.listDirectory path
      case result of
        Left err -> return $ Left (IOError $ show (err :: IOException))
        Right contents -> return $ Right contents

-- | 現在のディレクトリを変更
changeDirectory :: FilePath -> IO (Either FileSystemError ())
changeDirectory path = do
  exists <- Dir.doesDirectoryExist path
  if not exists
    then return $ Left (FileNotFound path)
    else do
      result <- try $ Dir.setCurrentDirectory path
      case result of
        Left err -> return $ Left (IOError $ show (err :: IOException))
        Right _  -> return $ Right ()

-- | ファイルサイズを取得
getFileSize :: FilePath -> IO (Either FileSystemError Integer)
getFileSize path = do
  exists <- Dir.doesFileExist path
  if not exists
    then return $ Left (FileNotFound path)
    else do
      result <- try $ Dir.getFileSize path
      case result of
        Left err -> return $ Left (IOError $ show (err :: IOException))
        Right size -> return $ Right size

-- | ファイルの詳細情報を取得（簡易版）
getFileInfo :: FilePath -> IO (Either FileSystemError (Integer, UTCTime))
getFileInfo path = do
  exists <- Dir.doesFileExist path
  if not exists
    then return $ Left (FileNotFound path)
    else do
      sizeResult <- try $ Dir.getFileSize path
      timeResult <- try $ Dir.getModificationTime path
      case (sizeResult, timeResult) of
        (Right size, Right time) -> return $ Right (size, time)
        _ -> return $ Left (IOError "Failed to get file info")

-- | ファイルが存在するかチェック
doesFileExist' :: FilePath -> IO Bool
doesFileExist' = Dir.doesFileExist

-- | ディレクトリが存在するかチェック
doesDirectoryExist' :: FilePath -> IO Bool
doesDirectoryExist' = Dir.doesDirectoryExist

-- | 絶対パスを取得
absolutePath :: FilePath -> IO FilePath
absolutePath = Dir.makeAbsolute

-- | 相対パスを取得（現在のディレクトリから）
relativePath :: FilePath -> IO FilePath
relativePath path = do
  currentDir <- Dir.getCurrentDirectory
  absPath <- Dir.makeAbsolute path
  return $ Path.makeRelative currentDir absPath

-- | パスを結合
joinPaths :: [FilePath] -> FilePath
joinPaths = foldr1 Path.combine

-- | パスを分割
splitPath' :: FilePath -> [FilePath]
splitPath' = Path.splitPath

-- | ファイル権限を変更（プレースホルダー実装）
changePermissions :: FilePath -> FilePermissions -> IO (Either FileSystemError ())
changePermissions path perms = do
  exists <- Dir.doesFileExist path
  if not exists
    then return $ Left (FileNotFound path)
    else return $ Right () -- 実際の実装はOSに依存

-- | ファイル権限を取得（プレースホルダー実装）
getPermissions' :: FilePath -> IO (Either FileSystemError FilePermissions)
getPermissions' path = do
  exists <- Dir.doesFileExist path
  if not exists
    then return $ Left (FileNotFound path)
    else do
      -- 簡易的な権限情報を返す
      perms <- Dir.getPermissions path
      let filePerms = FilePermissions
            { ownerRead = Dir.readable perms
            , ownerWrite = Dir.writable perms
            , ownerExecute = Dir.executable perms
            , groupRead = Dir.readable perms
            , groupWrite = False
            , groupExecute = False
            , otherRead = Dir.readable perms
            , otherWrite = False
            , otherExecute = False
            }
      return $ Right filePerms