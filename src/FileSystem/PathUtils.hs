{-|
Module      : FileSystem.PathUtils
Description : Linuxファイルパス操作のユーティリティ
Copyright   : (c) Portfolio, 2025
License     : MIT
Maintainer  : portfolio@example.com

Linuxファイルシステムのパス操作に関する便利な関数を提供します。
パスの正規化、相対パス・絶対パスの変換、パス検索などを学習できます。
-}

module FileSystem.PathUtils
  ( -- * パス操作
    normalizePath
  , canonicalizePath'
  , makeRelativePath
  , makeAbsolutePath
    -- * パス分析
  , isAbsolutePath
  , isRelativePath
  , getPathComponents
  , getParentDirectory
  , getFileName
  , getFileExtension
  , getBaseName
    -- * パス結合・分割
  , joinPath
  , splitPath
  , splitFileName
  , splitExtension
    -- * パス検索
  , findExecutable'
  , findFiles
  , findDirectories
  , searchPath
    -- * パス変換
  , toUnixPath
  , toWindowsPath
  , expandTilde
  , resolveSymlinks
    -- * パス検証
  , isValidPath
  , pathExists
  , isPathSafe
    -- * パターンマッチング
  , matchGlob
  , expandGlob
  ) where

import qualified System.FilePath as Path
import qualified System.Directory as Dir
import Control.Exception (try, IOException)
import Data.List (isPrefixOf, isSuffixOf)
-- import Text.Regex.Posix ((=~))

-- | パスを正規化（冗長な . と .. を解決）
normalizePath :: FilePath -> FilePath
normalizePath = Path.normalise

-- | パスを正規化し、シンボリックリンクを解決
canonicalizePath' :: FilePath -> IO (Either String FilePath)
canonicalizePath' path = do
  result <- try $ Dir.canonicalizePath path
  case result of
    Left err -> return $ Left (show (err :: IOException))
    Right canonPath -> return $ Right canonPath

-- | 基準ディレクトリからの相対パスを作成
makeRelativePath :: FilePath -> FilePath -> FilePath
makeRelativePath = Path.makeRelative

-- | 相対パスを絶対パスに変換
makeAbsolutePath :: FilePath -> IO FilePath
makeAbsolutePath = Dir.makeAbsolute

-- | パスが絶対パスかどうかチェック
isAbsolutePath :: FilePath -> Bool
isAbsolutePath = Path.isAbsolute

-- | パスが相対パスかどうかチェック
isRelativePath :: FilePath -> Bool
isRelativePath = Path.isRelative

-- | パスを構成要素に分割
getPathComponents :: FilePath -> [FilePath]
getPathComponents = Path.splitDirectories

-- | 親ディレクトリを取得
getParentDirectory :: FilePath -> FilePath
getParentDirectory = Path.takeDirectory

-- | ファイル名（拡張子込み）を取得
getFileName :: FilePath -> FilePath
getFileName = Path.takeFileName

-- | ファイル拡張子を取得
getFileExtension :: FilePath -> String
getFileExtension = Path.takeExtension

-- | ベース名（拡張子なしファイル名）を取得
getBaseName :: FilePath -> FilePath
getBaseName = Path.takeBaseName

-- | パス構成要素を結合
joinPath :: [FilePath] -> FilePath
joinPath = Path.joinPath

-- | パスをディレクトリとファイル名に分割
splitPath :: FilePath -> (FilePath, FilePath)
splitPath = Path.splitFileName

-- | パスをディレクトリとファイル名に分割（別実装）
splitFileName :: FilePath -> (FilePath, FilePath)
splitFileName = Path.splitFileName

-- | ファイル名と拡張子を分割
splitExtension :: FilePath -> (FilePath, String)
splitExtension = Path.splitExtension

-- | 実行可能ファイルを検索
findExecutable' :: String -> IO (Maybe FilePath)
findExecutable' = Dir.findExecutable

-- | 指定ディレクトリ内のファイルを検索
findFiles :: FilePath -> (FilePath -> Bool) -> IO [FilePath]
findFiles dir predicate = do
  exists <- Dir.doesDirectoryExist dir
  if not exists
    then return []
    else do
      contents <- Dir.listDirectory dir
      files <- filterM (\f -> do
        let fullPath = dir Path.</> f
        isFile <- Dir.doesFileExist fullPath
        return $ isFile && predicate f
        ) contents
      return $ map (dir Path.</>) files

-- | 指定ディレクトリ内のディレクトリを検索
findDirectories :: FilePath -> (FilePath -> Bool) -> IO [FilePath]
findDirectories dir predicate = do
  exists <- Dir.doesDirectoryExist dir
  if not exists
    then return []
    else do
      contents <- Dir.listDirectory dir
      dirs <- filterM (\d -> do
        let fullPath = dir Path.</> d
        isDir <- Dir.doesDirectoryExist fullPath
        return $ isDir && predicate d
        ) contents
      return $ map (dir Path.</>) dirs

-- | PATH環境変数でファイルを検索
searchPath :: String -> IO (Maybe FilePath)
searchPath fileName = do
  -- 簡易実装: 現在のディレクトリでのみ検索
  exists <- Dir.doesFileExist fileName
  if exists
    then return $ Just fileName
    else return Nothing

-- | パスをUnix形式に変換
toUnixPath :: FilePath -> FilePath
toUnixPath = map (\c -> if c == '\\' then '/' else c)

-- | パスをWindows形式に変換
toWindowsPath :: FilePath -> FilePath
toWindowsPath = map (\c -> if c == '/' then '\\' else c)

-- | チルダ（~）を展開
expandTilde :: FilePath -> IO FilePath
expandTilde path
  | "~" `isPrefixOf` path = do
      homeDir <- Dir.getHomeDirectory
      return $ homeDir ++ drop 1 path
  | otherwise = return path

-- | シンボリックリンクを解決
resolveSymlinks :: FilePath -> IO (Either String FilePath)
resolveSymlinks = canonicalizePath'

-- | パスが有効かどうかチェック
isValidPath :: FilePath -> Bool
isValidPath path = 
  not (null path) && 
  not (".." `isInfixOf` path && not (Path.isValid path)) &&
  all (\c -> c /= '\0') path
  where
    isInfixOf needle haystack = needle `isPrefixOf` haystack || 
                               case haystack of
                                 [] -> False
                                 (_:xs) -> isInfixOf needle xs

-- | パスが存在するかチェック
pathExists :: FilePath -> IO Bool
pathExists path = do
  fileExists <- Dir.doesFileExist path
  if fileExists
    then return True
    else Dir.doesDirectoryExist path

-- | パスが安全かどうかチェック（ディレクトリトラバーサル攻撃対策）
isPathSafe :: FilePath -> FilePath -> Bool
isPathSafe baseDir requestedPath = 
  let normalizedBase = normalizePath baseDir
      normalizedRequested = normalizePath requestedPath
      fullPath = normalizedBase Path.</> normalizedRequested
      canonicalFullPath = normalizePath fullPath
  in normalizedBase `isPrefixOf` canonicalFullPath

-- | globパターンにマッチするかチェック（簡易実装）
matchGlob :: String -> FilePath -> Bool
matchGlob pattern path = simpleGlobMatch pattern path

-- | 簡易globマッチ実装
simpleGlobMatch :: String -> String -> Bool
simpleGlobMatch [] [] = True
simpleGlobMatch [] _ = False
simpleGlobMatch ('*':ps) str = any (simpleGlobMatch ps) (suffixes str)
  where suffixes s = [drop i s | i <- [0..length s]]
simpleGlobMatch ('?':ps) (_:str) = simpleGlobMatch ps str
simpleGlobMatch ('?':_) [] = False
simpleGlobMatch (p:ps) (s:str) | p == s = simpleGlobMatch ps str
simpleGlobMatch _ _ = False

-- | globパターンでファイルを展開
expandGlob :: FilePath -> String -> IO [FilePath]
expandGlob dir pattern = do
  exists <- Dir.doesDirectoryExist dir
  if not exists
    then return []
    else do
      contents <- Dir.listDirectory dir
      let matches = filter (matchGlob pattern) contents
      return $ map (dir Path.</>) matches

-- | フィルタ用のヘルパー関数
filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM _ [] = return []
filterM predicate (x:xs) = do
  keep <- predicate x
  rest <- filterM predicate xs
  return $ if keep then x:rest else rest

-- * 共通パターン

-- | テキストファイルかどうかチェック
isTextFile :: FilePath -> Bool
isTextFile path = 
  let ext = getFileExtension path
  in ext `elem` [".txt", ".md", ".rst", ".log", ".conf", ".ini", ".yaml", ".yml", ".json", ".xml"]

-- | 実行可能ファイルかどうかチェック（拡張子ベース）
isExecutableFile :: FilePath -> Bool
isExecutableFile path = 
  let ext = getFileExtension path
  in ext `elem` [".exe", ".sh", ".py", ".pl", ".rb"] || null ext

-- | 隠しファイルかどうかチェック
isHiddenFile :: FilePath -> Bool
isHiddenFile path = 
  let fileName = getFileName path
  in not (null fileName) && head fileName == '.'

-- | バックアップファイルかどうかチェック
isBackupFile :: FilePath -> Bool
isBackupFile path = 
  let fileName = getFileName path
  in "~" `isSuffixOf` fileName || ".bak" `isSuffixOf` fileName || ".backup" `isSuffixOf` fileName

-- | 一時ファイルかどうかチェック
isTempFile :: FilePath -> Bool
isTempFile path = 
  let fileName = getFileName path
  in ".tmp" `isSuffixOf` fileName || ".temp" `isSuffixOf` fileName

-- | ディレクトリの深度を計算
getPathDepth :: FilePath -> Int
getPathDepth = length . getPathComponents

-- | 最も近い共通の親ディレクトリを見つける
findCommonParent :: [FilePath] -> Maybe FilePath
findCommonParent [] = Nothing
findCommonParent [path] = Just $ getParentDirectory path
findCommonParent paths = 
  let components = map getPathComponents paths
      commonParts = foldl1 findCommonPrefix components
  in if null commonParts 
     then Just "/"
     else Just $ joinPath commonParts
  where
    findCommonPrefix [] _ = []
    findCommonPrefix _ [] = []
    findCommonPrefix (x:xs) (y:ys)
      | x == y = x : findCommonPrefix xs ys
      | otherwise = []