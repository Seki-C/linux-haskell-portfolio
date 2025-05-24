{-|
Module      : Main
Description : Linux OS学習ポートフォリオのテストスイート
Copyright   : (c) Portfolio, 2025
License     : MIT
Maintainer  : portfolio@example.com

Linux OS学習ポートフォリオの包括的なテストスイートです。
各モジュールの機能をテストし、学習用の例も提供します。
-}

module Main where

import Test.Hspec
import qualified Data.Map as Map
import Control.Exception (evaluate)

-- テスト対象モジュールのインポート
import qualified Linux.Core as Core
import qualified FileSystem.Permissions as Perms
import qualified FileSystem.PathUtils as Path
import qualified Shell.Parser as ShellParser
import qualified Shell.Commands as ShellCmd
import qualified Network.Socket as NetSock
import qualified System.Memory as Memory

main :: IO ()
main = hspec $ do
  describe "Linux.Core" linuxCoreTests
  describe "FileSystem.Permissions" permissionsTests
  describe "FileSystem.PathUtils" pathUtilsTests
  describe "Shell.Parser" shellParserTests
  describe "Shell.Commands" shellCommandsTests
  describe "Network.Socket" networkSocketTests
  describe "System.Memory" memoryTests

-- | Linux基本概念のテスト
linuxCoreTests :: Spec
linuxCoreTests = do
  describe "ファイル権限の文字列表現" $ do
    it "rwxr-xr-- 形式の権限を正しく表示する" $ do
      let perms = Core.FilePermissions True True True True False True True False False
      Core.showPermissions perms `shouldBe` "rwxr-xr--"
    
    it "すべて拒否の権限を正しく表示する" $ do
      let perms = Core.FilePermissions False False False False False False False False False
      Core.showPermissions perms `shouldBe` "---------"
    
    it "すべて許可の権限を正しく表示する" $ do
      let perms = Core.FilePermissions True True True True True True True True True
      Core.showPermissions perms `shouldBe` "rwxrwxrwx"
  
  describe "実行可能チェック" $ do
    it "所有者実行権限があるファイルを実行可能として認識する" $ do
      let perms = Core.FilePermissions False False True False False False False False False
      Core.isExecutable perms `shouldBe` True
    
    it "実行権限がないファイルを実行不可として認識する" $ do
      let perms = Core.FilePermissions True True False True False False True False False
      Core.isExecutable perms `shouldBe` False
  
  describe "ディレクトリチェック" $ do
    it "ディレクトリタイプを正しく識別する" $ do
      Core.isDirectory Core.Directory `shouldBe` True
      Core.isDirectory Core.RegularFile `shouldBe` False

-- | ファイル権限のテスト
permissionsTests :: Spec
permissionsTests = do
  describe "8進数変換" $ do
    it "755権限を正しく8進数に変換する" $ do
      let perms = Core.FilePermissions True True True True False True True False True
      Perms.permissionsToOctal perms `shouldBe` 755
    
    it "644権限を正しく8進数に変換する" $ do
      let perms = Core.FilePermissions True True False True False False True False False
      Perms.permissionsToOctal perms `shouldBe` 644
    
    it "8進数から権限への往復変換が正しく動作する" $ do
      let originalPerms = Core.FilePermissions True False True False True False True False True
      let octal = Perms.permissionsToOctal originalPerms
      let convertedPerms = Perms.octalToPermissions octal
      convertedPerms `shouldBe` originalPerms
  
  describe "文字列変換" $ do
    it "正しい権限文字列を解析する" $ do
      let maybePerms = Perms.stringToPermissions "rwxr-x---"
      maybePerms `shouldSatisfy` (\p -> case p of Just _ -> True; Nothing -> False)
    
    it "無効な長さの文字列を拒否する" $ do
      Perms.stringToPermissions "rwx" `shouldBe` Nothing
    
    it "権限から文字列への往復変換が正しく動作する" $ do
      let originalPerms = Core.FilePermissions True False True True False True False False False
      let permString = Perms.permissionsToString originalPerms
      let maybePerms = Perms.stringToPermissions permString
      maybePerms `shouldBe` Just originalPerms
  
  describe "権限操作" $ do
    it "権限を正しく追加する" $ do
      let basePerms = Core.FilePermissions False False False False False False False False False
      let newPerms = Perms.addPermission Perms.Owner Perms.Read basePerms
      Core.ownerRead newPerms `shouldBe` True
    
    it "権限を正しく削除する" $ do
      let basePerms = Core.FilePermissions True True True True True True True True True
      let newPerms = Perms.removePermission Perms.Group Perms.Write basePerms
      Core.groupWrite newPerms `shouldBe` False
    
    it "権限の存在を正しくチェックする" $ do
      let perms = Core.FilePermissions True False True False True False False True False
      Perms.hasPermission Perms.Owner Perms.Read perms `shouldBe` True
      Perms.hasPermission Perms.Owner Perms.Write perms `shouldBe` False

-- | パス操作のテスト
pathUtilsTests :: Spec
pathUtilsTests = do
  describe "パス分析" $ do
    it "ファイル名を正しく抽出する" $ do
      Path.getFileName "/home/user/document.txt" `shouldBe` "document.txt"
      Path.getFileName "document.txt" `shouldBe` "document.txt"
    
    it "ディレクトリを正しく抽出する" $ do
      Path.getParentDirectory "/home/user/document.txt" `shouldBe` "/home/user"
      Path.getParentDirectory "document.txt" `shouldBe` "."
    
    it "拡張子を正しく抽出する" $ do
      Path.getFileExtension "document.txt" `shouldBe` ".txt"
      Path.getFileExtension "archive.tar.gz" `shouldBe` ".gz"
      Path.getFileExtension "README" `shouldBe` ""
    
    it "ベース名を正しく抽出する" $ do
      Path.getBaseName "document.txt" `shouldBe` "document"
      Path.getBaseName "/home/user/archive.tar.gz" `shouldBe` "archive.tar"
  
  describe "パス操作" $ do
    it "パス要素を正しく結合する" $ do
      Path.joinPath ["home", "user", "documents"] `shouldContain` "user"
    
    it "パスを正しく正規化する" $ do
      let messyPath = "/home/user/../user/./documents//file.txt"
      let normalized = Path.normalizePath messyPath
      normalized `shouldNotContain` ".."
      normalized `shouldNotContain` "./"
  
  describe "パス判定" $ do
    it "絶対パスを正しく識別する" $ do
      Path.isAbsolutePath "/home/user" `shouldBe` True
      Path.isAbsolutePath "home/user" `shouldBe` False
      Path.isAbsolutePath "C:\\Users\\user" `shouldBe` True
    
    it "相対パスを正しく識別する" $ do
      Path.isRelativePath "home/user" `shouldBe` True
      Path.isRelativePath "/home/user" `shouldBe` False
    
    it "隠しファイルを正しく識別する" $ do
      Path.isHiddenFile ".bashrc" `shouldBe` True
      Path.isHiddenFile "bashrc" `shouldBe` False
      Path.isHiddenFile "/home/user/.vimrc" `shouldBe` True

-- | シェルパーサーのテスト
shellParserTests :: Spec
shellParserTests = do
  describe "基本コマンド解析" $ do
    it "単純なコマンドを正しく解析する" $ do
      let result = ShellParser.parseCommand "ls -la"
      case result of
        Right cmd -> do
          ShellParser.cmdName cmd `shouldBe` "ls"
          ShellParser.cmdArgs cmd `shouldBe` ["-la"]
        Left err -> expectationFailure $ "解析に失敗: " ++ show err
    
    it "引数なしのコマンドを正しく解析する" $ do
      let result = ShellParser.parseCommand "pwd"
      case result of
        Right cmd -> do
          ShellParser.cmdName cmd `shouldBe` "pwd"
          ShellParser.cmdArgs cmd `shouldBe` []
        Left err -> expectationFailure $ "解析に失敗: " ++ show err
  
  describe "パイプライン解析" $ do
    it "単純なパイプラインを正しく解析する" $ do
      let result = ShellParser.parsePipeline "cat file | grep pattern"
      case result of
        Right pipeline -> do
          length (ShellParser.pipeCommands pipeline) `shouldBe` 2
          let commands = ShellParser.pipeCommands pipeline
          ShellParser.cmdName (head commands) `shouldBe` "cat"
          ShellParser.cmdName (commands !! 1) `shouldBe` "grep"
        Left err -> expectationFailure $ "解析に失敗: " ++ show err
  
  describe "リダイレクション解析" $ do
    it "出力リダイレクションを正しく解析する" $ do
      let result = ShellParser.parseRedirection "> output.txt"
      case result of
        Right redir -> do
          ShellParser.redirType redir `shouldBe` ShellParser.OutputRedirect
          ShellParser.redirTarget redir `shouldBe` "output.txt"
        Left err -> expectationFailure $ "解析に失敗: " ++ show err
    
    it "追記リダイレクションを正しく解析する" $ do
      let result = ShellParser.parseRedirection ">> log.txt"
      case result of
        Right redir -> do
          ShellParser.redirType redir `shouldBe` ShellParser.AppendRedirect
          ShellParser.redirTarget redir `shouldBe` "log.txt"
        Left err -> expectationFailure $ "解析に失敗: " ++ show err
  
  describe "変数展開" $ do
    it "単純な変数を正しく展開する" $ do
      let env = Map.fromList [("USER", "john"), ("HOME", "/home/john")]
      let result = ShellParser.expandVariables env "Hello $USER"
      result `shouldBe` Right "Hello john"
    
    it "ブレース記法の変数を正しく展開する" $ do
      let env = Map.fromList [("USER", "john")]
      let result = ShellParser.expandVariables env "Hello ${USER}!"
      result `shouldBe` Right "Hello john!"
    
    it "存在しない変数でエラーを返す" $ do
      let env = Map.empty
      let result = ShellParser.expandVariables env "Hello $UNKNOWN"
      case result of
        Left (ShellParser.UnknownVariable _) -> return ()
        _ -> expectationFailure "存在しない変数のエラーが期待されます"
  
  describe "特殊文字の認識" $ do
    it "特殊文字を正しく識別する" $ do
      ShellParser.isSpecialChar '|' `shouldBe` True
      ShellParser.isSpecialChar '&' `shouldBe` True
      ShellParser.isSpecialChar 'a' `shouldBe` False
    
    it "クォート文字列を正しく識別する" $ do
      ShellParser.isQuoted "\"hello\"" `shouldBe` True
      ShellParser.isQuoted "'hello'" `shouldBe` True
      ShellParser.isQuoted "hello" `shouldBe` False

-- | シェルコマンドのテスト
shellCommandsTests :: Spec
shellCommandsTests = do
  describe "引数検証" $ do
    it "cpコマンドの引数不足を検出する" $ do
      result <- ShellCmd.cp ["file1"]
      case result of
        ShellCmd.Error (ShellCmd.InvalidArguments _) -> return ()
        _ -> expectationFailure "引数不足のエラーが期待されます"
    
    it "mvコマンドの引数不足を検出する" $ do
      result <- ShellCmd.mv []
      case result of
        ShellCmd.Error (ShellCmd.InvalidArguments _) -> return ()
        _ -> expectationFailure "引数不足のエラーが期待されます"
  
  describe "コマンド実行結果" $ do
    it "pwdコマンドが成功結果を返す" $ do
      result <- ShellCmd.pwd' []
      case result of
        ShellCmd.Success _ -> return ()
        ShellCmd.Error err -> expectationFailure $ "pwdコマンドが失敗: " ++ show err
    
    it "psコマンドが成功結果を返す" $ do
      result <- ShellCmd.ps []
      case result of
        ShellCmd.Success _ -> return ()
        ShellCmd.Error err -> expectationFailure $ "psコマンドが失敗: " ++ show err

-- | ネットワークソケットのテスト
networkSocketTests :: Spec
networkSocketTests = do
  describe "アドレス解析" $ do
    it "IPv4アドレスを正しく解析する" $ do
      let maybeAddr = NetSock.parseAddress "192.168.1.1:8080"
      case maybeAddr of
        Just (NetSock.IPv4Address host port) -> do
          host `shouldBe` "192.168.1.1"
          port `shouldBe` 8080
        _ -> expectationFailure "IPv4アドレスの解析に失敗"
    
    it "無効なアドレス形式を拒否する" $ do
      NetSock.parseAddress "invalid" `shouldBe` Nothing
      NetSock.parseAddress "192.168.1.1" `shouldBe` Nothing
  
  describe "アドレスフォーマット" $ do
    it "IPv4アドレスを正しくフォーマットする" $ do
      let addr = NetSock.IPv4Address "127.0.0.1" 3000
      NetSock.formatAddress addr `shouldBe` "127.0.0.1:3000"
    
    it "IPv6アドレスを正しくフォーマットする" $ do
      let addr = NetSock.IPv6Address "::1" 3000
      NetSock.formatAddress addr `shouldBe` "[::1]:3000"
  
  describe "ソケット作成" $ do
    it "TCPソケットの作成が成功する" $ do
      result <- NetSock.createSocket NetSock.StreamSocket NetSock.TCP
      case result of
        Right socket -> do
          closeResult <- NetSock.closeSocket socket
          case closeResult of
            Right _ -> return ()
            Left err -> expectationFailure $ "ソケットクローズエラー: " ++ show err
        Left err -> expectationFailure $ "ソケット作成エラー: " ++ show err
    
    it "UDPソケットの作成が成功する" $ do
      result <- NetSock.createSocket NetSock.DatagramSocket NetSock.UDP
      case result of
        Right socket -> do
          closeResult <- NetSock.closeSocket socket
          case closeResult of
            Right _ -> return ()
            Left err -> expectationFailure $ "ソケットクローズエラー: " ++ show err
        Left err -> expectationFailure $ "ソケット作成エラー: " ++ show err

-- | メモリ管理のテスト
memoryTests :: Spec
memoryTests = do
  describe "メモリ情報取得" $ do
    it "システムメモリ情報を正常に取得する" $ do
      result <- Memory.getMemoryInfo
      case result of
        Right memInfo -> do
          Memory.totalMemory memInfo `shouldSatisfy` (> 0)
          Memory.totalMemory memInfo `shouldSatisfy` (>= Memory.usedMemory memInfo)
        Left err -> expectationFailure $ "メモリ情報取得エラー: " ++ show err
    
    it "スワップ情報を正常に取得する" $ do
      result <- Memory.getSwapInfo
      case result of
        Right swapInfo -> do
          Memory.totalSwap swapInfo `shouldSatisfy` (>= 0)
          Memory.totalSwap swapInfo `shouldSatisfy` (>= Memory.usedSwap swapInfo)
        Left err -> expectationFailure $ "スワップ情報取得エラー: " ++ show err
  
  describe "プロセスメモリ情報" $ do
    it "プロセスメモリ情報を正常に取得する" $ do
      result <- Memory.getProcessMemory 1000
      case result of
        Right procMem -> do
          Memory.processId procMem `shouldBe` 1000
          Memory.virtualSize procMem `shouldSatisfy` (>= Memory.residentSize procMem)
        Left err -> expectationFailure $ "プロセスメモリ情報取得エラー: " ++ show err
  
  describe "メモリ使用量監視" $ do
    it "メモリ使用量の監視が正常に動作する" $ do
      result <- Memory.monitorMemoryUsage 1000
      case result of
        Right usage -> do
          Memory.usageTotal usage `shouldSatisfy` (> 0)
          Memory.usageGrowth usage `shouldSatisfy` (>= 0)
        Left err -> expectationFailure $ "メモリ監視エラー: " ++ show err
  
  describe "メモリプレッシャー" $ do
    it "メモリプレッシャーを正常に計算する" $ do
      result <- Memory.getMemoryPressure
      case result of
        Right pressure -> do
          pressure `shouldSatisfy` (>= 0)
          pressure `shouldSatisfy` (<= 100)
        Left err -> expectationFailure $ "メモリプレッシャー計算エラー: " ++ show err

-- | テスト用ヘルパー関数
shouldContain :: String -> String -> Expectation
shouldContain haystack needle = 
  if needle `isSubsequenceOf` haystack
    then return ()
    else expectationFailure $ "Expected " ++ show haystack ++ " to contain " ++ show needle

shouldNotContain :: String -> String -> Expectation
shouldNotContain haystack needle = 
  if needle `isSubsequenceOf` haystack
    then expectationFailure $ "Expected " ++ show haystack ++ " to not contain " ++ show needle
    else return ()

-- | 部分文字列チェック
isSubsequenceOf :: String -> String -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf needle@(n:ns) (h:hs)
  | n == h = isSubsequenceOf ns hs
  | otherwise = isSubsequenceOf needle hs