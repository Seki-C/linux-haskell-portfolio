{-|
Module      : Main
Description : Linux OS学習ポートフォリオのメインアプリケーション
Copyright   : (c) Portfolio, 2025
License     : MIT
Maintainer  : portfolio@example.com

Linux OSの基本概念をHaskellで学習するためのメインアプリケーションです。
対話的なメニューシステムを通じて各機能をデモンストレーションできます。
-}

module Main where

import System.IO (hFlush, stdout)
import Control.Monad (forever, when)
import System.Exit (exitSuccess)
import qualified Data.Map as Map

-- 各モジュールのインポート
import qualified Linux.Core as Core
import qualified FileSystem.Operations as FileOps
import qualified FileSystem.Permissions as Perms
import qualified FileSystem.PathUtils as Path
import qualified Process.Management as ProcMgmt
import qualified Process.Communication as ProcComm
import qualified Network.SocketDemo as NetSock
import qualified Network.Client as NetClient
import qualified System.Calls as SysCalls
import qualified System.Memory as Memory
import qualified Shell.Commands as ShellCmd
import qualified Shell.Parser as ShellParser

-- | メインエントリーポイント
main :: IO ()
main = do
  showWelcome
  mainLoop

-- | ウェルカムメッセージを表示
showWelcome :: IO ()
showWelcome = do
  putStrLn "=================================================="
  putStrLn "  Linux OS学習ポートフォリオ (Haskell版)"
  putStrLn "=================================================="
  putStrLn ""
  putStrLn "このアプリケーションでは、Linux OSの基本概念を"
  putStrLn "Haskellのコードを通じて学習できます。"
  putStrLn ""
  putStrLn "各メニューを選択して、デモンストレーションを"
  putStrLn "お楽しみください。"
  putStrLn ""

-- | メインループ
mainLoop :: IO ()
mainLoop = forever $ do
  showMainMenu
  choice <- getChoice
  handleChoice choice

-- | メインメニューを表示
showMainMenu :: IO ()
showMainMenu = do
  putStrLn "\n==================== メインメニュー ===================="
  putStrLn "1.  ファイルシステム操作"
  putStrLn "2.  ファイル権限管理"
  putStrLn "3.  パス操作ユーティリティ"
  putStrLn "4.  プロセス管理"
  putStrLn "5.  プロセス間通信（IPC）"
  putStrLn "6.  ネットワークプログラミング"
  putStrLn "7.  ネットワククライアント"
  putStrLn "8.  システムコール"
  putStrLn "9.  メモリ管理"
  putStrLn "10. シェルコマンド"
  putStrLn "11. シェルパーサー"
  putStrLn "12. Linux基本概念"
  putStrLn "0.  終了"
  putStrLn "======================================================="
  putStr "選択してください (0-12): "
  hFlush stdout

-- | ユーザーの選択を取得
getChoice :: IO Int
getChoice = do
  line <- getLine
  case reads line of
    [(n, "")] -> return n
    _ -> do
      putStrLn "無効な入力です。数字を入力してください。"
      getChoice

-- | ユーザーの選択を処理
handleChoice :: Int -> IO ()
handleChoice choice = case choice of
  0  -> do
    putStrLn "ありがとうございました！"
    exitSuccess
  1  -> fileSystemDemo
  2  -> permissionsDemo
  3  -> pathUtilsDemo
  4  -> processManagementDemo
  5  -> ipcDemo
  6  -> networkSocketDemo
  7  -> networkClientDemo
  8  -> systemCallsDemo
  9  -> memoryManagementDemo
  10 -> shellCommandsDemo
  11 -> shellParserDemo
  12 -> linuxCoreDemo
  _  -> putStrLn "無効な選択です。0-12の範囲で選択してください。"

-- | ファイルシステム操作のデモ
fileSystemDemo :: IO ()
fileSystemDemo = do
  putStrLn "\n========== ファイルシステム操作デモ =========="
  
  -- ファイル作成のデモ
  putStrLn "1. ファイル作成のデモ"
  result <- FileOps.createFile "demo.txt"
  case result of
    Left err -> putStrLn $ "エラー: " ++ show err
    Right _ -> putStrLn "ファイル 'demo.txt' を作成しました"
  
  -- ファイル内容書き込みのデモ
  putStrLn "\n2. ファイル書き込みのデモ"
  writeResult <- FileOps.writeFileContent "demo.txt" "Hello, Linux World!\nThis is a demo file."
  case writeResult of
    Left err -> putStrLn $ "エラー: " ++ show err
    Right _ -> putStrLn "ファイルに内容を書き込みました"
  
  -- ファイル読み取りのデモ
  putStrLn "\n3. ファイル読み取りのデモ"
  readResult <- FileOps.readFileContent "demo.txt"
  case readResult of
    Left err -> putStrLn $ "エラー: " ++ show err
    Right content -> putStrLn $ "ファイル内容:\n" ++ content
  
  -- ディレクトリ一覧のデモ
  putStrLn "\n4. ディレクトリ一覧のデモ"
  listResult <- FileOps.listDirectory' "."
  case listResult of
    Left err -> putStrLn $ "エラー: " ++ show err
    Right files -> do
      putStrLn "現在のディレクトリの内容:"
      mapM_ putStrLn files
  
  -- ファイル削除のデモ
  putStrLn "\n5. ファイル削除のデモ"
  deleteResult <- FileOps.deleteFile "demo.txt"
  case deleteResult of
    Left err -> putStrLn $ "エラー: " ++ show err
    Right _ -> putStrLn "ファイル 'demo.txt' を削除しました"

-- | ファイル権限管理のデモ
permissionsDemo :: IO ()
permissionsDemo = do
  putStrLn "\n========== ファイル権限管理デモ =========="
  
  -- 権限表現のデモ
  putStrLn "1. 権限表現のデモ"
  let perms = Perms.octalToPermissions 755
  putStrLn $ "8進数 755 の権限: " ++ Perms.permissionsToString perms
  putStrLn $ "8進数表現に戻す: " ++ show (Perms.permissionsToOctal perms)
  
  -- よく使われる権限の表示
  putStrLn "\n2. よく使われる権限設定"
  mapM_ showCommonPermission Perms.commonPermissions
  where
    showCommonPermission (name, perms) = 
      putStrLn $ name ++ ": " ++ Perms.permissionsToString perms

-- | パス操作ユーティリティのデモ
pathUtilsDemo :: IO ()
pathUtilsDemo = do
  putStrLn "\n========== パス操作ユーティリティデモ =========="
  
  -- パス操作のデモ
  putStrLn "1. パス操作のデモ"
  let path = "/home/user/documents/file.txt"
  putStrLn $ "元のパス: " ++ path
  putStrLn $ "ディレクトリ: " ++ Path.getParentDirectory path
  putStrLn $ "ファイル名: " ++ Path.getFileName path
  putStrLn $ "拡張子: " ++ Path.getFileExtension path
  putStrLn $ "ベース名: " ++ Path.getBaseName path
  
  -- パス結合のデモ
  putStrLn "\n2. パス結合のデモ"
  let joinedPath = Path.joinPath ["/home", "user", "documents", "file.txt"]
  putStrLn $ "結合されたパス: " ++ joinedPath
  
  -- パス正規化のデモ
  putStrLn "\n3. パス正規化のデモ"
  let messyPath = "/home/user/../user/./documents//file.txt"
  putStrLn $ "元のパス: " ++ messyPath
  putStrLn $ "正規化後: " ++ Path.normalizePath messyPath

-- | プロセス管理のデモ
processManagementDemo :: IO ()
processManagementDemo = do
  putStrLn "\n========== プロセス管理デモ =========="
  
  -- プロセス一覧のデモ
  putStrLn "1. プロセス一覧の取得"
  processResult <- ProcMgmt.listProcesses
  case processResult of
    Left err -> putStrLn $ "エラー: " ++ show err
    Right processes -> do
      putStrLn "現在実行中のプロセス（抜粋）:"
      mapM_ showProcessInfo (take 5 processes)
  
  -- 現在のプロセス情報
  putStrLn "\n2. 現在のプロセス情報"
  pid <- SysCalls.getpid'
  putStrLn $ "現在のプロセスID: " ++ show pid
  parentPid <- SysCalls.getppid'
  putStrLn $ "親プロセスID: " ++ show parentPid
  
  where
    showProcessInfo proc = 
      putStrLn $ "PID: " ++ show (ProcMgmt.procId proc) ++ 
                 ", 名前: " ++ ProcMgmt.procName proc ++ 
                 ", 状態: " ++ show (ProcMgmt.procState proc)

-- | プロセス間通信のデモ
ipcDemo :: IO ()
ipcDemo = do
  putStrLn "\n========== プロセス間通信（IPC）デモ =========="
  
  -- パイプ通信のデモ
  putStrLn "1. パイプ通信のデモ"
  ProcComm.producerConsumer
  
  -- 共有メモリのデモ
  putStrLn "\n2. 共有メモリのデモ"
  ProcComm.sharedMemoryExample
  
  -- メッセージキューのデモ
  putStrLn "\n3. メッセージキューのデモ"
  ProcComm.messageQueueExample
  
  -- セマフォのデモ
  putStrLn "\n4. セマフォのデモ"
  ProcComm.semaphoreExample

-- | ネットワークソケットのデモ
networkSocketDemo :: IO ()
networkSocketDemo = do
  putStrLn "\n========== ネットワークソケットデモ =========="
  
  -- ソケット作成のデモ
  putStrLn "1. TCPソケット作成のデモ"
  socketResult <- NetSock.createSocket NetSock.StreamSocket NetSock.TCP
  case socketResult of
    Left err -> putStrLn $ "エラー: " ++ show err
    Right sock -> do
      putStrLn "TCPソケットを作成しました"
      closeResult <- NetSock.closeSocket sock
      case closeResult of
        Left err -> putStrLn $ "エラー: " ++ show err
        Right _ -> putStrLn "ソケットを閉じました"
  
  -- アドレス解析のデモ
  putStrLn "\n2. アドレス解析のデモ"
  case NetSock.parseAddress "127.0.0.1:8080" of
    Nothing -> putStrLn "アドレス解析に失敗しました"
    Just addr -> putStrLn $ "解析されたアドレス: " ++ NetSock.formatAddress addr

-- | ネットワククライアントのデモ
networkClientDemo :: IO ()
networkClientDemo = do
  putStrLn "\n========== ネットワククライアントデモ =========="
  
  -- HTTPクライアントのデモ
  putStrLn "1. HTTPクライアントのデモ"
  NetClient.httpClientExample
  
  putStrLn "\n注意: 実際のネットワーク接続のデモは、"
  putStrLn "対応するサーバーが実行されている場合のみ動作します。"

-- | システムコールのデモ
systemCallsDemo :: IO ()
systemCallsDemo = do
  putStrLn "\n========== システムコールデモ =========="
  
  -- プロセス情報のシステムコール
  putStrLn "1. プロセス情報のシステムコール"
  pid <- SysCalls.getpid'
  putStrLn $ "getpid(): " ++ show pid
  
  parentPid <- SysCalls.getppid'
  putStrLn $ "getppid(): " ++ show parentPid
  
  -- ファイルI/Oシステムコール（シミュレーション）
  putStrLn "\n2. ファイルI/Oシステムコール（シミュレーション）"
  openResult <- SysCalls.open' "test.txt" ["O_RDWR", "O_CREAT"] Nothing
  case openResult of
    Left err -> putStrLn $ "open() エラー: " ++ show err
    Right fd -> do
      putStrLn $ "open() 成功: " ++ show fd
      closeResult <- SysCalls.close' fd
      case closeResult of
        Left err -> putStrLn $ "close() エラー: " ++ show err
        Right _ -> putStrLn "close() 成功"

-- | メモリ管理のデモ
memoryManagementDemo :: IO ()
memoryManagementDemo = do
  putStrLn "\n========== メモリ管理デモ =========="
  
  -- システムメモリ情報
  putStrLn "1. システムメモリ情報"
  memResult <- Memory.getMemoryInfo
  case memResult of
    Left err -> putStrLn $ "エラー: " ++ show err
    Right memInfo -> do
      putStrLn $ "総メモリ: " ++ show (Memory.totalMemory memInfo `div` 1024 `div` 1024) ++ " MB"
      putStrLn $ "空きメモリ: " ++ show (Memory.freeMemory memInfo `div` 1024 `div` 1024) ++ " MB"
      putStrLn $ "使用メモリ: " ++ show (Memory.usedMemory memInfo `div` 1024 `div` 1024) ++ " MB"
  
  -- スワップ情報
  putStrLn "\n2. スワップ情報"
  swapResult <- Memory.getSwapInfo
  case swapResult of
    Left err -> putStrLn $ "エラー: " ++ show err
    Right swapInfo -> do
      putStrLn $ "総スワップ: " ++ show (Memory.totalSwap swapInfo `div` 1024 `div` 1024) ++ " MB"
      putStrLn $ "使用スワップ: " ++ show (Memory.usedSwap swapInfo `div` 1024 `div` 1024) ++ " MB"

-- | シェルコマンドのデモ
shellCommandsDemo :: IO ()
shellCommandsDemo = do
  putStrLn "\n========== シェルコマンドデモ =========="
  
  -- lsコマンドのデモ
  putStrLn "1. ls コマンドのデモ"
  lsResult <- ShellCmd.ls ["."]
  case lsResult of
    ShellCmd.Success output -> putStrLn $ "ls 出力:\n" ++ output
    ShellCmd.Error err -> putStrLn $ "エラー: " ++ show err
  
  -- pwdコマンドのデモ
  putStrLn "\n2. pwd コマンドのデモ"
  pwdResult <- ShellCmd.pwd' []
  case pwdResult of
    ShellCmd.Success output -> putStrLn $ "現在のディレクトリ: " ++ output
    ShellCmd.Error err -> putStrLn $ "エラー: " ++ show err
  
  -- psコマンドのデモ
  putStrLn "\n3. ps コマンドのデモ"
  psResult <- ShellCmd.ps []
  case psResult of
    ShellCmd.Success output -> putStrLn $ "プロセス一覧:\n" ++ take 500 output ++ "..."
    ShellCmd.Error err -> putStrLn $ "エラー: " ++ show err

-- | シェルパーサーのデモ
shellParserDemo :: IO ()
shellParserDemo = do
  putStrLn "\n========== シェルパーサーデモ =========="
  
  -- コマンドライン解析のデモ
  putStrLn "1. コマンドライン解析のデモ"
  let examples = 
        [ "ls -la"
        , "cat file.txt | grep pattern"
        , "echo hello > output.txt"
        , "find . -name '*.hs' | wc -l"
        ]
  
  mapM_ parseExample examples
  
  -- パーサーテストの実行
  putStrLn "\n2. 包括的パーサーテスト"
  ShellParser.runParserTests
  
  where
    parseExample cmd = do
      putStrLn $ "\n解析中: " ++ cmd
      case ShellParser.parseCommandLine cmd of
        Left err -> putStrLn $ "エラー: " ++ show err
        Right result -> putStrLn $ "成功: " ++ show result

-- | Linux基本概念のデモ
linuxCoreDemo :: IO ()
linuxCoreDemo = do
  putStrLn "\n========== Linux基本概念デモ =========="
  
  -- ユーザー情報のデモ
  putStrLn "1. ユーザー情報のデモ"
  let sampleUser = Core.User 1000 "john" 1000 "/home/john" "/bin/bash"
  putStrLn $ "ユーザー: " ++ show sampleUser
  
  -- ファイル権限のデモ
  putStrLn "\n2. ファイル権限のデモ"
  let samplePerms = Core.FilePermissions True True True True False True True False False
  putStrLn $ "権限: " ++ Core.showPermissions samplePerms
  putStrLn $ "実行可能: " ++ show (Core.isExecutable samplePerms)
  
  -- プロセス情報のデモ
  putStrLn "\n3. プロセス情報のデモ"
  let sampleProcess = Core.Process 1234 (Just 1) "example" Core.Running 1000 "/usr/bin/example"
  putStrLn $ "プロセス: " ++ show sampleProcess
  
  -- システム情報のデモ
  putStrLn "\n4. システム情報のデモ"
  let sampleSysInfo = Core.SystemInfo "Linux 5.4.0" 86400 (1.0, 0.8, 0.6) 5
  putStrLn $ "システム情報: " ++ show sampleSysInfo

-- | 継続を促すヘルパー関数
waitForContinue :: IO ()
waitForContinue = do
  putStrLn "\n続行するには Enter キーを押してください..."
  _ <- getLine
  return ()