# Linux OS学習ガイド

このドキュメントでは、Linux OSポートフォリオを使った効果的な学習方法について説明します。

## 学習の進め方

### 1. 基本概念から始める

まず [`Linux.Core`](../src/Linux/Core.hs) モジュールから学習を開始してください。
このモジュールには以下の基本概念が含まれています：

- **ユーザー管理**: [`User`](../src/Linux/Core.hs:25), [`Group`](../src/Linux/Core.hs:33)
- **ファイル権限**: [`FilePermissions`](../src/Linux/Core.hs:39)
- **プロセス**: [`Process`](../src/Linux/Core.hs:76), [`ProcessState`](../src/Linux/Core.hs:69)
- **システム情報**: [`SystemInfo`](../src/Linux/Core.hs:85), [`MemoryInfo`](../src/Linux/Core.hs:92)

### 2. ファイルシステムの理解

次に、ファイルシステム関連のモジュールを学習します：

#### ファイル操作 ([`FileSystem.Operations`](../src/FileSystem/Operations.hs))
- [`createFile()`](../src/FileSystem/Operations.hs:72), [`deleteFile()`](../src/FileSystem/Operations.hs:80)
- [`copyFile'()`](../src/FileSystem/Operations.hs:89), [`moveFile'()`](../src/FileSystem/Operations.hs:102)
- [`readFileContent()`](../src/FileSystem/Operations.hs:115), [`writeFileContent()`](../src/FileSystem/Operations.hs:123)

#### 権限管理 ([`FileSystem.Permissions`](../src/FileSystem/Permissions.hs))
- [`permissionsToOctal()`](../src/FileSystem/Permissions.hs:67), [`octalToPermissions()`](../src/FileSystem/Permissions.hs:77)
- [`addPermission()`](../src/FileSystem/Permissions.hs:109), [`removePermission()`](../src/FileSystem/Permissions.hs:119)

### 3. プロセス管理の学習

プロセス関連のモジュールで、Linuxの重要な概念を学習します：

#### プロセス管理 ([`Process.Management`](../src/Process/Management.hs))
- [`listProcesses()`](../src/Process/Management.hs:90), [`getProcessInfo()`](../src/Process/Management.hs:102)
- [`killProcess()`](../src/Process/Management.hs:117), [`sendSignal()`](../src/Process/Management.hs:124)

#### プロセス間通信 ([`Process.Communication`](../src/Process/Communication.hs))
- パイプ: [`createPipe()`](../src/Process/Communication.hs:96), [`readPipe()`](../src/Process/Communication.hs:102)
- 共有メモリ: [`createSharedMemory()`](../src/Process/Communication.hs:138), [`attachSharedMemory()`](../src/Process/Communication.hs:145)

### 4. ネットワークプログラミング

ネットワーク関連のモジュールで、通信の仕組みを理解します：

#### ソケットプログラミング ([`Network.Socket`](../src/Network/Socket.hs))
- [`createSocket()`](../src/Network/Socket.hs:108), [`bindSocket()`](../src/Network/Socket.hs:119)
- [`sendData()`](../src/Network/Socket.hs:175), [`receiveData()`](../src/Network/Socket.hs:181)

#### クライアント実装 ([`Network.Client`](../src/Network/Client.hs))
- [`httpGet()`](../src/Network/Client.hs:111), [`connectTcp()`](../src/Network/Client.hs:134)

### 5. システムコールの理解

低レベルなシステムインターフェースを学習します：

#### システムコール ([`System.Calls`](../src/System/Calls.hs))
- [`open'()`](../src/System/Calls.hs:116), [`read'()`](../src/System/Calls.hs:130), [`write'()`](../src/System/Calls.hs:138)
- [`fork'()`](../src/System/Calls.hs:150), [`exec'()`](../src/System/Calls.hs:158)

### 6. メモリ管理

メモリ関連の概念を深く理解します：

#### メモリ管理 ([`System.Memory`](../src/System/Memory.hs))
- [`getMemoryInfo()`](../src/System/Memory.hs:141), [`allocateMemory()`](../src/System/Memory.hs:171)
- [`mapMemory()`](../src/System/Memory.hs:226), [`createMemoryPool()`](../src/System/Memory.hs:253)

### 7. シェルとコマンドライン

最後に、シェルの仕組みを理解します：

#### シェルコマンド ([`Shell.Commands`](../src/Shell/Commands.hs))
- [`ls()`](../src/Shell/Commands.hs:70), [`cat()`](../src/Shell/Commands.hs:79), [`grep()`](../src/Shell/Commands.hs:168)

#### シェルパーサー ([`Shell.Parser`](../src/Shell/Parser.hs))
- [`parseCommandLine()`](../src/Shell/Parser.hs:78), [`expandVariables()`](../src/Shell/Parser.hs:235)

## 実践的な学習方法

### 1. コードを読む
各モジュールのソースコードを詳しく読み、関数の実装方法を理解してください。

### 2. 実際に動かす
```bash
cabal run linux-portfolio
```
を実行して、対話的メニューで各機能を試してください。

### 3. テストを実行する
```bash
cabal test
```
でテストを実行し、各機能の動作を確認してください。

### 4. コードを拡張する
既存のコードを基に、新しい機能を追加してみてください。

## 学習のポイント

### Haskellの関数型プログラミング
- **純粋関数**: 副作用のない関数の書き方
- **モナド**: `IO`モナドを使った副作用の扱い
- **エラーハンドリング**: `Either`型を使ったエラー処理

### Linuxシステムプログラミング
- **システムコール**: OSとの低レベルなインターフェース
- **ファイルディスクリプタ**: ファイルやソケットの抽象化
- **プロセス管理**: プロセスの生成、監視、制御

### ネットワークプログラミング
- **ソケット**: ネットワーク通信の基本
- **プロトコル**: TCP/UDPの違いと使い分け
- **クライアント/サーバー**: 通信アーキテクチャの理解

## 参考資料

### 書籍
- "Advanced Programming in the UNIX Environment" by W. Richard Stevens
- "Linux Programming Interface" by Michael Kerrisk
- "Real World Haskell" by Bryan O'Sullivan

### オンラインリソース
- [Linux man pages](https://man7.org/linux/man-pages/)
- [Haskell.org](https://www.haskell.org/)
- [Learn You a Haskell](http://learnyouahaskell.com/)

## トラブルシューティング

### コンパイルエラー
```bash
cabal clean
cabal update
cabal build
```

### 依存関係の問題
```bash
cabal freeze
cabal build --dependencies-only
```

### テストの失敗
個別のテストを実行して問題を特定：
```bash
cabal test --test-show-details=direct
```

## 次のステップ

このポートフォリオを完了したら、以下のような発展的な学習に進むことができます：

1. **実際のLinux環境での開発**
2. **システムプログラミングの実践**
3. **ネットワークサーバーの実装**
4. **Haskellでの並行プログラミング**
5. **組み込みシステム開発**

継続的な学習と実践を通じて、Linuxシステムプログラミングの理解を深めてください。