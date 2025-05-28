# Linux OS ポートフォリオ (Haskell)

Linux オペレーティングシステムの基本概念をHaskellで学ぶ教育的ポートフォリオです。

## 概要

このプロジェクトは、Linux OSの主要な概念と機能をHaskellのコードを通じて理解することを目的としています。関数型プログラミングのパラダイムを使用して、システムプログラミングの基礎を学びます。

## 学習内容

### 1. ファイルシステム
- ファイル操作とディレクトリ構造
- パーミッション管理
- パス操作

### 2. プロセス管理
- プロセスの作成と管理
- シグナル処理
- プロセス間通信（IPC）

### 3. ネットワーク
- ソケットプログラミング
- TCP/UDP通信
- ネットワーク設定

### 4. システムコール
- システムコールの理解
- I/O操作
- メモリ管理

### 5. シェルコマンド
- 基本的なシェルコマンドの実装
- パイプライン処理
- リダイレクション

## プロジェクト構造

```
linux-haskell-portfolio/
├── src/
│   ├── FileSystem/
│   ├── Process/
│   ├── Network/
│   ├── System/
│   └── Shell/
├── app/
├── test/
├── docs/
└── examples/
```

## 要件

- GHC (Glasgow Haskell Compiler) 8.10以上
- Cabal 3.0以上
- Stack（推奨）

## ビルドと実行

```bash
# プロジェクトのビルド
cabal build

# テストの実行
cabal test

# アプリケーションの実行
cabal run linux-portfolio
```

## 学習方法

1. 各モジュールのソースコードを読む
2. 実装されている機能をテストする
3. ドキュメントでLinuxの概念を理解する
4. 自分で機能を拡張してみる

## ライセンス

MIT License

## 作成者

Linux OSとHaskellの学習を目的とした教育的プロジェクト

## Claude PR Assistant

このプロジェクトにはClaude PR Assistantが設定されており、PRでの@claudeメンションによるコードレビューとサポートが利用できます。