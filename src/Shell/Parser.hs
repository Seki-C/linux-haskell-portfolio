{-|
Module      : Shell.Parser
Description : Linuxシェルコマンドのパーサー
Copyright   : (c) Portfolio, 2025
License     : MIT
Maintainer  : portfolio@example.com

Linuxシェルコマンドラインの解析を学習するためのモジュールです。
コマンドライン引数の解析、パイプライン、リダイレクションなどを理解できます。
-}

module Shell.Parser
  ( -- * コマンド構造
    Command(..)
  , Pipeline(..)
  , Redirection(..)
  , RedirectionType(..)
  , CommandLine(..)
    -- * パーサー
  , parseCommandLine
  , parseCommand
  , parsePipeline
  , parseRedirection
  , parseArguments
    -- * トークナイザー
  , Token(..)
  , tokenize
  , isSpecialChar
  , isQuoted
    -- * 展開
  , expandVariables
  , expandGlobs
  , expandTilde
  , expandBraces
    -- * エラー処理
  , ParseError(..)
  ) where

import Data.Char (isSpace, isAlphaNum)
import Data.List (intercalate)
import Text.Parsec hiding (ParseError)
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Control.Exception (Exception)
import qualified Data.Map as Map

-- | パースエラー型
data ParseError
  = SyntaxError String
  | UnterminatedQuote String
  | InvalidRedirection String
  | UnknownVariable String
  | ExpansionError String
  deriving (Show, Eq)

instance Exception ParseError

-- | コマンド
data Command = Command
  { cmdName :: String           -- ^ コマンド名
  , cmdArgs :: [String]         -- ^ 引数リスト
  , cmdEnv  :: Map.Map String String  -- ^ 環境変数
  } deriving (Show, Eq)

-- | リダイレクションタイプ
data RedirectionType
  = InputRedirect      -- ^ < (標準入力リダイレクト)
  | OutputRedirect     -- ^ > (標準出力リダイレクト)
  | AppendRedirect     -- ^ >> (標準出力追記リダイレクト)
  | ErrorRedirect      -- ^ 2> (標準エラーリダイレクト)
  | ErrorAppendRedirect -- ^ 2>> (標準エラー追記リダイレクト)
  | BothRedirect       -- ^ &> (標準出力と標準エラーリダイレクト)
  | HereDoc           -- ^ << (ヒアドキュメント)
  | HereString        -- ^ <<< (ヒアストリング)
  deriving (Show, Eq)

-- | リダイレクション
data Redirection = Redirection
  { redirType :: RedirectionType
  , redirTarget :: String
  } deriving (Show, Eq)

-- | パイプライン
data Pipeline = Pipeline
  { pipeCommands :: [Command]   -- ^ パイプで接続されたコマンド列
  , pipeRedirections :: [Redirection]  -- ^ リダイレクション
  } deriving (Show, Eq)

-- | コマンドライン
data CommandLine = CommandLine
  { cmdPipelines :: [Pipeline]  -- ^ パイプライン列（; で区切られた）
  , cmdBackground :: Bool       -- ^ バックグラウンド実行（& で終わる）
  } deriving (Show, Eq)

-- | トークン
data Token
  = Word String                 -- ^ 単語
  | Pipe                        -- ^ |
  | Semicolon                   -- ^ ;
  | Ampersand                   -- ^ &
  | LeftParen                   -- ^ (
  | RightParen                  -- ^ )
  | InputRedir                  -- ^ <
  | OutputRedir                 -- ^ >
  | AppendRedir                 -- ^ >>
  | ErrorRedir                  -- ^ 2>
  | ErrorAppendRedir            -- ^ 2>>
  | BothRedir                   -- ^ &>
  | HereDocRedir                -- ^ <<
  | HereStringRedir             -- ^ <<<
  | Variable String             -- ^ $VAR
  | QuotedString String         -- ^ "string" or 'string'
  | EOF                         -- ^ ファイル終端
  deriving (Show, Eq)

-- | コマンドラインをパース
parseCommandLine :: String -> Either ParseError CommandLine
parseCommandLine input = 
  case parse commandLineParser "shell" input of
    Left err -> Left (SyntaxError $ show err)
    Right result -> Right result

-- | コマンドラインパーサー
commandLineParser :: Parser CommandLine
commandLineParser = do
  pipelines <- pipelineParser `sepBy` (char ';' >> spaces)
  background <- option False (char '&' >> spaces >> return True)
  eof
  return $ CommandLine pipelines background

-- | パイプラインパーサー
pipelineParser :: Parser Pipeline
pipelineParser = do
  spaces
  commands <- commandParser `sepBy1` (char '|' >> spaces)
  redirs <- many redirectionParser
  return $ Pipeline commands redirs

-- | コマンドパーサー
commandParser :: Parser Command
commandParser = do
  spaces
  name <- wordParser
  args <- many (spaces >> wordParser)
  return $ Command name args Map.empty

-- | リダイレクションパーサー
redirectionParser :: Parser Redirection
redirectionParser = do
  spaces
  redirType <- choice
    [ try (string "2>>") >> return ErrorAppendRedirect
    , try (string "2>") >> return ErrorRedirect
    , try (string ">>") >> return AppendRedirect
    , try (string "<<<") >> return HereString
    , try (string "<<") >> return HereDoc
    , try (string "&>") >> return BothRedirect
    , char '>' >> return OutputRedirect
    , char '<' >> return InputRedirect
    ]
  spaces
  target <- wordParser
  return $ Redirection redirType target

-- | 単語パーサー
wordParser :: Parser String
wordParser = choice
  [ quotedStringParser
  , unquotedWordParser
  ]

-- | クォート文字列パーサー
quotedStringParser :: Parser String
quotedStringParser = choice
  [ between (char '"') (char '"') (many (noneOf "\""))
  , between (char '\'') (char '\'') (many (noneOf "'"))
  ]

-- | 非クォート単語パーサー
unquotedWordParser :: Parser String
unquotedWordParser = many1 (noneOf " \t\n|;&<>()\"'")

-- | コマンドをパース
parseCommand :: String -> Either ParseError Command
parseCommand input =
  case parse commandParser "command" input of
    Left err -> Left (SyntaxError $ show err)
    Right result -> Right result

-- | パイプラインをパース
parsePipeline :: String -> Either ParseError Pipeline
parsePipeline input =
  case parse pipelineParser "pipeline" input of
    Left err -> Left (SyntaxError $ show err)
    Right result -> Right result

-- | リダイレクションをパース
parseRedirection :: String -> Either ParseError Redirection
parseRedirection input =
  case parse redirectionParser "redirection" input of
    Left err -> Left (SyntaxError $ show err)
    Right result -> Right result

-- | 引数をパース
parseArguments :: String -> Either ParseError [String]
parseArguments input =
  case parse (many wordParser) "arguments" input of
    Left err -> Left (SyntaxError $ show err)
    Right result -> Right result

-- | 文字列をトークナイズ
tokenize :: String -> [Token]
tokenize [] = [EOF]
tokenize input = 
  case parseTokens input of
    Right tokens -> tokens
    Left _ -> [EOF]

-- | トークンパーサー
parseTokens :: String -> Either ParseError [Token]
parseTokens input =
  case parse tokensParser "tokens" input of
    Left err -> Left (SyntaxError $ show err)
    Right result -> Right result

-- | トークン列パーサー
tokensParser :: Parser [Token]
tokensParser = do
  tokens <- many tokenParser
  eof
  return $ tokens ++ [EOF]

-- | 単一トークンパーサー
tokenParser :: Parser Token
tokenParser = do
  spaces
  choice
    [ try (string "2>>") >> return ErrorAppendRedir
    , try (string "2>") >> return ErrorRedir
    , try (string ">>") >> return AppendRedir
    , try (string "<<<") >> return HereStringRedir
    , try (string "<<") >> return HereDocRedir
    , try (string "&>") >> return BothRedir
    , char '|' >> return Pipe
    , char ';' >> return Semicolon
    , char '&' >> return Ampersand
    , char '(' >> return LeftParen
    , char ')' >> return RightParen
    , char '<' >> return InputRedir
    , char '>' >> return OutputRedir
    , char '$' >> variableParser
    , quotedStringParser >>= return . QuotedString
    , unquotedWordParser >>= return . Word
    ]

-- | 変数パーサー
variableParser :: Parser Token
variableParser = do
  name <- choice
    [ between (char '{') (char '}') (many1 alphaNum)
    , many1 alphaNum
    ]
  return $ Variable name

-- | 特殊文字かどうかチェック
isSpecialChar :: Char -> Bool
isSpecialChar c = c `elem` "|;&<>()\"'$"

-- | クォートされた文字列かどうかチェック
isQuoted :: String -> Bool
isQuoted str = 
  (length str >= 2) && 
  ((head str == '"' && last str == '"') || 
   (head str == '\'' && last str == '\''))

-- | 変数展開
expandVariables :: Map.Map String String -> String -> Either ParseError String
expandVariables env str = expandVars str
  where
    expandVars [] = Right []
    expandVars ('$':'{':rest) = 
      case break (== '}') rest of
        (varName, '}':remaining) -> do
          value <- case Map.lookup varName env of
            Just v -> Right v
            Nothing -> Left (UnknownVariable varName)
          remainingExpanded <- expandVars remaining
          return $ value ++ remainingExpanded
        _ -> Left (SyntaxError "Unterminated variable expansion")
    expandVars ('$':rest) = 
      let (varName, remaining) = span isAlphaNum rest
      in if null varName
         then do
           remainingExpanded <- expandVars rest
           return $ '$' : remainingExpanded
         else do
           value <- case Map.lookup varName env of
             Just v -> Right v
             Nothing -> Left (UnknownVariable varName)
           remainingExpanded <- expandVars remaining
           return $ value ++ remainingExpanded
    expandVars (c:rest) = do
      remainingExpanded <- expandVars rest
      return $ c : remainingExpanded

-- | グロブ展開（簡易実装）
expandGlobs :: String -> IO [String]
expandGlobs pattern = do
  -- 実際の実装では globbing ライブラリを使用
  -- ここでは簡易的な実装
  if '*' `elem` pattern || '?' `elem` pattern
    then return [pattern ++ ".example1", pattern ++ ".example2"]  -- 仮の展開結果
    else return [pattern]

-- | チルダ展開
expandTilde :: String -> IO String
expandTilde ('~':rest) = do
  -- 実際の実装では getHomeDirectory を使用
  return $ "/home/user" ++ rest
expandTilde str = return str

-- | ブレース展開
expandBraces :: String -> [String]
expandBraces str = 
  if '{' `elem` str && '}' `elem` str
    then expandBracePattern str
    else [str]

-- | ブレースパターン展開
expandBracePattern :: String -> [String]
expandBracePattern str = 
  case findBraceGroup str of
    Nothing -> [str]
    Just (prefix, options, suffix) -> 
      [prefix ++ option ++ suffix | option <- options]

-- | ブレースグループを検索
findBraceGroup :: String -> Maybe (String, [String], String)
findBraceGroup str = 
  case break (== '{') str of
    (prefix, '{':rest) -> 
      case break (== '}') rest of
        (content, '}':suffix) -> 
          Just (prefix, splitOn ',' content, suffix)
        _ -> Nothing
    _ -> Nothing

-- | 文字で分割
splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn delim str = 
  case break (== delim) str of
    (chunk, []) -> [chunk]
    (chunk, _:rest) -> chunk : splitOn delim rest

-- * 高レベルな解析機能

-- | コマンドライン全体を解析
analyzeCommandLine :: String -> IO (Either ParseError CommandLine)
analyzeCommandLine input = do
  case parseCommandLine input of
    Left err -> return $ Left err
    Right cmdLine -> do
      -- 各パイプラインの展開処理
      expandedPipelines <- mapM expandPipeline (cmdPipelines cmdLine)
      return $ Right $ cmdLine { cmdPipelines = expandedPipelines }

-- | パイプラインを展開
expandPipeline :: Pipeline -> IO Pipeline
expandPipeline pipeline = do
  expandedCommands <- mapM expandCommand (pipeCommands pipeline)
  return $ pipeline { pipeCommands = expandedCommands }

-- | コマンドを展開
expandCommand :: Command -> IO Command
expandCommand cmd = do
  -- 引数のチルダ展開とグロブ展開
  expandedArgs <- mapM expandArgument (cmdArgs cmd)
  let flattenedArgs = concat expandedArgs
  return $ cmd { cmdArgs = flattenedArgs }

-- | 引数を展開
expandArgument :: String -> IO [String]
expandArgument arg = do
  tildeExpanded <- expandTilde arg
  globExpanded <- expandGlobs tildeExpanded
  return $ concatMap expandBraces globExpanded

-- * デバッグとヘルパー機能

-- | コマンドラインを文字列に変換
formatCommandLine :: CommandLine -> String
formatCommandLine cmdLine = 
  let pipelineStrs = map formatPipeline (cmdPipelines cmdLine)
      joined = intercalate "; " pipelineStrs
  in if cmdBackground cmdLine then joined ++ " &" else joined

-- | パイプラインを文字列に変換
formatPipeline :: Pipeline -> String
formatPipeline pipeline = 
  let commandStrs = map formatCommand (pipeCommands pipeline)
      redirStrs = map formatRedirection (pipeRedirections pipeline)
  in intercalate " | " commandStrs ++ concatMap (" " ++) redirStrs

-- | コマンドを文字列に変換
formatCommand :: Command -> String
formatCommand cmd = intercalate " " (cmdName cmd : cmdArgs cmd)

-- | リダイレクションを文字列に変換
formatRedirection :: Redirection -> String
formatRedirection redir = 
  let typeStr = case redirType redir of
        InputRedirect -> "<"
        OutputRedirect -> ">"
        AppendRedirect -> ">>"
        ErrorRedirect -> "2>"
        ErrorAppendRedirect -> "2>>"
        BothRedirect -> "&>"
        HereDoc -> "<<"
        HereString -> "<<<"
  in typeStr ++ " " ++ redirTarget redir

-- | パーサーのテスト例
testExamples :: [String]
testExamples = 
  [ "ls -la"
  , "cat file.txt | grep pattern | sort"
  , "command > output.txt 2> error.txt"
  , "find . -name '*.hs' | xargs grep 'main'"
  , "echo 'Hello, World!' && echo 'Done'"
  , "ls; pwd; whoami"
  , "(cd /tmp && ls) | wc -l"
  , "export VAR=value; echo $VAR"
  ]

-- | テスト実行
runParserTests :: IO ()
runParserTests = do
  putStrLn "シェルパーサーテスト実行中..."
  mapM_ testParse testExamples
  where
    testParse example = do
      putStrLn $ "\n入力: " ++ example
      case parseCommandLine example of
        Left err -> putStrLn $ "エラー: " ++ show err
        Right result -> putStrLn $ "成功: " ++ show result