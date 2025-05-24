{-|
Module      : FileSystem.Permissions
Description : Linuxファイル権限の管理と操作
Copyright   : (c) Portfolio, 2025
License     : MIT
Maintainer  : portfolio@example.com

Linuxファイルシステムの権限管理を学習するためのモジュールです。
chmod、chown、umaskなどの権限操作について理解できます。
-}

module FileSystem.Permissions
  ( -- * 権限表現
    Permission(..)
  , PermissionSet(..)
  , OctalPermission
    -- * 権限変換
  , permissionsToOctal
  , octalToPermissions
  , stringToPermissions
  , permissionsToString
    -- * 権限操作
  , addPermission
  , removePermission
  , hasPermission
  , setOwnerPermissions
  , setGroupPermissions
  , setOtherPermissions
    -- * 特殊権限
  , SpecialPermission(..)
  , hasSpecialPermission
  , setSpecialPermission
    -- * umask操作
  , UMask(..)
  , applyUMask
  , defaultUMask
    -- * 権限チェック
  , canRead
  , canWrite
  , canExecute
  , canAccess
    -- * ユーティリティ
  , commonPermissions
  , securePermissions
  , publicPermissions
  ) where

import Linux.Core (FilePermissions(..), UserID, GroupID)
import Data.Bits ((.&.), (.|.), complement, testBit, setBit, clearBit)
import Data.Word (Word16)

-- | 基本的な権限タイプ
data Permission 
  = Read     -- ^ 読み取り権限
  | Write    -- ^ 書き込み権限
  | Execute  -- ^ 実行権限
  deriving (Show, Eq, Enum, Bounded)

-- | 権限の対象
data PermissionSet
  = Owner    -- ^ 所有者
  | Group    -- ^ グループ
  | Other    -- ^ その他
  deriving (Show, Eq, Enum, Bounded)

-- | 8進数表現の権限（例: 755, 644）
type OctalPermission = Word16

-- | 特殊権限
data SpecialPermission
  = SetUID   -- ^ Set User ID
  | SetGID   -- ^ Set Group ID
  | StickyBit -- ^ スティッキービット
  deriving (Show, Eq, Enum, Bounded)

-- | umask値
newtype UMask = UMask OctalPermission deriving (Show, Eq)

-- | FilePermissionsを8進数表現に変換
permissionsToOctal :: FilePermissions -> OctalPermission
permissionsToOctal perms = fromIntegral $
  (if ownerRead perms then 4 else 0) * 100 +
  (if ownerWrite perms then 2 else 0) * 100 +
  (if ownerExecute perms then 1 else 0) * 100 +
  (if groupRead perms then 4 else 0) * 10 +
  (if groupWrite perms then 2 else 0) * 10 +
  (if groupExecute perms then 1 else 0) * 10 +
  (if otherRead perms then 4 else 0) +
  (if otherWrite perms then 2 else 0) +
  (if otherExecute perms then 1 else 0)

-- | 8進数表現をFilePermissionsに変換
octalToPermissions :: OctalPermission -> FilePermissions
octalToPermissions octal = 
  let owner = fromIntegral $ octal `div` 100
      group = fromIntegral $ (octal `div` 10) `mod` 10
      other = fromIntegral $ octal `mod` 10
  in FilePermissions
    { ownerRead = testBit owner 2
    , ownerWrite = testBit owner 1
    , ownerExecute = testBit owner 0
    , groupRead = testBit group 2
    , groupWrite = testBit group 1
    , groupExecute = testBit group 0
    , otherRead = testBit other 2
    , otherWrite = testBit other 1
    , otherExecute = testBit other 0
    }

-- | 文字列表現から権限に変換（例: "rwxr-xr--"）
stringToPermissions :: String -> Maybe FilePermissions
stringToPermissions str
  | length str /= 9 = Nothing
  | otherwise = Just $ FilePermissions
      { ownerRead = str !! 0 == 'r'
      , ownerWrite = str !! 1 == 'w'
      , ownerExecute = str !! 2 == 'x'
      , groupRead = str !! 3 == 'r'
      , groupWrite = str !! 4 == 'w'
      , groupExecute = str !! 5 == 'x'
      , otherRead = str !! 6 == 'r'
      , otherWrite = str !! 7 == 'w'
      , otherExecute = str !! 8 == 'x'
      }

-- | 権限を文字列表現に変換
permissionsToString :: FilePermissions -> String
permissionsToString perms = 
  [if ownerRead perms then 'r' else '-'] ++
  [if ownerWrite perms then 'w' else '-'] ++
  [if ownerExecute perms then 'x' else '-'] ++
  [if groupRead perms then 'r' else '-'] ++
  [if groupWrite perms then 'w' else '-'] ++
  [if groupExecute perms then 'x' else '-'] ++
  [if otherRead perms then 'r' else '-'] ++
  [if otherWrite perms then 'w' else '-'] ++
  [if otherExecute perms then 'x' else '-']

-- | 権限を追加
addPermission :: PermissionSet -> Permission -> FilePermissions -> FilePermissions
addPermission Owner Read perms = perms { ownerRead = True }
addPermission Owner Write perms = perms { ownerWrite = True }
addPermission Owner Execute perms = perms { ownerExecute = True }
addPermission Group Read perms = perms { groupRead = True }
addPermission Group Write perms = perms { groupWrite = True }
addPermission Group Execute perms = perms { groupExecute = True }
addPermission Other Read perms = perms { otherRead = True }
addPermission Other Write perms = perms { otherWrite = True }
addPermission Other Execute perms = perms { otherExecute = True }

-- | 権限を削除
removePermission :: PermissionSet -> Permission -> FilePermissions -> FilePermissions
removePermission Owner Read perms = perms { ownerRead = False }
removePermission Owner Write perms = perms { ownerWrite = False }
removePermission Owner Execute perms = perms { ownerExecute = False }
removePermission Group Read perms = perms { groupRead = False }
removePermission Group Write perms = perms { groupWrite = False }
removePermission Group Execute perms = perms { groupExecute = False }
removePermission Other Read perms = perms { otherRead = False }
removePermission Other Write perms = perms { otherWrite = False }
removePermission Other Execute perms = perms { otherExecute = False }

-- | 権限があるかチェック
hasPermission :: PermissionSet -> Permission -> FilePermissions -> Bool
hasPermission Owner Read perms = ownerRead perms
hasPermission Owner Write perms = ownerWrite perms
hasPermission Owner Execute perms = ownerExecute perms
hasPermission Group Read perms = groupRead perms
hasPermission Group Write perms = groupWrite perms
hasPermission Group Execute perms = groupExecute perms
hasPermission Other Read perms = otherRead perms
hasPermission Other Write perms = otherWrite perms
hasPermission Other Execute perms = otherExecute perms

-- | 所有者権限を設定
setOwnerPermissions :: Bool -> Bool -> Bool -> FilePermissions -> FilePermissions
setOwnerPermissions r w x perms = perms
  { ownerRead = r
  , ownerWrite = w
  , ownerExecute = x
  }

-- | グループ権限を設定
setGroupPermissions :: Bool -> Bool -> Bool -> FilePermissions -> FilePermissions
setGroupPermissions r w x perms = perms
  { groupRead = r
  , groupWrite = w
  , groupExecute = x
  }

-- | その他権限を設定
setOtherPermissions :: Bool -> Bool -> Bool -> FilePermissions -> FilePermissions
setOtherPermissions r w x perms = perms
  { otherRead = r
  , otherWrite = w
  , otherExecute = x
  }

-- | 特殊権限があるかチェック（プレースホルダー）
hasSpecialPermission :: SpecialPermission -> FilePermissions -> Bool
hasSpecialPermission _ _ = False  -- 簡易実装

-- | 特殊権限を設定（プレースホルダー）
setSpecialPermission :: SpecialPermission -> Bool -> FilePermissions -> FilePermissions
setSpecialPermission _ _ perms = perms  -- 簡易実装

-- | umaskを適用
applyUMask :: UMask -> FilePermissions -> FilePermissions
applyUMask (UMask mask) perms = 
  let maskPerms = octalToPermissions mask
      applyMask current maskBit = current && not maskBit
  in FilePermissions
    { ownerRead = applyMask (ownerRead perms) (ownerRead maskPerms)
    , ownerWrite = applyMask (ownerWrite perms) (ownerWrite maskPerms)
    , ownerExecute = applyMask (ownerExecute perms) (ownerExecute maskPerms)
    , groupRead = applyMask (groupRead perms) (groupRead maskPerms)
    , groupWrite = applyMask (groupWrite perms) (groupWrite maskPerms)
    , groupExecute = applyMask (groupExecute perms) (groupExecute maskPerms)
    , otherRead = applyMask (otherRead perms) (otherRead maskPerms)
    , otherWrite = applyMask (otherWrite perms) (otherWrite maskPerms)
    , otherExecute = applyMask (otherExecute perms) (otherExecute maskPerms)
    }

-- | デフォルトのumask値（022）
defaultUMask :: UMask
defaultUMask = UMask 022

-- | ユーザーがファイルを読めるかチェック
canRead :: UserID -> GroupID -> FilePermissions -> UserID -> GroupID -> Bool
canRead fileOwner fileGroup perms userId userGroup
  | userId == fileOwner = ownerRead perms
  | userGroup == fileGroup = groupRead perms
  | otherwise = otherRead perms

-- | ユーザーがファイルに書き込めるかチェック
canWrite :: UserID -> GroupID -> FilePermissions -> UserID -> GroupID -> Bool
canWrite fileOwner fileGroup perms userId userGroup
  | userId == fileOwner = ownerWrite perms
  | userGroup == fileGroup = groupWrite perms
  | otherwise = otherWrite perms

-- | ユーザーがファイルを実行できるかチェック
canExecute :: UserID -> GroupID -> FilePermissions -> UserID -> GroupID -> Bool
canExecute fileOwner fileGroup perms userId userGroup
  | userId == fileOwner = ownerExecute perms
  | userGroup == fileGroup = groupExecute perms
  | otherwise = otherExecute perms

-- | ユーザーがファイルにアクセスできるかチェック
canAccess :: UserID -> GroupID -> FilePermissions -> UserID -> GroupID -> (Bool, Bool, Bool)
canAccess fileOwner fileGroup perms userId userGroup = 
  ( canRead fileOwner fileGroup perms userId userGroup
  , canWrite fileOwner fileGroup perms userId userGroup
  , canExecute fileOwner fileGroup perms userId userGroup
  )

-- | よく使われる権限設定
commonPermissions :: [(String, FilePermissions)]
commonPermissions =
  [ ("644", octalToPermissions 644)  -- 一般的なファイル
  , ("755", octalToPermissions 755)  -- 実行可能ファイル
  , ("600", octalToPermissions 600)  -- プライベートファイル
  , ("700", octalToPermissions 700)  -- プライベート実行ファイル
  , ("666", octalToPermissions 666)  -- 全員読み書き可能
  , ("777", octalToPermissions 777)  -- 全権限
  ]

-- | セキュアな権限設定
securePermissions :: FilePermissions
securePermissions = octalToPermissions 600

-- | パブリックな権限設定
publicPermissions :: FilePermissions
publicPermissions = octalToPermissions 644