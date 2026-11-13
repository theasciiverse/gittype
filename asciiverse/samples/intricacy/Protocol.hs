-- This file is part of Intricacy
-- Copyright (C) 2013-2025 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

module Protocol where

import           Control.Monad
import           Data.Binary

import           Crypto.PubKey.RSA.Types (PublicKey)

import           BinaryInstances
import           Lock
import           Metagame

type ProtocolVersion = Int
protocolVersion = 1 :: ProtocolVersion

data ClientRequest = ClientRequest ProtocolVersion (Maybe Auth) Action
    deriving (Eq, Ord, Show, Read)

type VersionedUInfo = (Int, UserInfo)

data Action
    = Authenticate
    | Register
    | ResetPassword Password
    | SetEmail String
    | GetServerInfo
    | GetPublicKey
    | GetLock LockSpec
    | GetRetired Codename
    | GetUserInfo Codename (Maybe Int)
    | GetHint NoteInfo
    | GetSolution NoteInfo
    | DeclareSolution Solution LockSpec ActiveLock LockIndex
    | SetLock Lock LockIndex Solution
    | GetRandomNames Int
    | RetireLock ActiveLock
    | UndefinedAction
    deriving (Eq, Ord, Show, Read)

data Auth = Auth {authUser :: Codename, authPasswd :: Password}
    deriving (Eq, Ord, Show, Read)
type Password = String

needsAuth :: Action -> Bool
needsAuth GetServerInfo      = False
needsAuth GetPublicKey       = False
needsAuth (GetLock _)        = False
needsAuth (GetUserInfo _ _)  = False
needsAuth (GetRetired _)     = False
needsAuth (GetRandomNames _) = False
needsAuth _                  = True

data ServerResponse
    = ServerAck
    | ServerMessage String
    | ServerError String
    | ServedServerInfo ServerInfo
    | ServedPublicKey PublicKey
    | ServedLock Lock
    | ServedRetired [LockSpec]
    | ServedUserInfo VersionedUInfo
    | ServedUserInfoDeltas [UserInfoDelta]
    | ServedSolution Solution
    | ServedHint Hint
    | ServedRandomNames [Codename]
    | ServerCodenameFree
    | ServerFresh
    | ServerUndefinedResponse
    deriving (Eq, Show, Read)

data ServerInfo = ServerInfo {serverLockSize :: Int, serverInfoString::String}
    deriving (Eq, Ord, Show, Read)
defaultServerInfo locksize = ServerInfo locksize ""

instance Binary ClientRequest where
    put (ClientRequest pv mauth act) = putPackedInt pv >> put mauth >> put act
    get = liftM3 ClientRequest getPackedInt get get

instance Binary Action where
    put Authenticate = put (0::Word8)
    put Register = put (1::Word8)
    put GetServerInfo = put (2::Word8)
    put (GetLock lspec) = put (3::Word8) >> put lspec
    put (GetUserInfo name version) = put (4::Word8) >> put name >> put version
    put (GetHint lspec) = put (5::Word8) >> put lspec
    put (GetSolution lspec) = put (6::Word8) >> put lspec
    put (DeclareSolution soln lspec alock idx) = put (7::Word8) >> put soln >> put lspec >> put alock >> put idx
    put (SetLock lock li soln) = put (8::Word8) >> put lock >> put li >> put soln
    put (GetRandomNames n) = put (9::Word8) >> put n
    put (ResetPassword pw) = put (10::Word8) >> put pw
    put (GetRetired name) = put (11::Word8) >> put name
    put (SetEmail address) = put (12::Word8) >> put address
    put GetPublicKey = put (13::Word8)
    put (RetireLock alock) = put (14::Word8) >> put alock
    put UndefinedAction = error "Tried to put UndefinedAction"
    get = do
        tag <- get :: Get Word8
        case tag of
            0  -> return Authenticate
            1  -> return Register
            2  -> return GetServerInfo
            3  -> GetLock <$> get
            4  -> liftM2 GetUserInfo get get
            5  -> GetHint <$> get
            6  -> GetSolution <$> get
            7  -> liftM4 DeclareSolution get get get get
            8  -> liftM3 SetLock get get get
            9  -> GetRandomNames <$> get
            10 -> ResetPassword <$> get
            11 -> GetRetired <$> get
            12 -> SetEmail <$> get
            13 -> return GetPublicKey
            14 -> RetireLock <$> get
            _  -> return UndefinedAction

instance Binary Auth where
    put (Auth name pw) = put name >> put pw
    get = liftM2 Auth get get

instance Binary ServerResponse where
    put ServerAck                   = put (0::Word8)
    put (ServerMessage mesg)        = put (1::Word8) >> put mesg
    put (ServerError err)           = put (2::Word8) >> put err
    put (ServedServerInfo sinfo)    = put (3::Word8) >> put sinfo
    put (ServedLock lock)           = put (4::Word8) >> put lock
    put (ServedUserInfo info)       = put (5::Word8) >> put info
    put (ServedUserInfoDeltas info) = put (6::Word8) >> put info
    put (ServedSolution soln)       = put (7::Word8) >> put soln
    put (ServedHint hint)           = put (8::Word8) >> put hint
    put (ServedRandomNames names)   = put (9::Word8) >> put names
    put ServerCodenameFree          = put (10::Word8)
    put ServerFresh                 = put (11::Word8)
    put (ServedRetired lss)         = put (12::Word8) >> put lss
    put (ServedPublicKey publicKey) = put (13::Word8) >> put (show publicKey)
    put ServerUndefinedResponse = error "Tried to put ServerUndefinedResponse"
    get = do
        tag <- get :: Get Word8
        case tag of
            0  -> return ServerAck
            1  -> ServerMessage <$> get
            2  -> ServerError <$> get
            3  -> ServedServerInfo <$> get
            4  -> ServedLock <$> get
            5  -> ServedUserInfo <$> get
            6  -> ServedUserInfoDeltas <$> get
            7  -> ServedSolution <$> get
            8  -> ServedHint <$> get
            9  -> ServedRandomNames <$> get
            10 -> return ServerCodenameFree
            11 -> return ServerFresh
            12 -> ServedRetired <$> get
            13 -> ServedPublicKey . read <$> get
            _  -> return ServerUndefinedResponse
instance Binary ServerInfo where
    put (ServerInfo sz str) = put sz >> put str
    get = liftM2 ServerInfo get get
