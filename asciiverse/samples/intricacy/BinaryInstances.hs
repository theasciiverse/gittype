-- This file is part of Intricacy
-- Copyright (C) 2013-2025 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

{-# OPTIONS_GHC -fno-warn-orphans #-}

module BinaryInstances where
import           Control.Monad
import           Data.Binary
import           Data.Int       (Int8)
import qualified Data.Vector    as Vector

import           Frame
import           GameStateTypes
import           Hex
import           Physics

newtype SmallInt = SmallInt {fromSmallInt :: Int}
newtype SmallNat = SmallNat {fromSmallNat :: Int}
instance Binary SmallNat where
    put (SmallNat n) = if n < 255 then putWord8 (fromIntegral n) else putWord8 255 >> put n
    get = do
        n' <- get :: Get Word8
        if n' == 255 then SmallNat <$> get else return . SmallNat $ fromIntegral n'

instance Binary SmallInt where
    put (SmallInt n) = if abs n < 127 then put (fromIntegral n :: Int8) else put (127::Int8) >> put n
    get = do
        n' <- get :: Get Int8
        if n' == 127 then SmallInt <$> get else return . SmallInt $ fromIntegral n'

putPackedNat,putPackedInt :: Int -> Put
putPackedNat n = put $ SmallNat n
putPackedInt n = put $ SmallInt n
getPackedNat,getPackedInt :: Get Int
getPackedNat = fromSmallNat <$> get
getPackedInt = fromSmallInt <$> get

newtype ShortList a = ShortList {fromShortList :: [a]}
instance Binary a => Binary (ShortList a) where
    put (ShortList as) = putPackedNat (length as) >> mapM_ put as
    get = do
        n <- getPackedNat
        ShortList <$> getMany n

 -- | 'getMany n' get 'n' elements in order, without blowing the stack.
 -- [ copied from source of package 'binary' by Lennart Kolmodin ]
getMany :: Binary a => Int -> Get [a]
getMany = go []
 where
    go xs 0 = return $! reverse xs
    go xs i = do x <- get
                 -- we must seq x to avoid stack overflows due to laziness in
                 -- (>>=)
                 x `seq` go (x:xs) (i-1)
{-# INLINE getMany #-}

instance Binary HexVec where
    put (HexVec x y _) = putPackedInt x >> putPackedInt y
    get = do
        x <- getPackedInt
        y <- getPackedInt
        return $ tupxy2hv (x,y)
instance Binary g => Binary (PHS g) where
    put (PHS v) = put v
    get = PHS <$> get
instance Binary GameState where
    put (GameState pps conns) = put (ShortList $ Vector.toList pps) >> put (ShortList conns)
    get = liftM2 GameState (Vector.fromList . fromShortList <$> get) (fromShortList <$> get)
instance Binary PlacedPiece where
    put (PlacedPiece ppos p) = put ppos >> put p
    get = liftM2 PlacedPiece get get
instance Binary Piece where
    put (Block patt)         = put (0::Word8) >> put (ShortList patt)
    put (Pivot arms)         = put (1::Word8) >> put (ShortList arms)
    put (Hook arm stiffness) = put (2::Word8) >> put arm >> put stiffness
    put (Wrench mom)         = put (3::Word8) >> put mom
    put Ball                 = put (4::Word8)
    get = do
        tag <- get :: Get Word8
        case tag of
            0 -> Block . fromShortList <$> get
            1 -> Pivot . fromShortList <$> get
            2 -> liftM2 Hook get get
            3 -> Wrench <$> get
            4 -> return Ball
            _ -> fail "bad tag to Piece"
instance Binary Connection where
    put (Connection (ri,rp) (ei,ep) l) = putPackedInt ri >> put rp >> putPackedInt ei >> put ep >> put l
    get = do
        ri <- getPackedInt
        rp <- get
        ei <- getPackedInt
        ep <- get
        Connection (ri,rp) (ei,ep) <$> get
instance Binary Link where
    put (Free p)     = put (0::Word8) >> put p
    put (Spring d l) = put (1::Word8) >> put d >> putPackedInt l
    get = do
        tag <- get :: Get Word8
        case tag of
            0 -> Free <$> get
            1 -> liftM2 Spring get getPackedInt
            _ -> fail "bad tag to Link"
instance Binary HookForce where
    put NullHF         = put (0::Word8)
    put (TorqueHF dir) = put (1::Word8) >> putPackedInt dir
    put (PushHF v)     = put (2::Word8) >> put v
    get = do
        tag <- get :: Get Word8
        case tag of
            0 -> return NullHF
            1 -> TorqueHF <$> getPackedInt
            2 -> PushHF <$> get
            _ -> fail "bad tag to HookForce"
instance Binary Frame where
    put (BasicFrame s) = putPackedInt s
    get = BasicFrame <$> getPackedInt

instance Binary PlayerMove where
    put NullPM           = put (0::Word8)
    put (HookPush v)     = put (1::Word8) >> put v
    put (HookTorque dir) = put (2::Word8) >> putPackedInt dir
    put (WrenchPush v)   = put (3::Word8) >> put v
    get = do
        tag <- get :: Get Word8
        case tag of
            0 -> return NullPM
            1 -> HookPush <$> get
            2 -> HookTorque <$> getPackedInt
            3 -> WrenchPush <$> get
            _ -> fail "bad tag to PlayerMove"
