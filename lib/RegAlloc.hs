{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module RegAlloc where

import Control.Arrow
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Data.Foldable
import Data.Functor
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace
import GHC.Natural
import Repr

-- TODO: walk the code and see how to unit test stuff.

-- registers that are not going to be allocated.
forbiddenRegisters :: [Register]
forbiddenRegisters = [Rsp, Rbp]

nop :: Applicative f => f ()
nop = pure ()

mkAllocs :: MonadReader (Registry Abi) m => Block -> m (BindingMap Register)
mkAllocs block = do
  (allocs, left_out) <- allocBlock block
  unless (null left_out) $ error "expected no left outs from allocator"
  pure allocs

-- TODO: memory allocations through stack frames
allocBlock :: MonadReader (Registry Abi) m => Block -> m (BindingMap Register, [Binding])
allocBlock block = do
  abiAllocs <- fmap (fromMaybe Map.empty) $ runMaybeT $ MaybeT (pure $ blockAbi block) >>= MaybeT . asks . registryIndex <&> flip mkAllocsFromAbi (blockOps block)
  let collision_map = mkCollisions block
  pure $ mkAllocsFromCollisions collision_map abiAllocs

type BindingMap = Map.Map Binding

-- currently only return registers
mkAllocsFromAbi :: Abi -> [Op] -> BindingMap Register
mkAllocsFromAbi abi = fst . traverse allocReturn . mapMaybe fromReturn
  where
    fromReturn (Return bindings) = Just bindings
    fromReturn _ = Nothing

    allocReturn :: MonadWriter (BindingMap Register) m => [Binding] -> m ()
    allocReturn bindings = tell $ Map.fromList $ zip bindings (abiReturns abi)

mkAllocsFromCollisions :: CollisionMap -> BindingMap Register -> (BindingMap Register, [Binding])
mkAllocsFromCollisions collision_map already_allocated = execRWS (traverse allocOrDrop $ Map.keys collision_map \\ Map.keys already_allocated) collision_map already_allocated

-- mkAllocsFromCollisions :: CollisionMap -> (BindingMap Register, [Binding])
-- mkAllocsFromCollisions collision_map = execRWS (traverse allocOrDrop $ Map.keys collision_map) collision_map Map.empty

getUsedRegister :: MonadReader (BindingMap Register) m => Binding -> m (Maybe Register)
getUsedRegister binding = asks (Map.!? binding)

-- currently it uses just the 'free register' approach, then just drops it.
allocOrDrop :: (MonadReader CollisionMap m, MonadWriter [Binding] m, MonadState (BindingMap Register) m) => Binding -> m ()
allocOrDrop binding = do
  result <- get >>= allocFreeRegister binding
  case result of
    Just reg -> modify' (Map.insert binding reg)
    Nothing -> tell [binding]

-- try allocating a register that is free, free as in no other thing is using it
allocFreeRegister :: MonadReader CollisionMap m => Binding -> BindingMap Register -> m (Maybe Register)
allocFreeRegister binding already_allocated = do
  collisions <- asks (Map.!? binding) <&> fromMaybe []
  let collidedRegisters = mapMaybe (`getUsedRegister` already_allocated) collisions
  let allowedRegisters = ([minBound .. maxBound] \\ forbiddenRegisters) \\ collidedRegisters
  pure $ listToMaybe allowedRegisters

-- lifetimes
data Lifetime = Lifetime {lifetimeEnd :: Natural, lifetimeStart :: Natural}

instance Show Lifetime where
  show (Lifetime end start) = unwords [show start, "-", show end]

type CollisionMap = BindingMap [Binding]

mkCollisions :: Block -> CollisionMap
mkCollisions = lifetimesToCollisions . mkLifetimes

lifetimesToCollisions :: LifetimeMap -> CollisionMap
lifetimesToCollisions (Map.toList -> lifetimes) =
  Map.fromList $ map (\(binding, lifetime) -> (binding, computeCollisions lifetime (lifetimesExcept binding))) lifetimes
  where
    lifetimesExcept binding = filter ((/= binding) . fst) lifetimes

    computeCollisions lt = map fst . filter (\(_, other) -> lt `collidesWith` other)

-- hey, do these two collide?
collidesWith :: Lifetime -> Lifetime -> Bool
Lifetime endA startA `collidesWith` Lifetime endB startB =
  -- A is defined when B is alive or B is defined while A is alive.
  (startA >= startB && startA < endB) || (startB >= startA && startB < endA)

type LifetimeMap = BindingMap Lifetime

type HalfAssignedLifetimes = BindingMap Natural

endLife :: MonadState HalfAssignedLifetimes m => Binding -> m (Natural -> Lifetime)
endLife name = do
  map <- get
  let start = map Map.!? name <&> Lifetime
  modify' $ Map.delete name -- pop the element from the map
  pure $ fromMaybe (join Lifetime) start

valueBindings :: Value -> [Binding]
valueBindings (Constants _) = []
valueBindings (Add a b) = a : maybeToList (sourceBinding b)

sourceBinding :: Source -> Maybe Binding
sourceBinding (SrcBinding b) = Just b
sourceBinding (SrcConstant _) = Nothing

data OpBindings = OpBindings {declaredBindings :: [Binding], usedBindings :: [Binding]}

opBindings :: Op -> OpBindings
opBindings (Assign bindings value) = OpBindings bindings (valueBindings value)
opBindings (Return bindings) = OpBindings [] bindings

processOpBindings :: (MonadState HalfAssignedLifetimes m, MonadWriter [(Binding, Lifetime)] m) => Natural -> OpBindings -> m ()
processOpBindings index (OpBindings declared used) = do
  -- set the lifetime start to all the declared bindings
  startLifetimes <- traverse endLife declared <&> map ($ index) <&> zip declared
  tell startLifetimes

  -- insert all the used bindnigs that weren't already used (map's <> operator keeps the already set keys)
  let newBindings = Map.fromList $ map (,index) used
  modify' (<> newBindings)

mkLifetimes :: Block -> LifetimeMap
mkLifetimes (blockOps >>> map opBindings >>> zip [0 ..] >>> reverse -> indexedOps) =
  let (leftBindings, foundEnds) = execRWS (traverse_ (uncurry processOpBindings) indexedOps) () Map.empty
      unused = map (second (join Lifetime)) $ Map.toList leftBindings
   in Map.fromList $ unused <> foundEnds
