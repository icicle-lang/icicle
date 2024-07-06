-- | When converting from Core to Avalanche, we need to implement
--   some Core prims with non-trivial Avalanche.

{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module Icicle.Avalanche.Statement.Flatten.Algorithms where

import              Icicle.Avalanche.Statement.Flatten.Base

import              Icicle.Avalanche.Statement.Statement
import qualified    Icicle.Avalanche.Prim.Flat      as Flat
import qualified    Icicle.Avalanche.Prim.Compounds as Flat

import qualified    Icicle.Core.Exp.Prim           as Core
import qualified    Icicle.Common.Exp.Prim.Minimal as Min

import              Icicle.Common.Base
import              Icicle.Common.Type
import              Icicle.Common.Exp
import              Icicle.Common.Fresh

import              P

import              Data.Hashable                  (Hashable)
import              Data.String                    (IsString)


type FlatX a n = a -> Exp a n Core.Prim -> (Exp a n Flat.Prim -> FlatM a n) -> FlatM a n
type StmtX a n = Exp a n Flat.Prim -> FlatM a n

--------------------------------------------------------------------------------

initInt :: Name n -> Exp a n p -> Statement a n p -> Statement a n  p
initInt acc init
  = InitAccumulator (Accumulator acc IntT init)

readInt :: Name n -> Name n -> Statement a n p -> Statement a n  p
readInt local acc
  = Read local acc IntT

initArr :: ValType -> Name n -> Exp a n p -> Statement a n p -> Statement a n p
initArr t acc init
  = InitAccumulator (Accumulator acc (ArrayT t) init)

readArr :: ValType -> Name n -> Name n -> Statement a n p -> Statement a n p
readArr t local acc
  = Read local acc (ArrayT t)

initBool :: Name n -> Exp a n p -> Statement a n p -> Statement a n  p
initBool acc init
  = InitAccumulator (Accumulator acc BoolT init)

readBool :: Name n -> Name n -> Statement a n p -> Statement a n  p
readBool local acc
  = Read local acc BoolT

-- Create a let binding with a fresh name
slet_ :: (Hashable n, Monad m)
      => a -> Maybe n -> Exp a n p -> (Exp a n p' -> FreshT n m (Statement a n p))
      -> FreshT n m (Statement a n p)
slet_ a_fresh prefix x ss
 = do n  <- maybe fresh freshPrefix prefix
      Let n x <$> ss (XVar a_fresh n)

slet :: (Hashable n, Monad m)
     => a -> Exp a n p -> (Exp a n p' -> FreshT n m (Statement a n p))
     -> FreshT n m (Statement a n p)
slet a = slet_ a Nothing

sletPrefix :: (Hashable n, Monad m)
           => a -> n -> Exp a n p -> (Exp a n p' -> FreshT n m (Statement a n p))
           -> FreshT n m (Statement a n p)
sletPrefix a n = slet_ a (Just n)

-- For loop with fresh name for iterator
forI :: (Hashable n, Monad m, IsString n)
     => a -> Exp a n p -> (Exp a n p -> FreshT n m (Statement a n p))
     -> FreshT n m (Statement a n p)
forI a_fresh to ss
 = forI_ a_fresh ForeachStepUp (XValue a_fresh IntT (VInt 0)) to ss

forI_ :: (Hashable n, Monad m, IsString n)
     => a -> ForeachType -> Exp a n p -> Exp a n p -> (Exp a n p -> FreshT n m (Statement a n p))
     -> FreshT n m (Statement a n p)
forI_ a_fresh t from to ss
 = do n  <- freshPrefix "for_counter"
      ForeachInts t n from to <$> ss (XVar a_fresh n)

-- Update an accumulator
updateAcc :: (Hashable n, Monad m, IsString n)
          => a -> Name n -> ValType -> (Exp a n p -> Exp a n p)
          -> FreshT n m (Statement a n p)
updateAcc a_fresh acc t x
 = do n'x <- freshPrefix "update_acc"
      return $ Read n'x acc t $ Write acc $ x $ XVar a_fresh n'x

-- | Mutable put
putArrayAcc' :: (Hashable n, Monad m, IsString n)
             => a -> ValType -> Name n -> Flat.X a n -> Flat.X a n
             -> FreshT n m (Flat.S a n)
putArrayAcc' a_fresh t n'acc idx push
 = do let t'          = ArrayT t
          put arr i x = arrUpd' t arr i x

      updateAcc a_fresh n'acc t' (\arr -> put arr idx push)
 where
  Flat.FlatOps  {..} = Flat.flatOps a_fresh


-- | Copy an array - before mutating
copyArrayAcc :: (Hashable n, Monad m, IsString n)
             => a -> ValType -> Name n
             -> FreshT n m (Flat.S a n)
copyArrayAcc a_fresh t n'acc
 = do n'x <- freshPrefix "copy_array"
      let x = xVar n'x
      let num = xValue IntT . VInt
      let is_empty = relNe IntT (arrLen t x) (num 0)
      let cpy = Write n'acc (arrCpy t x)

      return $ Read n'x n'acc (ArrayT t)
             $ If is_empty cpy mempty

 where
  Flat.FlatOps  {..} = Flat.flatOps a_fresh
  Flat.FlatCons {..} = Flat.flatCons a_fresh


--------------------------------------------------------------------------------

-- | Avalanche program that uses binary search to find and return the index
--   of a key in a sorted array.
--
-- binary_search key array
--   found = false
--   m = -1
--
--   low  = 0
--   high = size array - 1
--   end = false
--
--   while end == false
--     m = floor ((high + low) / 2)
--     x = array[m]
--     if x == key
--       end = true
--       found = true
--     else
--       if x < key
--         low = m + 1
--       else
--         high = m - 1
--     if low > high
--       end = true
--
avalancheBinarySearch
  :: (Hashable n, IsString n)
  => a
  -> ValType            -- ^ type of array element
  -> Exp a n Flat.Prim  -- ^ key
  -> Exp a n Flat.Prim  -- ^ array
  -> Name n             -- ^ put true in this accumulator if the key is found.
  -> Name n             -- ^ put result in this accumulator.
                        --   if found, this is the index of the key.
                        --   if not found, it's where to insert, e.g. insert at ix means
                        --   moving all elements A[ix].. to A[ix+1].. then write A[ix] = key
  -> FlatM a n
avalancheBinarySearch a_fresh t key array found result
 = do n_loc_key   <- freshPrefix "bs_loc_key"
      n_loc_array <- freshPrefix "bs_loc_array"
      n_loc_found <- freshPrefix "bs_loc_found"
      n_loc_mid   <- freshPrefix "bs_loc_mid"
      n_acc_mid   <- freshPrefix "bs_acc_mid"
      n_loc_ins   <- freshPrefix "bs_loc_ins"
      n_acc_ins   <- freshPrefix "bs_acc_ins"
      n_acc_found <- freshPrefix "bs_acc_found"

      let v_mid   = xVar n_loc_mid
          v_ins   = xVar n_loc_ins
          v_found = xVar n_loc_found

      loop <- bsLoop n_loc_key n_loc_array n_acc_mid n_acc_ins n_acc_found

      let res
            = readBool n_loc_found n_acc_found
            $ readInt  n_loc_mid   n_acc_mid
            $ readInt  n_loc_ins   n_acc_ins
            $ If (relEq BoolT v_found xTrue)
                 (Write found xTrue  <> Write result v_mid )
                 (Write found xFalse <> Write result v_ins )

      return
        $ Let      n_loc_key   key
        $ Let      n_loc_array array
        $ initBool n_acc_found xFalse
        $ initInt  n_acc_mid   (xValue IntT (VInt (-1)))
        $ initInt  n_acc_ins   (xValue IntT (VInt (-1)))
        $ loop <> res

  where
    Flat.FlatOps  {..} = Flat.flatOps a_fresh
    Flat.FlatCons {..} = Flat.flatCons a_fresh

    bsLoop n_loc_key n_loc_array n_acc_mid n_acc_ins n_acc_found
      = do n_loc_mid   <- freshPrefix "bs_loc_mid"
           n_loc_x     <- freshPrefix "bs_loc_x"
           n_loc_low   <- freshPrefix "bs_loc_low"
           n_loc_high  <- freshPrefix "bs_loc_high"
           n_acc_low   <- freshPrefix "bs_acc_low"
           n_acc_high  <- freshPrefix "bs_acc_high"
           n_acc_end   <- freshPrefix "bs_acc_end"

           let v_low   = xVar n_loc_low
               v_high  = xVar n_loc_high
               v_array = xVar n_loc_array
               v_key   = xVar n_loc_key
               v_mid   = xVar n_loc_mid
               v_x     = xVar n_loc_x

           return
             $ initInt  n_acc_low   (xValue IntT (VInt 0))
             $ initInt  n_acc_high  (xHigh  v_array)
             $ initBool n_acc_end   xFalse
             $ While WhileEq n_acc_end BoolT xFalse
             $ readInt n_loc_low  n_acc_low
             $ readInt n_loc_high n_acc_high
             $ If ( relGt IntT v_low v_high )
                  ( Write n_acc_end xTrue <> Write n_acc_ins v_low )
             $ Block
               [ Write   n_acc_mid (xMid v_low v_high)
               , readInt n_loc_mid  n_acc_mid
               $ Let     n_loc_x   (xIndex v_array v_mid)
               $ If ( relEq t v_x v_key )
                    ( Write n_acc_end xTrue <> Write n_acc_found xTrue )
               $ If ( relLt t v_x v_key )
                    ( Write n_acc_low  (xPlusOne  v_mid) )
                    ( Write n_acc_high (xMinusOne v_mid) )
               ]

    xPlusOne m
      = xArith Min.PrimArithPlus `xApp` m `xApp` xValue IntT (VInt 1)
    xMinusOne m
      = xArith Min.PrimArithMinus `xApp` m `xApp` xValue IntT (VInt 1)

    xHigh arr
     = xMinusOne (xPrim (Flat.PrimProject (Flat.PrimProjectArrayLength t)) `xApp` arr)

    xIndex arr i
     = xPrim (Flat.PrimUnsafe (Flat.PrimUnsafeArrayIndex t)) `xApp` arr `xApp` i

    xMid low high
      = xMath Min.PrimBuiltinFloor
         `xApp` (xMath Min.PrimBuiltinDiv
                  `xApp` (xMath Min.PrimBuiltinToDoubleFromInt
                           `xApp` (xArith Min.PrimArithPlus `xApp` low `xApp` high))
                  `xApp` xValue DoubleT (VDouble 2.0))

    xTrue  = xValue BoolT (VBool True)
    xFalse = xValue BoolT (VBool False)

--------------------------------------------------------------------------------

avalancheHeapSortArray
  :: forall a n . (Hashable n, IsString n)
  => a
  -> StmtX a n
  -> FlatX a n
  -> ValType
  -> Exp a n Core.Prim
  -> FlatM a n
avalancheHeapSortArray a_fresh stm flatX t array
  = flatX a_fresh array $ \array' -> do
      n_acc_arr <- freshPrefix "sort_acc_array"
      n_loc_arr <- freshPrefix "sort_array_result"

      copyArr      <- copyArrayAcc a_fresh t n_acc_arr
      sort         <- avalancheHeapSort_ a_fresh t n_acc_arr Nothing
      array_sorted <- stm (xVar n_loc_arr)

      return
        $ initArr t n_acc_arr array'
        $ Block
        [ copyArr
        , sort
        , readArr t n_loc_arr n_acc_arr array_sorted ]
  where
    Flat.FlatCons{..} = Flat.flatCons a_fresh


avalancheHeapSortMap
  :: forall a n . (Hashable n, IsString n)
  => a
  -> ValType
  -> ValType
  -> Name n
  -> Name n
  -> FlatM a n
avalancheHeapSortMap a_fresh tk tv n_acc_keys n_acc_vals =
  avalancheHeapSort_ a_fresh tk n_acc_keys (Just (tv, n_acc_vals))

avalancheHeapSort_
  :: forall a n . (Hashable n, IsString n)
  => a
  -> ValType
  -> Name n
  -> Maybe (ValType, Name n)
  -> FlatM a n

avalancheHeapSort_ a_fresh t array extras = do
  n_acc_heapSize <- freshPrefix "sort_acc_heap_size"

  heapSort array n_acc_heapSize (fmap snd extras) (fmap fst extras)

  where
    Flat.FlatCons{..} = Flat.flatCons a_fresh

    xIndex arr i
     = xPrim (Flat.PrimUnsafe (Flat.PrimUnsafeArrayIndex t))
        `xApp` arr
        `xApp` i
    xLen arr
     = xPrim (Flat.PrimProject (Flat.PrimProjectArrayLength t))
        `xApp` arr
    xNil      = xValue IntT (VInt 0)
    xOne      = xValue IntT (VInt 1)
    xTwo      = xValue IntT (VInt 2)
    xMinusOne = xValue IntT (VInt (-1))
    xTrue     = xValue BoolT (VBool True)
    xFalse    = xValue BoolT (VBool False)
    xMinus x y
     = xArith Min.PrimArithMinus `xApp` x `xApp` y
    xHalf i
     = xMath Min.PrimBuiltinFloor
        `xApp` (xMath Min.PrimBuiltinDiv
                 `xApp` (xMath Min.PrimBuiltinToDoubleFromInt
                          `xApp` i)
                 `xApp` xValue DoubleT (VDouble 2.0))
    xTimesTwo i
     = xArith Min.PrimArithMul
        `xApp` i
        `xApp` xTwo
    -- left i = 2 * i + 1
    xLeft i
     = xArith Min.PrimArithPlus
        `xApp` xTimesTwo i
        `xApp` xOne
    -- right i = 2 * i + 2
    xRight i
     = xArith Min.PrimArithPlus
        `xApp` xTimesTwo i
        `xApp` xTwo
    -- i < heap_size
    xHeapSize i heapSize
     = xRel IntT Min.PrimRelationLt
        `xApp` i
        `xApp` heapSize
    -- left < heap_size && arr[left] > arr[i]
    xHeapLeft arr heapSize l i
     = [ xHeapSize l heapSize
       , xRel t Min.PrimRelationGt
          `xApp` xIndex arr l
          `xApp` xIndex arr i
       ]
    -- right < heap_size && arr[right] > arr[largest]
    xHeapRight arr heapSize r largest
     = [ xHeapSize r heapSize
       , xRel t Min.PrimRelationGt
           `xApp` xIndex arr r
           `xApp` xIndex arr largest
       ]
    -- largest  /= i
    xHeap i largest
     = xRel IntT Min.PrimRelationNe
        `xApp` largest
        `xApp` i
    -- swap (A[i], A[largest])
    xSwap ty arr i largest 
     = xPrim (Flat.PrimArray (Flat.PrimArraySwap ty))
        `xApp` arr
        `xApp` i
        `xApp` largest
    xSwap' Nothing   arr _ _       = arr
    xSwap' (Just ty) arr i largest = xSwap ty arr i largest

    -- max_heap(array, heap_size, i):
    --   left  = 2 * i + 1
    --   right = 2 * i + 2
    --   if left < heap_size && array[left] > array[i]
    --     largest  = left
    --   else
    --     largest  = i
    --   if right < heap_size && array[right] > array[largest ]
    --     largest  = right
    --   if largest  /= i
    --     swap (array[i], array[largest ])
    --     max_heap (array, heap_size, largest )
    maxHeap n_acc_arr n_acc_heapSize n_acc_index n_acc_av tv
     = do n_acc_left      <- freshPrefix "max_heap_acc_left"
          n_acc_right     <- freshPrefix "max_heap_acc_right"
          n_acc_largest   <- freshPrefix "max_heap_acc_largest"
          n_acc_end       <- freshPrefix "max_heap_acc_end"
          n_loc_left      <- freshPrefix "max_heap_left"
          n_loc_right     <- freshPrefix "max_heap_right"
          n_loc_largest   <- freshPrefix "max_heap_largest"
          n_loc_arr       <- freshPrefix "max_heap_array"
          n_loc_av        <- freshPrefix "max_heap_av"
          n_loc_heapSize  <- freshPrefix "max_heap_size"
          n_loc_index     <- freshPrefix "max_heap_index"

          let v_left      = xVar n_loc_left
              v_right     = xVar n_loc_right
              v_largest   = xVar n_loc_largest
              v_arr       = xVar n_loc_arr
              v_av        = xVar n_loc_av
              v_heapSize  = xVar n_loc_heapSize
              v_index     = xVar n_loc_index

          return
            $ initInt  n_acc_left     xMinusOne
            $ initInt  n_acc_right    xMinusOne
            $ initInt  n_acc_largest  xMinusOne
            $ initBool n_acc_end      xFalse
            $ While WhileEq n_acc_end BoolT xFalse
            $ readInt     n_loc_index   n_acc_index
            $ readArr  t  n_loc_arr     n_acc_arr
            $ readArr' tv n_loc_av      n_acc_av
            $ readInt     n_loc_heapSize n_acc_heapSize
            $ Block
              -- left  = 2 * i + 1
              [ Write n_acc_left  (xLeft  v_index)
              -- right = 2 * i + 2
              , Write n_acc_right (xRight v_index)
              , readInt n_loc_left     n_acc_left
              $ readInt n_loc_right    n_acc_right
              $ Block
                -- if left < heap_size && array[left] > array[i]
                [ nestedIfs
                    (xHeapLeft  v_arr v_heapSize v_left  v_index)
                    -- then largest  = left
                    (Write n_acc_largest  v_left)
                    -- else largest  = i
                    (Write n_acc_largest  v_index)
                -- if right < heap_size && array[right] > array[largest ]
                , readInt n_loc_largest  n_acc_largest
                $ nestedIfs
                    (xHeapRight v_arr v_heapSize v_right v_largest )
                    -- then largest  = right
                    (Write n_acc_largest  v_right)
                    mempty
                -- if largest  /= i
                , readInt n_loc_largest   n_acc_largest 
                $ If (xHeap v_largest  v_index)
                     -- then swap (array[i], array[largest ])
                     (Block
                       [ Write  n_acc_arr   (xSwap  t  v_arr v_index v_largest )
                       , write' n_acc_av    (xSwap' tv v_av  v_index v_largest )
                       , Write  n_acc_index v_largest
                       ])
                     (Write n_acc_end xTrue)
                ]
              ]

    -- build_max_heap (array):
    --   heap_size = length (array)
    --   for (i = floor (length (array) / 2); i >= 0; i--)
    --     array = max_heap (array, heap_size, i)
    buildMaxHeap n_acc_arr n_acc_heapSize n_acc_av tv
     = do n_acc_index    <- freshPrefix "build_max_heap_acc_index"
          n_loc_index    <- freshPrefix "build_max_heap_index"
          n_loc_arr      <- freshPrefix "build_max_heap_array"

          let v_index     = xVar n_loc_index
              v_arr       = xVar n_loc_arr

          body           <- maxHeap n_acc_arr n_acc_heapSize n_acc_index n_acc_av tv

          return
            $ readArr t n_loc_arr n_acc_arr
            $ Block
              -- heap_size = length (array)
              [ Write   n_acc_heapSize (xLen v_arr)
              , initInt n_acc_index    xMinusOne
              -- for (i = floor (length (array) / 2); i > -1; i--)
              $ ForeachInts ForeachStepDown n_loc_index (xHalf (xLen v_arr)) xMinusOne
              $ Block
                [ Write n_acc_index v_index
                -- array = max_heap (array, heap_size, i)
                , body
                ]
              ]

    -- heap_sort (array):
    --   array = build_max_heap (array)
    --   for (i = length array - 1; i >= 2; i--)
    --     array = swap (array[0], array[i])
    --     heap_size = heap_size - 1
    --     array = max_heap (array, heap_size, 0)
    heapSort n_acc_arr n_acc_heapSize n_acc_av tv
      = do n_acc_index    <- freshPrefix "heap_sort_acc_index"
           n_loc_index    <- freshPrefix "heap_sort_index"
           n_loc_heapSize <- freshPrefix "heap_sort_heapSize"
           n_loc_arr      <- freshPrefix "heap_sort_arr"
           n_loc_av       <- freshPrefix "heap_sort_av"

           let v_index     = xVar n_loc_index
               v_heapSize  = xVar n_loc_heapSize
               v_arr       = xVar n_loc_arr
               v_av        = xVar n_loc_av

           body1 <- buildMaxHeap n_acc_arr n_acc_heapSize             n_acc_av tv
           body3 <- maxHeap      n_acc_arr n_acc_heapSize n_acc_index n_acc_av tv

           let body2
                = readArr  t  n_loc_arr n_acc_arr
                $ readArr' tv n_loc_av  n_acc_av
                $ Block
                  -- for (i = length array - 1; i > 0; i --)
                  [ ForeachInts ForeachStepDown n_loc_index (xMinus (xLen v_arr) xOne) xNil
                    $ Block
                      [ -- array = swap (array[0], array[i])
                        Write   n_acc_arr (xSwap  t  v_arr xNil v_index)
                      , write'  n_acc_av  (xSwap' tv v_av  xNil v_index)
                      , readInt n_loc_heapSize n_acc_heapSize
                      $ Block
                        -- heap_size = heap_size - 1
                        [ Write n_acc_heapSize (xMinus v_heapSize xOne)
                        , Write n_acc_index    xNil
                        -- array = max_heap (array, heap_size, 0)
                        , body3
                        ]
                      ]
                  ]

           return
             $ initInt   n_acc_index xMinusOne
             $ readArr t n_loc_arr n_acc_arr
             $ initInt   n_acc_heapSize (xLen v_arr)
             $ Block [ body1, body2 ]

    readArr' (Just ty) n (Just acc) stmt = readArr ty n acc stmt
    readArr'  _        _  _         stmt = stmt

    write' Nothing  _ = mempty
    write' (Just n) x = Write n x


--------------------------------------------------------------------------------

avalancheMapInsertUpdate
  :: (Hashable n, IsString n)
  => a
  -> StmtX a n
  -> FlatX a n
  -> ValType
  -> ValType
  -> Exp a n Core.Prim
  -> Exp a n Core.Prim
  -> Exp a n Core.Prim
  -> Exp a n Core.Prim
  -> FlatM a n
avalancheMapInsertUpdate a_fresh stm flatX tk tv upd ins key map
  = flatX' map $ \map' -> flatX' key $ \key' -> do
      n_acc_keys   <- freshPrefix "map_insert_acc_keys"
      n_acc_vals   <- freshPrefix "map_insert_acc_vals"
      n_acc_idx    <- freshPrefix "map_insert_acc_bs_index"
      n_acc_found  <- freshPrefix "map_insert_acc_bs_found"
      n_loc_keys   <- freshPrefix "map_insert_loc_keys"
      n_loc_vals   <- freshPrefix "map_insert_loc_vals"
      n_cpy_keys   <- freshPrefix "map_insert_cpy_keys"
      n_cpy_vals   <- freshPrefix "map_insert_cpy_vals"
      n_loc_idx    <- freshPrefix "map_insert_loc_bs_index"
      n_loc_found  <- freshPrefix "map_insert_loc_bs_found"
      n_res        <- freshPrefix "map_insert_result"
      n_size       <- freshPrefix "map_insert_size"

      let v_keys  = xVar  n_loc_keys
          v_vals  = xVar  n_loc_vals
          v_idx   = xVar  n_loc_idx
          v_found = xVar  n_loc_found
          v_res   = xVar  n_res
          v_size  = xVar  n_size

      let acc_idx   = Accumulator n_acc_idx    IntT        $ xValue IntT  (VInt (-1))
          acc_found = Accumulator n_acc_found  BoolT       $ xFalse
          acc_keys  = Accumulator n_acc_keys  (ArrayT  tk) $ mapKeys tk tv map'
          acc_vals  = Accumulator n_acc_vals  (ArrayT  tv) $ mapVals tk tv map'

      let readk  = readArr tk n_loc_keys n_acc_keys
          readv  = readArr tv n_loc_vals n_acc_vals
          readk' = readArr tk n_cpy_keys n_acc_keys
          readv' = readArr tv n_cpy_vals n_acc_vals
          readix = readBool n_loc_found n_acc_found
                 . readInt  n_loc_idx   n_acc_idx

      let get_k i   = arrIx  tk (xVar n_cpy_keys) i
          get_v i   = arrIx  tv (xVar n_cpy_vals) i

      -- Look up the key.
      sLookup <- avalancheBinarySearch a_fresh tk key' v_keys n_acc_found n_acc_idx

      -- If it exists, update.
      sUpd <- sletPrefix a_fresh "map_insert_loc_old" (get_v v_idx)
            $ \v  -> flatX' (upd `xApp'` v)
            $ \v' -> putArrayAcc' a_fresh tv n_acc_vals v_idx v'

      -- Otherwise, insert at v_idx.
      -- Start by creating a copy of the arrays so we can mutate
      copyk   <- copyArrayAcc a_fresh tk n_acc_keys
      copyv   <- copyArrayAcc a_fresh tv n_acc_vals

      -- Now move elements to the right of v_idx.
      -- for i from size downto ix+1: A[i] = A[i-1]
      move    <- forI_ a_fresh ForeachStepDown v_size v_idx
              $ \i -> do k <- putArrayAcc' a_fresh tk n_acc_keys i $ get_k (xMinusOne i)
                         v <- putArrayAcc' a_fresh tv n_acc_vals i $ get_v (xMinusOne i)
                         return (k <> v)
      putk    <- putArrayAcc' a_fresh tk n_acc_keys v_idx key'
      putv    <- flatX' ins
               $ putArrayAcc' a_fresh tv n_acc_vals v_idx
      let sIns = copyk <> copyv <> readk' (readv' move) <> putk <> putv

      let keyInMap        = relEq BoolT v_found xTrue
      let sInsertOrUpdate = If keyInMap (copyv <> readv' sUpd) sIns

      stm'    <- stm v_res
      let res  = Let n_res (mapPack tk tv v_keys v_vals) stm'
          ss   = InitAccumulator acc_keys
               $ InitAccumulator acc_vals
               $ InitAccumulator acc_found
               $ InitAccumulator acc_idx
               $ readk . readv . readix
               $ Let n_size (arrLen tk v_keys)
               $ sLookup <> (readix (sInsertOrUpdate <> readk (readv res)))

      return ss

  where
    xMinusOne m = xArith Min.PrimArithMinus `xApp` m `xApp` xValue IntT (VInt 1)
    xTrue       = xValue BoolT (VBool True)
    xFalse      = xValue BoolT (VBool False)

    flatX'             = flatX a_fresh
    xApp'              = XApp   a_fresh
    Flat.FlatOps  {..} = Flat.flatOps  a_fresh
    Flat.FlatCons {..} = Flat.flatCons a_fresh

--------------------------------------------------------------------------------

avalancheMapDelete
  :: (Hashable n, IsString n)
  => a
  -> StmtX a n
  -> FlatX a n
  -> ValType
  -> ValType
  -> Exp a n Core.Prim
  -> Exp a n Core.Prim
  -> FlatM a n
avalancheMapDelete a_fresh stm flatX tk tv key map
  = flatX' key $ \key' -> flatX' map $ \map' -> do
      n_acc_keys   <- freshPrefix "map_insert_acc_keys"
      n_acc_vals   <- freshPrefix "map_insert_acc_vals"
      n_acc_idx    <- freshPrefix "map_insert_acc_bs_index"
      n_acc_found  <- freshPrefix "map_insert_acc_bs_found"
      n_loc_keys   <- freshPrefix "map_insert_loc_keys"
      n_loc_vals   <- freshPrefix "map_insert_loc_vals"
      n_cpy_keys   <- freshPrefix "map_insert_cpy_keys"
      n_cpy_vals   <- freshPrefix "map_insert_cpy_vals"
      n_loc_idx    <- freshPrefix "map_insert_loc_bs_index"
      n_loc_found  <- freshPrefix "map_insert_loc_bs_found"
      n_res        <- freshPrefix "map_insert_result"

      let v_keys  = xVar  n_loc_keys
          v_vals  = xVar  n_loc_vals
          v_idx   = xVar  n_loc_idx
          v_found = xVar  n_loc_found
          v_res   = xVar  n_res

      let acc_idx   = Accumulator n_acc_idx    IntT        $ xValue IntT  (VInt (-1))
          acc_found = Accumulator n_acc_found  BoolT       $ xFalse
          acc_keys  = Accumulator n_acc_keys  (ArrayT  tk) $ mapKeys tk tv map'
          acc_vals  = Accumulator n_acc_vals  (ArrayT  tv) $ mapVals tk tv map'

      let readk  = readArr tk n_loc_keys n_acc_keys
          readv  = readArr tv n_loc_vals n_acc_vals
          readk' = readArr tk n_cpy_keys n_acc_keys
          readv' = readArr tv n_cpy_vals n_acc_vals
          readix = readBool n_loc_found n_acc_found
                 . readInt  n_loc_idx   n_acc_idx

      -- Look up the key.
      sLookup <- avalancheBinarySearch a_fresh tk key' v_keys n_acc_found n_acc_idx

      -- If it's found, we'll need a copy.
      copyk   <- copyArrayAcc a_fresh tk n_acc_keys
      copyv   <- copyArrayAcc a_fresh tv n_acc_vals

      -- Perform the delete (this will also update the count)
      let del  = Write n_acc_keys (arrDel tk (xVar n_cpy_keys) v_idx)
              <> Write n_acc_vals (arrDel tv (xVar n_cpy_vals) v_idx)

      let sDel = copyk <> copyv <> readk' (readv' del)

      let keyInMap        = relEq BoolT v_found xTrue
      let sInsertOrUpdate = If keyInMap sDel mempty

      stm'    <- stm v_res
      let res  = Let n_res (mapPack tk tv v_keys v_vals) stm'
          ss   = InitAccumulator acc_keys
               $ InitAccumulator acc_vals
               $ InitAccumulator acc_found
               $ InitAccumulator acc_idx
               $ readk . readv . readix
               $ sLookup <> (readix (sInsertOrUpdate <> readk (readv res)))

      return ss

  where
    xTrue       = xValue BoolT (VBool True)
    xFalse      = xValue BoolT (VBool False)

    flatX'             = flatX a_fresh
    Flat.FlatOps  {..} = Flat.flatOps  a_fresh
    Flat.FlatCons {..} = Flat.flatCons a_fresh


--------------------------------------------------------------------------------

avalancheMapLookup
  :: (Hashable n, IsString n)
  => a
  -> StmtX a n
  -> FlatX a n
  -> ValType
  -> ValType
  -> Exp a n Core.Prim
  -> Exp a n Core.Prim
  -> FlatM a n
avalancheMapLookup a_fresh stm flatX tk tv key map
  = flatX' map $ \map' -> flatX' key $ \key' -> do
      n_keys  <- freshPrefix "map_lookup_keys"
      n_vals  <- freshPrefix "map_lookup_vals"
      n_res   <- freshPrefix "map_lookup_res"
      n_idx   <- freshPrefix "map_lookup_bs_index"
      n_found <- freshPrefix "map_lookup_bs_found"

      let v_keys  = xVar' n_keys
          v_vals  = xVar  n_vals
          v_idx   = xVar  n_idx
          v_found = xVar  n_found
          v_res   = xVar  n_res

      let acc_res   = Accumulator n_res   (OptionT tv) $ xNothing tv
          acc_found = Accumulator n_found  BoolT       $ xFalse
          acc_idx   = Accumulator n_idx    IntT        $ xValue IntT (VInt (-1))

      search      <- avalancheBinarySearch a_fresh tk key' v_keys n_found n_idx

      let keyInMap  = relEq BoolT v_found xTrue
          notFound  = Write n_res $ xNothing tv
          found     = Write n_res $ xJust    tv $ arrIx tv v_vals v_idx
          ss        = If keyInMap found notFound

      res <- stm $ v_res

      let readix  = Read n_idx   n_idx    IntT
                  . Read n_found n_found BoolT

      return
        $ Let   n_keys (mapKeys tk tv map')
        $ Let   n_vals (mapVals tk tv map')
        $ InitAccumulator acc_res
        $ InitAccumulator acc_found
        $ InitAccumulator acc_idx
        $ Block [ search
                , readix ss
                , Read n_res n_res (OptionT tv) res
                ]

  where
    xJust t x
     = xMin (Min.PrimConst (Min.PrimConstSome t)) `xApp` x
    xNothing t
     = xValue (OptionT t) VNone
    xTrue  = xValue BoolT (VBool True)
    xFalse = xValue BoolT (VBool False)

    flatX'             = flatX a_fresh
    xVar'              = XVar a_fresh
    Flat.FlatCons {..} = Flat.flatCons a_fresh
    Flat.FlatOps  {..} = Flat.flatOps  a_fresh
