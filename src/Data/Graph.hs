{-# LANGUAGE Rank2Types #-}
--------------------------------------------------------------------------------
-- |
-- Module : Data.Graph
-- Copyright : (C) 2016 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Data.Graph
    ( Graph
    , VertexId
    , vertices
    , edges
    , reduceByKey
    , join
    ) where

--------------------------------------------------------------------------------
import Control.Applicative
import Data.Foldable

--------------------------------------------------------------------------------
import qualified Data.Map as M
import Pipes

--------------------------------------------------------------------------------
type VertexId = Integer

--------------------------------------------------------------------------------
data Graph m v e =
    Graph
    { vertices :: forall r. Producer (VertexId, v) m r
    , edges    :: forall r. Producer (VertexId, VertexId, e) m r
    }

--------------------------------------------------------------------------------
reduceByKey :: (Ord k, MonadPlus m)
            => (v -> v -> v)
            -> Producer (k, v) m r
            -> Producer (k, v) m r
reduceByKey k s = s >-> start
  where
    start = running M.empty

    running m = do
        (vid, v) <- await <|> finish m

        let _F (Just v') = Just $ k v v'
            _F _         = Just v

        running $ M.alter _F vid m

    finish m = do
        traverse_ yield $ M.assocs m
        mzero

--------------------------------------------------------------------------------
join :: (Ord k, MonadPlus m)
     => Producer (k, a) m r
     -> Producer (k, b) m r
     -> Producer (k, (a, b)) m r
join start_l start_r = start
  where
    start = onLeft M.empty start_l

    onLeft m cur_l = do
      res <- lift $ next cur_l
      case res of
        Left _                -> onRight m start_r
        Right ((vid, a), nxt) -> onLeft (M.insert vid (Left a) m) nxt

    onRight m cur_r = do
      res <- lift $ next cur_r
      case res of
        Left _ -> finish m
        Right ((vid, b), nxt) ->
          onRight (M.adjust (\(Left a) -> Right (a, b)) vid m) nxt

    finish m = do
      for_ (M.assocs m) $ \(vid, res) ->
        case res of
          Right tup -> yield (vid, tup)
          _         -> return ()
      mzero

--------------------------------------------------------------------------------
