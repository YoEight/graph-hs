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
    ) where

--------------------------------------------------------------------------------
import Control.Applicative
import Data.Foldable

--------------------------------------------------------------------------------
import Data.Machine
import qualified Data.Map as M

--------------------------------------------------------------------------------
type VertexId = Integer

--------------------------------------------------------------------------------
data Graph m v e =
    Graph
    { vertices :: SourceT m (VertexId, v)
    , edges    :: SourceT m (VertexId, VertexId, e)
    }

--------------------------------------------------------------------------------
reduceByKey :: Monad m => (v -> v -> v) -> Graph m v e -> Graph m v e
reduceByKey k g = Graph (vertices g ~> start) (edges g)
  where
    start = construct $ running M.empty

    running m = do
        (vid, v) <- await <|> finish m

        let _F (Just v') = Just $ k v v'
            _F _         = Just v

        running $ M.alter _F vid m

    finish m = do
        traverse_ yield $ M.assocs m
        stop

--------------------------------------------------------------------------------
join :: Monad m => Graph m v e -> Graph m u e -> Graph m (v, u) e
join g j = Graph (teeT start (vertices g) (vertices j)) undefined
  where
    start = construct $ onLeft M.empty

    onLeft m = do
        (vid, v) <- awaits L <|> onRight m
        onLeft $ M.insert vid (Left v) m

    onRight m = do
        (vid, u) <- awaits R <|> finish m
        onRight $ M.adjust (\(Left v) -> Right (v, u)) vid m

    finish m = do
        for_ (M.assocs m) $ \(vid, res) ->
          case res of
            Right tup -> yield (vid, tup)
            _         -> return ()

        stop
