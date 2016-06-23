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
    , fromList
    , collectVertices
    , collectEdges
    ) where

--------------------------------------------------------------------------------
import Control.Applicative
import Data.Foldable

--------------------------------------------------------------------------------
import Data.Conduit
import Data.Conduit.List
import qualified Data.Map as M

--------------------------------------------------------------------------------
type VertexId = Integer

--------------------------------------------------------------------------------
data Graph m v e =
    Graph
    { vertices :: Source m (VertexId, v)
    , edges    :: Source m (VertexId, VertexId, e)
    }

--------------------------------------------------------------------------------
collect :: Monad m => Source m a -> m [a]
collect = sourceToList

--------------------------------------------------------------------------------
collectVertices :: Monad m => Graph m v e -> m [(VertexId, v)]
collectVertices = collect . vertices

--------------------------------------------------------------------------------
collectEdges :: Monad m => Graph m v e -> m [(VertexId, VertexId, e)]
collectEdges = collect . edges

--------------------------------------------------------------------------------
fromList :: Monad m
         => [(VertexId, v)]
         -> [(VertexId, VertexId, e)]
         -> Graph m v e
fromList vs es = Graph (sourceList vs) (sourceList es)

--------------------------------------------------------------------------------
reduceByKey :: (Ord k, Monad m)
            => (v -> v -> v)
            -> Source m (k, v)
            -> Source m (k, v)
reduceByKey k s = s =$= start
  where
    start = running M.empty

    running m = do
        res <- await

        case res of
          Nothing       -> sourceList $ M.assocs m
          Just (vid, v) ->
            let _F (Just v') = Just $ k v v'
                _F _         = Just v in
            running $ M.alter _F vid m

--------------------------------------------------------------------------------
join :: (Ord k, Monad m)
     => Source m (k, a)
     -> Source m (k, b)
     -> Source m (k, (a, b))
join start_l start_r = appended =$= start
  where
    appended = do
      mapOutput Left start_l
      mapOutput Right start_r

    start = onLeft M.empty

    onLeft m = do
      res <- await
      case res of
        Nothing -> return ()
        Just e  ->
          case e of
            Left (vid, a) -> onLeft (M.insert vid (Left a) m)
            right         -> leftover right >> onRight m

    onRight m = do
      res <- await
      case res of
        Nothing -> finish m
        Just e  ->
          case e of
            Right (vid, b) ->
              onRight (M.adjust (\(Left a) -> Right (a, b)) vid m)
            _ -> return ()

    finish m = do
      for_ (M.assocs m) $ \(vid, res) ->
        case res of
          Right tup -> yield (vid, tup)
          _         -> return ()

--------------------------------------------------------------------------------
