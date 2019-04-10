{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Graph.AdjacencyListMapGraph where

import Control.Monad (when)
import Data.Either.Combinators (maybeToRight)
import Data.Graph
  ( Graph(..)
  , GraphError(..)
  , Vertex(..)
  , Edge(..)
  , mkEdge
  )
import Data.List (find)
import qualified Data.Map as M

data AdjacencyListMapGraph w a
  = AdjacencyListMapGraph (M.Map a [(a, w)])
  deriving (Eq, Show)

instance Ord a => Graph AdjacencyListMapGraph w a where
  emptyGraph = AdjacencyListMapGraph $ M.fromList []

  vertices (AdjacencyListMapGraph m) = Vertex <$> M.keys m

  edges (AdjacencyListMapGraph m) = M.foldlWithKey' combine [] m
    where
      combine es v vs = es ++ (uncurry (mkEdge v) <$> vs)

  getEdge (Vertex v) (Vertex v') (AdjacencyListMapGraph m) = do
    vs <- M.lookup v m
    (_, w) <- find ((== v') . fst) vs
    return $ mkEdge v v' w

  insertVertex v (AdjacencyListMapGraph m)
    = (Vertex v, AdjacencyListMapGraph $ M.insert v [] m)

  insertEdge (Vertex v) (Vertex v') w (AdjacencyListMapGraph m) = (edge, graph)
    where
      edge = do
        vs <- maybeToRight (NonExistingVertexError v) $ M.lookup v m
        when (not (M.member v' m)) $ Left (NonExistingVertexError v')
        if any ((== v') . fst) vs
          then Left (AlreadyExistingEdgeError v v')
          else Right $ mkEdge v v' w
      graph = case edge of
        Left _ -> (AdjacencyListMapGraph m)
        Right _ -> AdjacencyListMapGraph $ M.adjust (++ [(v', w)]) v m

  removeVertex (Vertex v) (AdjacencyListMapGraph m)
    = AdjacencyListMapGraph $ M.map (filter ((/= v) . fst)) $ M.delete v m

  removeEdge (Edge (Vertex v) (Vertex v') w) (AdjacencyListMapGraph m)
    = AdjacencyListMapGraph $ M.adjust (filter ((/= v') . fst)) v m

  outDegree (Vertex v) (AdjacencyListMapGraph m)
    = maybeToRight (NonExistingVertexError v) $ fmap length $ M.lookup v m

  inDegree (Vertex v) (AdjacencyListMapGraph m) = do
    when (not (M.member v m)) $ Left (NonExistingVertexError v)
    return $ M.foldl' combine 0 m
    where
      combine x vs =
        if any ((== v) . fst) vs
          then x + 1
          else x

  outgoingEdges (Vertex v) (AdjacencyListMapGraph m)
    = maybeToRight (NonExistingVertexError v) $ fmap (uncurry (mkEdge v)) <$> M.lookup v m

  incomingEdges (Vertex v) (AdjacencyListMapGraph m) = do
    when (not (M.member v m)) $ Left $ NonExistingVertexError v
    return $ M.foldlWithKey' combine [] m
    where
      combine es v' vs = es ++ ((uncurry (mkEdge v')) <$> filter ((== v) . fst) vs)
