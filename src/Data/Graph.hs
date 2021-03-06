{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Graph where

import Control.Applicative (liftA2)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.State.Class (MonadState, state, modify, get, put)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.State (StateT, execStateT, runStateT)
import Data.Functor.Identity (Identity, runIdentity)

data GraphError a
  = NonExistingVertexError a
  | AlreadyExistingEdgeError a a
  deriving (Eq, Show)

newtype Vertex a
  = Vertex
  { getElement :: a
  }
  deriving (Eq, Show)

data Edge v a
  = Edge
  { from :: Vertex v
  , to :: Vertex v
  , getWeight :: a
  }
  deriving (Eq, Show)

class Graph g w a where
  emptyGraph :: g w a

  vertices :: g w a -> [Vertex a]

  numVertices :: g w a -> Int
  numVertices = length . vertices

  edges :: g w a -> [Edge a w]

  numEdges :: g w a -> Int
  numEdges = length . edges

  getEdge :: (Vertex a) -> (Vertex a) -> g w a -> Maybe (Edge a w)

  insertVertex :: a -> g w a -> (Vertex a, g w a)

  insertEdge :: (Vertex a) -> (Vertex a) -> w -> g w a -> (Either (GraphError a) (Edge a w), g w a)

  removeVertex :: Vertex a -> g w a -> g w a

  removeEdge :: Edge a w -> g w a -> g w a

  outDegree :: Vertex a -> g w a -> Either (GraphError a) Int

  inDegree :: Vertex a -> g w a -> Either (GraphError a) Int

  outgoingEdges :: Vertex a -> g w a -> Either (GraphError a) [Edge a w]

  incomingEdges :: Vertex a -> g w a -> Either (GraphError a) [Edge a w]

mkEdge :: a -> a -> w -> Edge a w
mkEdge v v' w = Edge (Vertex v) (Vertex v') w

endVertices :: Edge a w -> (Vertex a, Vertex a)
endVertices = liftA2 (,) from to

oposite :: Eq a => Vertex a -> Edge a w -> Maybe (Vertex a)
oposite v e | from e == v = Just (to e)
            | to e == v = Just (from e)
            | otherwise = Nothing

insertVertexS :: (Graph g w a, MonadState (g w a) m) => a -> m (Vertex a)
insertVertexS v = state $ insertVertex v

insertEdgeS :: (Graph g w a, MonadState (g w a) m, MonadError (GraphError a) m) => Vertex a -> Vertex a -> w -> m (Edge a w)
insertEdgeS v v' w = do
  g <- get
  let (egee, g') = insertEdge v v' w g
  put g'
  case egee of
    Left ge -> throwError ge
    Right e -> return e

removeVertexS :: (Graph g w a, MonadState (g w a) m) => Vertex a -> m ()
removeVertexS v = modify $ removeVertex v

removeEdgeS :: (Graph g w a, MonadState (g w a) m) => Edge a w -> m ()
removeEdgeS e = modify $ removeEdge e

withGraph :: Graph g w a => g w a -> StateT (g w a) (ExceptT e Identity) b  -> Either e (b, g w a)
withGraph graph = runIdentity . withGraphT graph

withGraph_ :: Graph g w a => g w a -> StateT (g w a) (ExceptT e Identity) b  -> Either e (g w a)
withGraph_ graph = runIdentity . withGraphT_ graph

withGraphT :: Graph g w a => g w a -> StateT (g w a) (ExceptT e m) b -> m (Either e (b, g w a))
withGraphT graph = runExceptT . flip runStateT graph

withGraphT_ :: (Graph g w a, Monad m) => g w a -> StateT (g w a) (ExceptT e m) b -> m (Either e (g w a))
withGraphT_ graph = runExceptT . flip execStateT graph

buildGraph :: Graph g w a => StateT (g w a) (ExceptT e Identity) b  -> Either e (b, g w a)
buildGraph = runIdentity . buildGraphT

buildGraph_ :: Graph g w a => StateT (g w a) (ExceptT e Identity) b  -> Either e (g w a)
buildGraph_ = runIdentity . buildGraphT_

buildGraphT :: Graph g w a => StateT (g w a) (ExceptT e m) b -> m (Either e (b, g w a))
buildGraphT = withGraphT emptyGraph

buildGraphT_ :: (Graph g w a, Monad m) => StateT (g w a) (ExceptT e m) b -> m (Either e (g w a))
buildGraphT_ = withGraphT_ emptyGraph
