{-# LANGUAGE BlockArguments #-}

module Main where

import Data.Graph
  ( GraphError
  , buildGraph_
  , insertVertexS
  , insertEdgeS
  , removeVertexS
  , removeEdgeS
  )
import Data.Graph.AdjacencyListMapGraph (AdjacencyListMapGraph)

main :: IO ()
main = print graph
  where
    graph :: Either (GraphError Char) (AdjacencyListMapGraph Int Char)
    graph = buildGraph_ do
      va <- insertVertexS 'a'
      vb <- insertVertexS 'b'
      vc <- insertVertexS 'c'
      vd <- insertVertexS 'd'
      insertVertexS 'e'

      removeVertexS vd

      insertEdgeS va vb 1
      vac <- insertEdgeS va vc 1
      insertEdgeS vb vc 1

      removeEdgeS vac
