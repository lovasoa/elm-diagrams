module GraphEditor.Controller exposing (..)

import Debug

import Diagrams.Interact exposing (..)
import Diagrams.Geom exposing (..)

import GraphEditor.Model exposing (..)
import GraphEditor.View exposing (..)

-- the view has to import these types, but we have to import the view...

update : UpdateFunc State Action
update action state =
    case action of
      DragNodeStart attrs -> { state | dragState = Just <| DraggingNode attrs }
      DragEdgeStart attrs -> { state | dragState = Just <| DraggingEdge attrs }
      DragMove mousePos ->
          case state.dragState of
            Just (DraggingNode attrs) -> { state | graph = moveNode state.graph attrs.nodeId (mousePos `pointSubtract` attrs.offset) }
            Just (DraggingEdge attrs) -> { state | dragState = Just <| DraggingEdge { attrs | endPos = mousePos } }
            Nothing -> state
      DragEnd -> { state | dragState = Nothing }
      RemoveNode nodeId -> { state | graph = removeNode state.graph nodeId }
      RemoveEdge edge -> { state | graph = removeEdge state.graph edge }
      AddEdge edge -> { state | graph = addEdge state.graph edge
                              , dragState = Nothing }
      NoOp -> state

render : RenderFunc State Tag Action
render = view
