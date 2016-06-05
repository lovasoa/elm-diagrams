module GraphEditor.Main exposing (..)

import Dict as D
import Signal
import Window

import Diagrams.Type exposing (Diagram)
import Diagrams.Wiring as DW
import Diagrams.FullWindow as DFW
import Diagrams.Interact as DI

import GraphEditor.Model exposing (..)
import GraphEditor.Controller as Cont

-- DATA

fooNode = { title = "Foo", inPorts = ["InAasdfasdfsdafasdfs", "asdfs", "InB", "InC"], outPorts = ["out1", "out2"] }
fooPosNode = { node = fooNode, pos = (-300, 100), id = "foo" }

bazNode = { title = "Baz", inPorts = ["InA", "InB", "InC"], outPorts = ["out1", "out2"] }
bazPosNode = { node = bazNode, pos = (100, -200), id = "baz" }

barNode = { title = "Bar", inPorts = ["InA", "InB", "InC"], outPorts = ["out1", "out2"] }
barPosNode = { node = barNode, pos = (100, 100), id = "bar" }

fooBarEdge = { from = ("foo", "out1"), to = ("bar", "InA") }
--fooBazEdge = { from = ("foo", "out2"), to = ("baz", "InC") }

initGraph = { nodes = D.fromList [ (fooPosNode.id, fooPosNode)
                                 , (barPosNode.id, barPosNode)
                                 , (bazPosNode.id, bazPosNode)
                                 ]
            , edges = [fooBarEdge] }

initState = { graph = initGraph, dragState = Nothing }

-- start 'er up

diagrams : Signal (Diagram Tag Action)
diagrams = DI.interactFold Cont.update Cont.render DFW.fullWindowCollageLocFunc initState

main = Signal.map2 DFW.fullWindowView Window.dimensions diagrams
