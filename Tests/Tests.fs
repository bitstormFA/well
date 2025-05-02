module Tests

open Xunit
open FsUnit 
open Lib.SmilesParser
open Lib.Graph
open FParsec

let test p str =
    let initState = ParseState.Default
    runParserOnString p initState "" str

[<Fact>]
let ``Create Empty graph`` () =
    let g = MutableGraph<int, int>.Empty
    numberOfEdges g |> should equal 0
    numberOfNodes g |> should equal 0
    
[<Fact>]
let ``Add nodes and edges to graph`` () =
    let g = MutableGraph<string, string>.Empty
    let nID1 = addNode "1" g
    numberOfNodes g |> should equal 1
    numberOfEdges g |> should equal 0
    let ana1 = addNodeAt "2" nID1 "E1" g
    let e1 = ana1.Value
    numberOfNodes g |> should equal 2
    numberOfEdges g |> should equal 1

    let nID2 = if e1.fromID = nID1 then e1.toID else e1.fromID
    let nID3 = addNode "3" g
    addEdge {fromID=nID1;toID=nID3;edgeData="E2"} g |> should be True
    numberOfNodes g |> should equal 3
    numberOfEdges g |> should equal 2
    hasEdgeBetween nID1 nID2 g |> should be True
    hasEdgeBetween nID1 nID3 g |> should be True 
    removeNode nID3 g |> should be True
    numberOfNodes g |> should equal 2
    let current_edges = edgesFromGraph g |> List.ofSeq
    numberOfEdges g |> should equal 1
    