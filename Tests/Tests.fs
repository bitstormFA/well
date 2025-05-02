module Tests

open System
open Xunit
open Lib.SmilesParser
open Lib.Graph
open FParsec

let test p str =
    let initState = ParseState.Default
    runParserOnString p initState "" str

[<Fact>]
let ``Create Empty graph`` () =
    let g = MutableGraph<int, int>.Empty
    Assert.Equal(0, numberOfNodes g)
    Assert.Equal(0, numberOfEdges g)
    
[<Fact>]
let ``Add nodes and edges to graph`` () =
    let g = MutableGraph<string, string>.Empty
    let nID1 = addNode "1" g
    Assert.Equal(1, numberOfNodes g)
    Assert.Equal(0, numberOfEdges g)
    let ana1 = addNodeAt "2" nID1 "E1" g
    let e1 = ana1.Value
    Assert.Equal(2, numberOfNodes g)
    Assert.Equal(1, numberOfEdges g)
    let nID2 = if e1.fromID = nID1 then e1.toID else e1.fromID
    let nID3 = addNode "3" g
    Assert.True(addEdge {fromID=nID1;toID=nID3;edgeData="E2"} g)
    Assert.Equal(3, numberOfNodes g)
    Assert.Equal(2, numberOfEdges g)
    Assert.True(hasEdgeBetween nID1 nID2 g)
    Assert.True(hasEdgeBetween nID1 nID3 g)
    Assert.True(removeNode nID3 g)
    Assert.Equal(2, numberOfNodes g)
    let current_edges = edgesFromGraph g |> List.ofSeq
    Assert.Equal(1, numberOfEdges g)
    