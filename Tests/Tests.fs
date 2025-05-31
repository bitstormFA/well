module Tests

open FsUnitTyped
open Lib.Types
open Xunit
open FsUnit 
open Lib.SmilesParser
open Lib.Graph
open FParsec

let runParser p str  =
    let initState = ParseState.Default
    let parseResult = runParserOnString p initState "" str 
    match parseResult with 
    | ParserResult.Success (result, state, _) -> Some result
    | ParserResult.Failure _ -> None

[<Fact>]
let ``Create Empty graph`` () =
    let g:MutableGraph<int,int> = MutableGraph.Empty
    numberOfEdges g |> should equal 0
    numberOfNodes g |> should equal 0
    
[<Fact>]
let ``Add nodes and edges to graph`` () =
    let g:MutableGraph<string, string> = MutableGraph.Empty
    let nID1 = addNode "1" g
    numberOfNodes g |> should equal 1
    numberOfEdges g |> should equal 0
    let nID2 = addNode "2" g
    let edge = {fromID=nID1; toID=nID2; edgeData="e1"}
    let edgeCreated = addEdge edge g
    edgeCreated |> should be True 
    numberOfNodes g |> should equal 2
    numberOfEdges g |> should equal 1
    hasEdgeBetween nID1 nID2 g |> should be True


[<Fact>]
let ``Test Smiles Parser`` () =
    let input = "CC(=O)Oc1ccccc1C(O)=O"
    let res = runParser smiles input
    res |> should not' (be Null)
    let resVal = res.Value
    resVal.Length |> should equal 1
    let aspirinMol = resVal[0]
    let plot = dotGraph aspirinMol
    plot.Length |> shouldBeGreaterThan 0

[<Fact>]
let ``Test DFS and BFS`` () =
    let g:MutableGraph<string, string> = MutableGraph.Empty
    let nID0 = addNode "n1" g
    let nID1 = (addNodeToNode "n1" nID0 "e0" g).Value |> snd
    let nID2 = (addNodeToNode "n2" nID1 "e1" g).Value |> snd
    let nID3 = (addNodeToNode "n3" nID1 "e2" g).Value |> snd
    let nID4 = (addNodeToNode "n4" nID2 "e3" g).Value |> snd
    let nID5 = (addNodeToNode "n5" nID4 "e4" g).Value |> snd
    let nID6 = (addNodeToNode "n6" nID5 "e5" g).Value |> snd
    let nID7 = (addNodeToNode "n7" nID2 "e6" g).Value |> snd
    let nID8 = (addNodeToNode "n8" nID3 "e7" g).Value |> snd
    let nID9 = (addNodeToNode "n9" nID8 "e8" g).Value |> snd
    addEdge {Edge.fromID=nID7; toID=nID5; edgeData="e8"} g |> ignore
    addEdge {Edge.fromID=nID7; toID=nID5; edgeData="e8"} g |> ignore
    
    let dfsNodeList = dfs nID0 g
    dfsNodeList |> shouldEqual [0; 1; 3; 8; 9; 2; 7; 4; 5; 6]
    let bfsNodeList = bfs nID0 g
    bfsNodeList |> shouldEqual [0; 1; 2; 3; 4; 7; 8; 5; 9; 6]
