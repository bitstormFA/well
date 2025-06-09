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
    let g:Graph<int,int> = Graph.Empty
    numberOfEdges g |> should equal 0
    numberOfNodes g |> should equal 0
    
[<Fact>]
let ``Add nodes and edges to graph`` () =
    let g:Graph<string, string> = Graph.Empty
    let g, nID1 = addNode "1" g
    numberOfNodes g |> should equal 1
    numberOfEdges g |> should equal 0
    let g, nID2 = addNode "2" g
    let edge = {fromID=nID1; toID=nID2; edgeData="e1"}
    let g  = addEdge edge g
    numberOfEdges g |> shouldEqual 1
    numberOfNodes g |> should equal 2
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
let ``Test shortest path`` () =
    let input = "CC(=O)Oc1ccccc1C(O)=O"
    let mol = (runParser smiles input).Value.Head
    let dist, path = shortestPath 1 12 mol
    dist |> shouldEqual 6
    path |> shouldEqual [1;2;4;5;10;11;12]
    