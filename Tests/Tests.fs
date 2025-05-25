module Tests

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
    | ParserResult.Success (_, state, _) -> Some state
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


[<Fact>]
let ``Test Smiles Parser`` () =
    let input = "CC(=O)Oc1ccccc1C(O)=O"
    let res = runParser chain input
    printfn("")