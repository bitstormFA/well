module Tests

open FsUnitTyped
open Lib.Types
open Molecule
open Swensen.Unquote
open Xunit
open FsUnit 
open Lib.SmilesParser
open Lib.Graph

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
    let edge = {nodes=NodeIDSet.construct nID1 nID2; edgeData="e1"}
    let g  = addEdge edge g
    numberOfEdges g |> shouldEqual 1
    numberOfNodes g |> should equal 2
    hasEdgeBetween nID1 nID2 g |> should be True

[<Fact>]
let ``Remove nodes and edges to graph`` () =
    let g:Graph<string, string> = Graph.Empty
    let g, n1 = addNode "1" g
    let g, n2 = addNode "2" g
    let g, n3 = addNode "3" g
    let e1 = {nodes=NodeIDSet.construct n1 n2; edgeData="e12"}
    let e2 = {nodes=NodeIDSet.construct n1 n3; edgeData="e13"}
    let e3 = {nodes=NodeIDSet.construct n2 n3; edgeData="e23"}
    let gStart= g |> addEdge e1 |> addEdge e2 |> addEdge e3
    test <@ numberOfEdges gStart = 3 @>
    test <@ numberOfNodes gStart = 3 @>
    let gEdgeMinus1 = removeEdge n1 n2 gStart
    test <@ numberOfNodes gEdgeMinus1 = 3 @>
    test <@ numberOfEdges gEdgeMinus1 = 2 @>
    let gNodeMinus1 = removeNode n1 gStart
    test <@ numberOfNodes gNodeMinus1 = 2 @>
    test <@ numberOfEdges gNodeMinus1 = 1 @>
    
[<Fact>]
let ``Change nodes and edges to graph`` () =
    let g:Graph<string, string> = Graph.Empty
    let g, n1 = addNode "1" g
    let g, n2 = addNode "2" g
    let g, n3 = addNode "3" g
    let e1 = {nodes=NodeIDSet.construct n1 n2; edgeData="e12"}
    let e2 = {nodes=NodeIDSet.construct n1 n3; edgeData="e13"}
    let e3 = {nodes=NodeIDSet.construct n2 n3; edgeData="e23"}
    let gStart= g |> addEdge e1 |> addEdge e2 |> addEdge e3
    test <@ numberOfEdges gStart = 3 @>
    test <@ numberOfNodes gStart = 3 @>
    let changeNode1Graph = changeNode n1 "changed1" gStart
    test <@ changeNode1Graph.IsSome @>
    let changedNodeData = tryGetNodeData n1 changeNode1Graph.Value
    test <@ changedNodeData.Value = "changed1"  @>
    test <@ numberOfEdges changeNode1Graph.Value = 3 @>
    test <@ numberOfNodes changeNode1Graph.Value = 3 @> 
    let changeEdge1Graph = changeEdge n1 n2 "changedE12" gStart
    let changedEdgeData = getEdgeBetween n1 n2 changeEdge1Graph.Value
    test <@ changedEdgeData.IsSome @>
    test <@ changedEdgeData.Value.edgeData = "changedE12" @>
    test <@ numberOfEdges changeEdge1Graph.Value = 3 @>
    test <@ numberOfNodes changeEdge1Graph.Value = 3 @>   
    

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
let ``Test ez handling`` () =
    let mol = (smilesToMol "F/C=C/F").Value.Head
    test <@ numberOfNodes mol = 4 @>
    let bond1 = getEdgeBetween 0 1 mol
    test <@ bond1.IsSome @>
    test <@ bond1.Value.edgeData.Direction=BondDirection.CisTrans1 @>
    let bond2 = getEdgeBetween 2 3 mol
    test <@ bond2.IsSome @>
    test <@ bond2.Value.edgeData.Direction=BondDirection.CisTrans1 @>
    
    let mol2 = (smilesToMol @"F\C=C/F").Value.Head
    test <@ numberOfNodes mol = 4 @>
    let bond3 = getEdgeBetween 0 1 mol2
    test <@ bond3.IsSome @>
    test <@ bond3.Value.edgeData.Direction=BondDirection.CisTrans2 @>
    let bond4 = getEdgeBetween 2 3 mol2
    test <@ bond4.IsSome @>
    test <@ bond4.Value.edgeData.Direction=BondDirection.CisTrans1 @>

[<Fact>]
let ``Test simple graph functions`` () =
    let input = "CC(=O)Oc1ccccc1C(O)=O"
    let res = runParser smiles input
    res |> should not' (be Null)
    let graph = res.Value[0]
    test <@ numberOfNodes graph = 13 @>
    test <@ numberOfEdges graph = 13 @>
    edgesInAdjecencyList graph.AdjecencyList |> List.length |> shouldEqual 13 
    let removedAL = removeEdgesContaining 0 graph.AdjecencyList
    test <@ (edgesInAdjecencyList removedAL).Length = 12 @>
    test <@ getConnectedNodes 1 graph |> Set.ofSeq = (Set [0;2;3]) @>
    test <@ isDirectlyConnected 2 1 graph = true  @>
    test <@ isDirectlyConnected 1 2 graph = true  @>
    test <@ isDirectlyConnected 0 3 graph = false @>
    test <@ isDirectlyConnected 3 0 graph = false @>
    test <@ Option.isSome (getEdgeBetween 1 2 graph) @> 
    test <@ Option.isNone (getEdgeBetween 0 3 graph) @> 

[<Fact>]
let ``Test graph properties`` () =
    let graph = Graph.Empty
    let withComment = addProperty "comment" (GraphProperties.Comment ["comment1"; "comment2"]) graph
    test <@ hasProperty "comment" withComment @>
    test <@ (getProperty "comment" withComment).Value = (GraphProperties.Comment ["comment1"; "comment2"]) @>
    let removedComment = removeProperty "comment" withComment
    test <@ hasProperty "comment" removedComment = false@>
    test <@ (getProperty "comment" removedComment).IsNone  @>

[<Fact>]
let ``Test Dijkstra shortest path`` () =
    let input = "CC(=O)Oc1ccccc1C(O)=O"
    let mol = (runParser smiles input).Value.Head
    let dist, path = shortestPath 0 11 mol
    dist |> shouldEqual 6
    path |> shouldEqual [0;1;3;4;9;10;11]
    let dist2, path2 = shortestPath 11 0 mol
    dist2 |> shouldEqual 6
    path2 |> List.rev |>  shouldEqual [0;1;3;4;9;10;11]

[<Fact>]
let ``Test Floyd-Warshall shortest path`` () =
    let input = "CC(=O)Oc1ccccc1C(O)=O"
    let mol = (runParser smiles input).Value.Head
    let dist, prev = floydWarshall mol
    let path = reconstructFloydWarshallPath 0 11 dist prev
    path |> shouldNotEqual None
    path.Value |> shouldEqual [0;1;3;4;9;10;11]
    let path2 = reconstructFloydWarshallPath 11 0 dist prev
    path2.Value |> List.rev |> shouldEqual path.Value
    
[<Fact>]
let ``Test cycle base`` () =
    let input = "C1=NC(=C2C(=N1)N(C=N2)[C@H]3[C@@H]([C@@H]([C@H](O3)CO)O)O)N"
    let mol = (smilesToMol input).Value.Head
    let cb = findMinimumCycleBasis mol
    test <@ cb.Length=3 @>

[<Fact>]    
let ``Test update valences``() =
    let input = "Cc1ccccc1C"
    let mol = (smilesToMol input).Value.Head
    let molWithValences = updateImplicitHydrogens mol
    let testAtom1 = getAtom 0 molWithValences
    let testAtom2 = getAtom 1 molWithValences
    test <@ (atomIDs molWithValences) |> Seq.length = 8@>
    test <@ testAtom1.Hydrogens = Some 3@>
    test <@ testAtom2.Hydrogens = Some 0@>
    
    