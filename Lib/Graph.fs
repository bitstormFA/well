module Lib.Graph

open System
open System.Text
open FSharpx.Collections
open System.Collections.Generic
open Lib.Types
open Types

type NodeID = int
type EdgeID = int

let createCounter initial =
    let mutable state = initial

    (fun () ->
        let current = state
        state <- current + 1
        current)

type Edge<'EdgeData when 'EdgeData: equality> =
    { fromID: NodeID
      toID: NodeID
      edgeData: 'EdgeData }

type MutableGraph<'NodeData, 'EdgeData when 'NodeData: equality and 'EdgeData: equality> =
    { NodeData: Dictionary<NodeID, 'NodeData>
      NodeIDFactory: unit -> int
      AdjecencyList: Dictionary<NodeID, Dictionary<NodeID, 'EdgeData>> }

    static member Empty:MutableGraph<'EdgeData, 'NodeData> =
        { NodeData = Dictionary()
          AdjecencyList = Dictionary()
          NodeIDFactory = createCounter 0
        }

let tryGetFromDict (k: 'K) (d: Dictionary<'K, 'V>) : 'V option =
    match d.ContainsKey k with
    | true -> Some d.[k]
    | false -> None


let orderNodeIDs (id1: NodeID) (id2: NodeID) : NodeID * NodeID =
    if hash (id1) < hash (id2) then
        id1, id2
    else
        id2, id1

let numberOfNodes (graph: MutableGraph<_, _>) : int = graph.NodeData.Count

let getNodeData nodeID graph =
    match graph.NodeData.ContainsKey nodeID with
    | true -> Some graph.NodeData[nodeID]
    | false -> None
    
let getNodeLabel nodeID  graph =
    let nodeData = getNodeData nodeID graph
    match nodeData with
    | Some nd -> string(nd)
    | None -> ""

let getEdges (graph: MutableGraph<_, 'EdgeData>) : Edge<'EdgeData> seq =
    graph.AdjecencyList |>
    Seq.map (|KeyValue|)|>
    Seq.map (fun (nodeID, dict) ->
        dict |>
        Seq.map (|KeyValue|) |>
        Seq.map (fun (n2, ed) ->
            { fromID = nodeID
              toID = n2
              edgeData = ed }))
    |> Seq.concat

let getConnectedNodes (nodeID:NodeID) (graph:MutableGraph<_,_>) : NodeID list =
    match graph.AdjecencyList.ContainsKey nodeID with
    | false -> []
    | true -> graph.AdjecencyList[nodeID]
              |> (fun x -> x.Keys)
              |> List.ofSeq

let isDirectlyConnected (node1:NodeID) (node2:NodeID) (graph:MutableGraph<_,_>) : bool =
    let sorted1, sorted2 = orderNodeIDs node1 node2
    graph.AdjecencyList.ContainsKey sorted1 && graph.AdjecencyList[node1].ContainsKey sorted2

let numberOfEdges (graph: MutableGraph<_, _>) =
    graph |> getEdges |> Seq.length   
    
let addNode (nodeData:'NodeData) (graph: MutableGraph<_, _>) : NodeID =
    let newID = graph.NodeIDFactory()
    graph.NodeData.[newID] <- nodeData
    graph.AdjecencyList.[newID] <- Dictionary()
    newID
    
let removeNode (nodeId:NodeID) (graph: MutableGraph<_,_>) : bool =
    if not (graph.NodeData.ContainsKey(nodeId)) then false
    else
       graph.NodeData.Remove(nodeId) |> ignore
       graph.AdjecencyList.Remove(nodeId) |> ignore
       graph.AdjecencyList |> Seq.map (|KeyValue|) |> Seq.filter (fun (_,v) -> v.ContainsKey nodeId) |> Seq.iter(fun (_,v) -> v.Remove nodeId |> ignore)             
       true
       
let nodeWithID (nodeId:NodeID)(graph: MutableGraph<_, _>) : 'NodeData option =
    match graph.NodeData.ContainsKey nodeId with
        | true -> Some graph.NodeData[nodeId]
        | false -> None 
    

let addNodeToNode (nodeData: 'NodeData) (connectID: NodeID) (edgeData: 'EdgeData) (graph: MutableGraph<'NodeData, 'EdgeData>) : (Edge<'EdgeData> * NodeID) option =
    if not (graph.NodeData.ContainsKey(connectID)) then None  // the node to which should be connected is unknown
    else
        let newNodeID = graph |> addNode nodeData
        let n1ID, n2ID = orderNodeIDs newNodeID connectID  // make sure bonds are always in the same order so they can be identified
        let edge = {fromID=n1ID; toID=n2ID; edgeData=edgeData}
        graph.AdjecencyList[n1ID].Add(n2ID, edgeData)
        Some (edge, newNodeID)

            
let addEdge (edge:Edge<'EdgeData>) (graph: MutableGraph<'NodeData, 'EdgeData>): bool =
    if graph.NodeData.ContainsKey(edge.fromID) && graph.NodeData.ContainsKey(edge.toID) then
        let n1Id, n2Id = orderNodeIDs edge.fromID edge.toID
        graph.AdjecencyList.[n1Id].[n2Id] <- edge.edgeData 
        true
    else
        false
        
let hasEdgeBetween (n1:NodeID) (n2: NodeID) (graph: MutableGraph<_, _>) : bool =
    isDirectlyConnected n1 n2 graph
            
type MolGraph = MutableGraph<Atom, BondType>

let dotGraph graph : string =               
    let result = StringBuilder()
    result.Append("graph {\n") |> ignore
    graph.NodeData
    |> Map.ofDict
    |> Map.fold (fun (state:StringBuilder) k v -> state.Append($"node_{k} [label=\"{getNodeLabel k graph}\"];\n")) result |> ignore
    getEdges graph
    |> Seq.fold (fun (result:StringBuilder) edge -> result.Append($"node_{edge.fromID}--node_{edge.toID}[label=\"{edge.edgeData}\"];\n")) result |> ignore
    result.Append("}\n") |> ignore
    result.ToString()


type SearchType =
    | DFS
    | BFS

let listNodes (startNode: NodeID) (searchType:SearchType) (graph: MutableGraph<_, _>) : NodeID list =
    let dq = LinkedList<NodeID>()
    let result = HashSet<NodeID>()
    dq.AddFirst(startNode) |> ignore
    while dq.Count > 0 do
        let current = dq.First.Value
        dq.RemoveFirst()
        if  not (result.Contains(current)) then
            result.Add current |> ignore
            getConnectedNodes current graph
            |> List.iter (fun x -> if searchType = DFS then dq.AddFirst(x) |> ignore else dq.AddLast(x) |> ignore)
    result |> List.ofSeq
               

let dfs (startNode: NodeID) (graph: MutableGraph<_, _>) : NodeID list =
    listNodes startNode SearchType.DFS graph
    
let bfs (startNode: NodeID) (graph: MutableGraph<_, _>) : NodeID list =
    listNodes startNode SearchType.BFS graph