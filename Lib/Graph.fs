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


let orderNodeIDs (node1: NodeID) (node2: NodeID) : NodeID * NodeID =
    if hash (node1) < hash (node2) then
        node1, node2
    else
        node2, node1

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
       
let nodeAtIndex (nodeId:NodeID)(graph: MutableGraph<_, _>) : 'NodeData option =
    match graph.NodeData.ContainsKey nodeId with
        | true -> Some graph.NodeData[nodeId]
        | false -> None 
    

let addNodeAt (nodeData: 'NodeData) (connectID: NodeID) (edgeData: 'EdgeData) (graph: MutableGraph<'NodeData, 'EdgeData>) : (Edge<'EdgeData> * NodeID) option =
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
        
let hasEdgeBetween (n1:NodeID) (n2: NodeID) (graph: MutableGraph<'NodeData, 'EdgeData>) : bool =
    if not (graph.NodeData.ContainsKey(n1)) or not (graph.NodeData.ContainsKey(n2)) then false
    else
        let n1ID, n2ID = orderNodeIDs n1 n2
        if graph.AdjecencyList.ContainsKey n1ID then
            graph.AdjecencyList.[n1ID].ContainsKey(n2ID)
        else
            false
            
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

            
