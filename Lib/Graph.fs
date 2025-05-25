module Lib.Graph

open System
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

let edgesFromGraph (graph: MutableGraph<'NodeData, 'EdgeData>) : Edge<'EdgeData> seq =
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
    graph |> edgesFromGraph |> Seq.length   
    
let addNode (nodeData:'NodeData) (graph: MutableGraph<'NodeData, 'EdgeData>) : NodeID =
    let newID = graph.NodeIDFactory()
    graph.NodeData.[newID] <- nodeData
    graph.AdjecencyList.[newID] <- Dictionary()
    newID
    
let removeNode (nodeId:NodeID) (graph: MutableGraph<'NodeData, 'EdgeData>) : bool =
    if not (graph.NodeData.ContainsKey(nodeId)) then false
    else
       graph.NodeData.Remove(nodeId) |> ignore
       graph.AdjecencyList.Remove(nodeId) |> ignore
       graph.AdjecencyList |> Seq.map (|KeyValue|) |> Seq.filter (fun (k,v) -> v.ContainsKey nodeId) |> Seq.iter(fun (k,v) -> v.Remove nodeId |> ignore)             
       true
       
let nodeAtIndex (nodeId:NodeID)(graph: MutableGraph<'NodeData, 'EdgeData>) : 'NodeData option =
    match graph.NodeData.ContainsKey nodeId with
        | true -> Some graph.NodeData[nodeId]
        | false -> None 
    
/// Adds a new node with the given data to the graph and connects it to an existing node using the specified edge data.
/// Returns an option containing a tuple of the created edge and the ID of the new node, or None if the connection node ID does not exist.
///
/// Parameters:
///   nodeData: The data to be associated with the new node.
///   connectID: The ID of the existing node to which the new node is to be connected.
///   edgeData: The data to be associated with the edge connecting the nodes.
///   graph: The mutable graph to which the node is added.
///
/// Returns:
///   Some (edge, newNodeID): A tuple containing the created edge and the ID of the newly added node, if the connection was successful.
///   None: Indicates that the connectID was not found in the graph, and the operation failed.
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