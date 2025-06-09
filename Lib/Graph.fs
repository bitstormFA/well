module Lib.Graph

open System.Text
open FSharp.HashCollections.HashMap
open FSharpx.Collections
open System.Collections.Generic
open Lib.Types
open FSharp.HashCollections
open Microsoft.FSharp.Collections
type NodeID = int
type EdgeID = int

[<Struct>]
type Edge<'EdgeData when 'EdgeData: equality> =
    { fromID: NodeID
      toID: NodeID
      edgeData: 'EdgeData }
    
[<Struct>]
type Node<'NodeData> =
    {
        id: NodeID
        data: 'NodeData
    }
    
type GraphAdjecencyList<'EdgeData> = HashMap<NodeID, HashMap<NodeID, 'EdgeData>>

let addEdgeData<'EdgeData> n1id (n2id:NodeID) (edgeData:'EdgeData) (adjecencyList:GraphAdjecencyList<'EdgeData>) : GraphAdjecencyList<'EdgeData> =
    if containsKey n1id adjecencyList then
        let inner = adjecencyList[n1id] |> remove n2id |> add n2id edgeData
        adjecencyList |> remove n1id |> add n1id inner 
    else
        let inner = HashMap.empty |> HashMap.add n2id edgeData
        adjecencyList |> add n1id inner 

let removeEdgesContaining (nID:NodeID) (adjecencyList:GraphAdjecencyList<'EdgeData>) : GraphAdjecencyList<'EdgeData> =
    let outer = adjecencyList |> remove nID
    outer |> toSeq |> Seq.filter (fun struct(k,v) -> not (containsKey nID v)) |> Seq.map (fun struct(k,v) -> KeyValuePair(k,v)) |> ofSeq

let edgesInAdjecencyList (adjecencyList:GraphAdjecencyList<_>) =
    adjecencyList |> Seq.map (|KeyValue|)|>
    Seq.map (fun (nodeID, dict) ->
        dict |>
        Seq.map (|KeyValue|) |>
        Seq.map (fun (n2, ed) ->
            { fromID = nodeID
              toID = n2
              edgeData = ed }))
    |> Seq.concat

let numberEdgesInAdjecencyList (adjecencyList:GraphAdjecencyList<_>) : int =
    toSeq adjecencyList |>
    Seq.map (fun struct(_,v) -> HashMap.count v) |> Seq.sum
    

[<Struct>]
type Graph<'NodeData, 'EdgeData when 'NodeData: equality and 'EdgeData: equality> =
    { NodeData: HashMap<NodeID, 'NodeData>
      AdjecencyList: GraphAdjecencyList<'EdgeData>
      LastId: int
      }

    static member Empty:Graph<'EdgeData, 'NodeData> =
        { NodeData = HashMap.empty
          AdjecencyList = HashMap.empty
          LastId = 0
        }

let orderNodeIDs (id1: NodeID) (id2: NodeID) : NodeID * NodeID =
    if hash (id1) < hash (id2) then
        id1, id2
    else
        id2, id1

let numberOfNodes (graph: Graph<_, _>) : int = HashMap.count graph.NodeData

let NodesIDs graph =
    HashMap.keys graph.NodeData
    
let NodeData graph =
    HashMap.values graph.NodeData

let Nodes graph =
    graph.NodeData |> HashMap.toSeq |> Seq.map (fun struct (k,v) -> {Node.id=k; Node.data=v})
     
let getNodeData nodeID graph =
    graph.NodeData.TryFind nodeID 
    
let getNodeLabel nodeID  graph =
    let nodeData = getNodeData nodeID graph
    match nodeData with
    | Some nd -> string(nd)
    | None -> ""

let Edges (graph: Graph<_, 'EdgeData>) : Edge<'EdgeData> seq =
    graph.AdjecencyList |> edgesInAdjecencyList

let getConnectedNodes (nodeID:NodeID) (graph:Graph<_,_>) : NodeID list =
    match HashMap.containsKey nodeID graph.AdjecencyList with
    | false -> []
    | true -> graph.AdjecencyList[nodeID]
              |> HashMap.keys
              |> List.ofSeq

let isDirectlyConnected (node1:NodeID) (node2:NodeID) (graph:Graph<_,_>) : bool =
    let sorted1, sorted2 = orderNodeIDs node1 node2
    HashMap.containsKey sorted1 graph.AdjecencyList && HashMap.containsKey sorted2 graph.AdjecencyList[node1]
    
let numberOfEdges (graph: Graph<_, _>) =
    graph.AdjecencyList |> numberEdgesInAdjecencyList
    
let addNode (nodeData:'NodeData) (graph: Graph<_, _>) : Graph<_, _> * NodeID =
    let newID = graph.LastId + 1
    {
        Graph.NodeData = HashMap.add newID nodeData graph.NodeData
        Graph.AdjecencyList = graph.AdjecencyList
        LastId = newID
    }, newID
    
let addNodeFlow nodeData graph =
    let g, newId = addNode nodeData graph
    g

let removeNode (nodeId:NodeID) (graph: Graph<'NodeData, 'EdgeData>) : Graph<'NodeData, 'EdgeData> =
       {
           Graph.NodeData = HashMap.remove nodeId graph.NodeData
           Graph.AdjecencyList = removeEdgesContaining nodeId graph.AdjecencyList
           Graph.LastId = graph.LastId
       }
       
let findNodeWithID (nodeId:NodeID)(graph: Graph<'NodeData, 'EdgeData>) : 'NodeData voption =
    tryFind nodeId graph.NodeData

let addNodeToNode (nodeData: 'NodeData) (connectID: NodeID) (edgeData: 'EdgeData) (graph: Graph<'NodeData, 'EdgeData>) : Graph<_,_> * (Edge<'EdgeData> * NodeID) option =
    if not (HashMap.containsKey connectID graph.NodeData) then graph, None  // the node to which should be connected is unknown
    else
        let graph, newNodeID = graph |> addNode nodeData
        let n1ID, n2ID = orderNodeIDs newNodeID connectID  // make sure bonds are always in the same order so they can be identified
        let edge = {fromID=n1ID; toID=n2ID; edgeData=edgeData}
        let graph = {graph with AdjecencyList=graph.AdjecencyList |> addEdgeData n1ID n2ID edgeData}
        graph, Some (edge, newNodeID)

let addNodeToNodeFlow (nodeData: 'NodeData) (connectID: NodeID) (edgeData: 'EdgeData) (graph: Graph<'NodeData, 'EdgeData>) : Graph<_,_> =
    let g, _ = addNodeToNode nodeData connectID edgeData graph
    g
            
let addEdge (edge:Edge<'EdgeData>) (graph: Graph<'NodeData, 'EdgeData>): Graph<'NodeData, 'EdgeData> =
    if containsKey edge.fromID graph.NodeData && containsKey edge.toID graph.NodeData then
        let n1id, n2id = orderNodeIDs edge.fromID edge.toID
        {graph with AdjecencyList=addEdgeData n1id n2id edge.edgeData graph.AdjecencyList}
    else
        graph
        
let hasEdgeBetween (n1:NodeID) (n2: NodeID) (graph: Graph<_, _>) : bool =
    isDirectlyConnected n1 n2 graph
            
type MolGraph = Graph<Atom, BondType>

let atoms (mol:MolGraph) =
    Edges mol

let dotGraph graph : string =               
    let result = StringBuilder()
    result.Append("graph {\n") |> ignore
    graph.NodeData
    |> Map.ofDict
    |> Map.fold (fun (state:StringBuilder) k v -> state.Append($"node_{k} [label=\"{getNodeLabel k graph}\"];\n")) result |> ignore
    Edges graph
    |> Seq.fold (fun (result:StringBuilder) edge -> result.Append($"node_{edge.fromID}--node_{edge.toID}[label=\"{edge.edgeData}\"];\n")) result |> ignore
    result.Append("}\n") |> ignore
    result.ToString()


type SearchType =
    | DFS
    | BFS

let listNodes (startNode: NodeID) (searchType:SearchType) (graph: Graph<_, _>) : NodeID list =
    let dq = LinkedList<NodeID>()
    let result = Set.empty
    dq.AddFirst(startNode) |> ignore
    while dq.Count > 0 do
        let current = dq.First.Value
        dq.RemoveFirst()
        if  not (result.Contains current) then
            result.Add current |> ignore
            getConnectedNodes current graph
            |> List.iter (fun x -> if searchType = DFS then dq.AddFirst(x) |> ignore else dq.AddLast(x) |> ignore)
    result |> List.ofSeq
               

let dfs (startNode: NodeID) (graph: Graph<_, _>) : NodeID list =
    listNodes startNode SearchType.DFS graph
    
let bfs (startNode: NodeID) (graph: Graph<_, _>) : NodeID list =
    listNodes startNode SearchType.BFS graph
    
let shortestPath (startID:NodeID) (endID:NodeID) (graph: Graph<'NodeData, 'EdgeData>) : float * NodeID list =
    let nodeQueue = PriorityQueue<NodeID, float>()
    let previousNodes = Dictionary<NodeID, NodeID>()
    let dist = Dictionary<NodeID, float>()
    NodesIDs graph |> Seq.iter (fun id ->
                                    let initDist = if id = startID then 0.0 else infinity
                                    nodeQueue.Enqueue(id,initDist)
                                    dist[id] <- initDist)
    
    while nodeQueue.Count > 0 do
        let currentNode = nodeQueue.Dequeue()
        let currenDist = dist[currentNode]
        for connected in getConnectedNodes currentNode graph do
            let weight = 1.0 // assume constant weights, change if more general is needed
            let connectedDist = weight + currenDist
            if connectedDist < dist[connected] then
                dist[connected] <- connectedDist 
                previousNodes[connected] <- currentNode
                nodeQueue.Remove connected |> ignore
                nodeQueue.Enqueue(connected, connectedDist)
        nodeQueue.Remove currentNode |> ignore
                
    let path = ResizeArray<NodeID>()
    let mutable currentNode = endID
    while currentNode <> startID do
        path.Add currentNode
        currentNode <- previousNodes[currentNode]
    path.Add startID
    dist.[endID], (path |> List.ofSeq |> List.rev)
    