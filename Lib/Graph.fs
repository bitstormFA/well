module Lib.Graph

open System.Text
open FSharp.HashCollections.HashMap
open FSharpx.Collections
open System.Collections.Generic
open Lib.Types
open FSharp.HashCollections
open Microsoft.FSharp.Collections
open Priority_Queue
type NodeID = int
type EdgeID = int

let orderNodeIDs (id1: NodeID) (id2: NodeID) : NodeID * NodeID =
    if hash id1 < hash id2 then
        id1, id2
    else
        id2, id1

type NodeSet =
    {
        node1: NodeID
        node2: NodeID 
    }
    static member construct (node1:NodeID) (node2:NodeID) =
        let oid1, oid2 = orderNodeIDs node1 node2
        {node1=oid1; node2=oid2}
    member this.hasMember (node:NodeID) =
        node = this.node1 || node = this.node2
    member this.otherNode (node:NodeID) =
        if node = this.node1 then Some this.node2
        elif node = this.node2 then Some this.node1
        else None

[<Struct>]
type Edge<'EdgeData when 'EdgeData: equality> =
    {
      nodes: NodeSet 
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
        let inner = empty |> add n2id edgeData
        adjecencyList |> add n1id inner 

let removeEdgesContaining (nID:NodeID) (adjecencyList:GraphAdjecencyList<'EdgeData>) : GraphAdjecencyList<'EdgeData> =
    let outer = adjecencyList |> remove nID
    outer |> toSeq |> Seq.filter (fun struct(_,v) -> not (containsKey nID v)) |> Seq.map (fun struct(k,v) -> KeyValuePair(k,v)) |> ofSeq

let edgesInAdjecencyList (adjecencyList:GraphAdjecencyList<_>) =
    adjecencyList |> Seq.map (|KeyValue|)|>
    Seq.map (fun (nodeID, dict) ->
        dict |>
        Seq.map (|KeyValue|) |>
        Seq.map (fun (n2, ed) ->
            {
              nodes = NodeSet.construct nodeID n2
              edgeData = ed }))
    |> Seq.concat
    |> List.ofSeq

let numberEdgesInAdjecencyList (adjecencyList:GraphAdjecencyList<_>) : int =
    toSeq adjecencyList |>
    Seq.map (fun struct(_,v) -> count v) |> Seq.sum

let edgesWithNodeId (nID:NodeID) (adjecencyList:GraphAdjecencyList<'EdgeData>): Edge<'EdgeData> seq =
    edgesInAdjecencyList adjecencyList |> Seq.filter(fun e -> e.nodes.hasMember nID ) 
    
let connectedNodes (nID:NodeID) (adjecencyList:GraphAdjecencyList<'EdgeData>): NodeID seq =
    keys adjecencyList[nID]
    
[<Struct>]
type Graph<'NodeData, 'EdgeData when 'NodeData: equality and 'EdgeData: equality> =
    { NodeData: HashMap<NodeID, 'NodeData>
      AdjecencyList: GraphAdjecencyList<'EdgeData>
      LastId: int
      }

    static member Empty:Graph<'EdgeData, 'NodeData> =
        { NodeData = empty
          AdjecencyList = empty
          LastId = -1
        }

type MolGraph = Graph<Atom, BondType>

let numberOfNodes (graph: Graph<_, _>) : int = count graph.NodeData

let nodesIDs graph =
    keys graph.NodeData
    
let nodeData graph =
    values graph.NodeData

let nodes graph =
    graph.NodeData |> toSeq |> Seq.map (fun struct (k,v) -> {Node.id=k; Node.data=v})
     
let getNodeData nodeID graph =
    graph.NodeData.TryFind nodeID 
    
let getNodeLabel nodeID  graph =
    let nodeData = getNodeData nodeID graph
    match nodeData with
    | Some nd -> string(nd)
    | None -> ""

let edges (graph: Graph<_, 'EdgeData>) : Edge<'EdgeData> list =
    graph.AdjecencyList |> edgesInAdjecencyList

let getNodeEdges (node:NodeID) (graph:Graph<_, 'EdgeData>) : Edge<'EdgeData> list =
    edges graph |>
    List.filter (fun e -> e.nodes.hasMember node)

let getConnectedNodes (nodeID:NodeID) (graph:Graph<_,_>) : NodeID list =
    getNodeEdges nodeID graph |>
    List.map (fun e -> e.nodes.otherNode nodeID) |>
    List.choose id

let isDirectlyConnected (node1:NodeID) (node2:NodeID) (graph:Graph<_,_>) : bool =
    let sorted1, sorted2 = orderNodeIDs node1 node2
    containsKey sorted1 graph.AdjecencyList && containsKey sorted2 graph.AdjecencyList[sorted1]

let getEdgeBetween (node1:NodeID) (node2:NodeID) (graph:Graph<_,_>) : Edge<'EdgeData> option =
    let sorted1, sorted2 = orderNodeIDs node1 node2
    if containsKey sorted1 graph.AdjecencyList && containsKey sorted2 graph.AdjecencyList[sorted1] then
        Some { nodes=NodeSet.construct sorted1 sorted2; edgeData=graph.AdjecencyList[sorted1][sorted2]}
    else
        None 
    
let numberOfEdges (graph: Graph<_, _>) =
    graph.AdjecencyList |> numberEdgesInAdjecencyList
    
let addNode (nodeData:'NodeData) (graph: Graph<_, _>) : Graph<_, _> * NodeID =
    let newID = graph.LastId + 1
    {
        Graph.NodeData = add newID nodeData graph.NodeData
        Graph.AdjecencyList = graph.AdjecencyList
        LastId = newID
    }, newID
    
let addNodeFlow nodeData graph =
    let g, _ = addNode nodeData graph
    g

let removeNode (nodeId:NodeID) (graph: Graph<'NodeData, 'EdgeData>) : Graph<'NodeData, 'EdgeData> =
       {
           Graph.NodeData = remove nodeId graph.NodeData
           Graph.AdjecencyList = removeEdgesContaining nodeId graph.AdjecencyList
           Graph.LastId = graph.LastId
       }
       
let findNodeWithID (nodeId:NodeID)(graph: Graph<'NodeData, 'EdgeData>) : 'NodeData voption =
    tryFind nodeId graph.NodeData

let addNodeToNode (nodeData: 'NodeData) (connectID: NodeID) (edgeData: 'EdgeData) (graph: Graph<'NodeData, 'EdgeData>) : Graph<_,_> * (Edge<'EdgeData> * NodeID) option =
    if not (containsKey connectID graph.NodeData) then graph, None  // the node to which should be connected is unknown
    else
        let graph, newNodeID = graph |> addNode nodeData
        let n1ID, n2ID = orderNodeIDs newNodeID connectID  // make sure bonds are always in the same order so they can be identified
        let edge = {nodes=NodeSet.construct n1ID n2ID; edgeData=edgeData}
        let graph = {graph with AdjecencyList=graph.AdjecencyList |> addEdgeData n1ID n2ID edgeData}
        graph, Some (edge, newNodeID)

let addNodeToNodeFlow (nodeData: 'NodeData) (connectID: NodeID) (edgeData: 'EdgeData) (graph: Graph<'NodeData, 'EdgeData>) : Graph<_,_> =
    let g, _ = addNodeToNode nodeData connectID edgeData graph
    g
            
let addEdge (edge:Edge<'EdgeData>) (graph: Graph<'NodeData, 'EdgeData>): Graph<'NodeData, 'EdgeData> =
    if containsKey edge.nodes.node1 graph.NodeData && containsKey edge.nodes.node2 graph.NodeData then
        {graph with AdjecencyList=addEdgeData edge.nodes.node1 edge.nodes.node2 edge.edgeData graph.AdjecencyList}
    else
        graph
        
let hasEdgeBetween (n1:NodeID) (n2: NodeID) (graph: Graph<_, _>) : bool =
    isDirectlyConnected n1 n2 graph

let atoms (mol:MolGraph) =
    edges mol

let dotGraph graph : string =               
    let result = StringBuilder()
    result.Append("graph {\n") |> ignore
    graph.NodeData
    |> Map.ofDict
    |> Map.fold (fun (state:StringBuilder) k _ -> state.Append($"node_{k} [label=\"{getNodeLabel k graph}\"];\n")) result |> ignore
    edges graph
    |> Seq.fold (fun (result:StringBuilder) edge -> result.Append($"node_{edge.nodes.node1}--node_{edge.nodes.node2}[label=\"{edge.edgeData}\"];\n")) result |> ignore
    result.Append("}\n") |> ignore
    result.ToString()


type SearchType =
    | DFS
    | BFS

let listNodes (startNode: NodeID) (searchType:SearchType) (graph: Graph<_, _>) : NodeID seq =
    let dq = LinkedList<NodeID>()
    let result = Set.empty
    dq.AddFirst(startNode) |> ignore
    while dq.Count > 0 do
        let current = dq.First.Value
        dq.RemoveFirst()
        if  not (result.Contains current) then
            result.Add current |> ignore
            getConnectedNodes current graph
            |> Seq.iter (fun x -> if searchType = DFS then dq.AddFirst(x) |> ignore else dq.AddLast(x) |> ignore)
    result 
               

let dfs (startNode: NodeID) (graph: Graph<_, _>) : NodeID seq =
    listNodes startNode SearchType.DFS graph
    
let bfs (startNode: NodeID) (graph: Graph<_, _>) : NodeID seq =
    listNodes startNode SearchType.BFS graph

let shortestPathWithWeights (startID:NodeID) (endID:NodeID) (getEdgeWeight:'EdgeData -> float) (graph: Graph<'NodeData, 'EdgeData>)  : float * NodeID list =
    let nodeQueue = SimplePriorityQueue<NodeID, float>()
    let previousNodes = Dictionary<NodeID, NodeID>()
    let dist = Dictionary<NodeID, float>()
    nodesIDs graph |> Seq.iter (fun id ->
                                    let initDist = if id = startID then 0.0 else infinity
                                    nodeQueue.Enqueue(id,initDist)
                                    dist[id] <- initDist)
    
    while nodeQueue.Count > 0 do
        let currentNode = nodeQueue.Dequeue()
        let currenDist = dist[currentNode]
        for connected in getConnectedNodes currentNode graph do
            let edge = (getEdgeBetween currentNode connected graph).Value
            let weight = getEdgeWeight edge.edgeData
            let connectedDist = weight + currenDist
            if connectedDist < dist[connected] then
                dist[connected] <- connectedDist 
                previousNodes[connected] <- currentNode
                nodeQueue.UpdatePriority(connected, connectedDist)
                
    let path = ResizeArray<NodeID>()
    let mutable currentNode = endID
    while currentNode <> startID do
        path.Add currentNode
        currentNode <- previousNodes[currentNode]
    path.Add startID
    dist[endID], (path |> List.ofSeq |> List.rev)
 
let shortestPath (startID:NodeID) (endID:NodeID) (graph: Graph<'NodeData, 'EdgeData>) : float * NodeID list =
    shortestPathWithWeights startID endID (fun _ -> 1.0) graph 
    
let floydWarshallWeight (graph: Graph<'NodeData, 'EdgeData>) (getEdgeWeight:'EdgeData -> float) =
    let n = numberOfNodes graph
    let dist = Array2D.init n n (fun i j -> if i = j then 0.0 else infinity)
    let prev = Array2D.init n n (fun x y -> if x = y then x else -1)

    for e in edges graph do
        let fromID = e.nodes.node1 
        let toID = e.nodes.node2
        let weight = getEdgeWeight e.edgeData  
        dist[fromID, toID] <- weight
        dist[toID, fromID] <- weight
        prev[fromID, toID] <- fromID
        prev[toID, fromID] <- toID

    for k in 0..n-1 do
        for i in 0..n-1 do
            for j in i+1..n-1 do
                if dist[i, k] + dist[k, j] < dist[i, j] then
                    dist[i, j] <- dist[i, k] + dist[k, j]
                    dist[j, i] <- dist[i, j]
                    prev[i, j] <- prev[k, j]
                    prev[j, i] <- prev[k, i]

    dist, prev 

let reconstructFloydWarshallPath (fromID: NodeID) (toID: NodeID) (dist: float array2d) (prev: int array2d) : int list option =
    if dist[fromID, toID] = -1 then 
        None
    else
        let path = ResizeArray<NodeID>()
        path.Add(toID)
        let mutable curr = toID
        while curr <> fromID do
            curr <- prev[fromID, curr] 
            if curr = -1 then 
                curr <- toID
                path.Clear()
            else 
                path.Add(curr) 
        if path.Count >0 then
            Some (path |> Seq.toList |> List.rev )
        else
            None 
        
 
let rec floydWarshall (graph: Graph<'NodeData, 'EdgeData>) =
    floydWarshallWeight graph (fun _ -> 1.0)

type ShortestPathTree =
    {
        distances: Map<NodeID, int>
        predecessor: Map<NodeID, NodeID>
    }
    
let shortestPathTree(graph: Graph<_,_>) (startNode:NodeID) =
    let distances = Dictionary<NodeID, int>()
    let predecessor = Dictionary<NodeID, NodeID>()
    let queue = Queue<NodeID>()
    
    distances[startNode] <- 0
    queue.Enqueue(startNode)
    
    while queue.Count > 0 do
        let current = queue.Dequeue()
        for neighbor in getConnectedNodes current graph do
            if not (distances.ContainsKey neighbor) then
                distances[neighbor] <- distances[current] + 1
                predecessor[neighbor] <- current
                queue.Enqueue(neighbor)
    {distances=Map.ofDict distances; predecessor=Map.ofDict predecessor}
    
let getPathToRoot (predecessors: Map<NodeID, NodeID>) (endNode:NodeID) =
    let path = LinkedList<NodeID>()
    let mutable currentNode = Some endNode 
    while currentNode.IsSome do
        currentNode <- match predecessors.TryFind currentNode.Value with
                        | Some pred -> path.AddFirst currentNode.Value |> ignore
                                       Some pred
                        | None -> None
                
    path |> List.ofSeq
    
let spanningTree (startNode:NodeID) (graph:Graph<_,_>) =
    1