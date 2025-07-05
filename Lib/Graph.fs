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

type EdgeNodes =
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
      nodes: EdgeNodes 
      edgeData: 'EdgeData }
    
[<Struct>]
type Node<'NodeData> =
    {
        id: NodeID
        data: 'NodeData
    }
    
type GraphAdjecencyList<'EdgeData> = HashMap<NodeID, HashMap<NodeID, 'EdgeData>>

let addEdgeData<'EdgeData> (n1id:NodeID) (n2id:NodeID) (edgeData:'EdgeData) (adjecencyList:GraphAdjecencyList<'EdgeData>) : GraphAdjecencyList<'EdgeData> =
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
              nodes = EdgeNodes.construct nodeID n2
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

type MolGraph = Graph<Atom, Bond>

let numberOfNodes (graph: Graph<_, _>) : int = count graph.NodeData

let nodesIDs graph =
    keys graph.NodeData
    
let nodeData graph =
    values graph.NodeData

let nodes graph =
    graph.NodeData |> toSeq |> Seq.map (fun struct (k,v) -> {Node.id=k; Node.data=v}) |> List.ofSeq


let nodeIndex graph =
    nodes graph |> List.mapi (fun i n -> i,n)
     
let getNodeData nodeID graph =
    graph.NodeData.TryFind nodeID 
    
let getNodeLabel nodeID  graph =
    let nodeData = getNodeData nodeID graph
    match nodeData with
    | Some nd -> string(nd)
    | None -> ""

let edges (graph: Graph<_, 'EdgeData>) : Edge<'EdgeData> list =
    graph.AdjecencyList |> edgesInAdjecencyList

let edgeIndex (graph: Graph<_, 'EdgeData>) =
    edges graph |> List.mapi (fun i e -> i,e) |> Map.ofList

let nodeSets graph =
    edges graph |> List.map(fun x -> x.nodes)

let nodeSetIndex (graph: Graph<_, 'EdgeData>) =
    edges graph |> List.mapi (fun i e -> e.nodes, i ) |> Map.ofList

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
        Some { nodes=EdgeNodes.construct sorted1 sorted2; edgeData=graph.AdjecencyList[sorted1][sorted2]}
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

let removeNode (nodeId:NodeID) (graph: Graph<'NodeData, 'EdgeData>)  =
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
        let edge = {nodes=EdgeNodes.construct n1ID n2ID; edgeData=edgeData}
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

let changeNode (id:NodeID) (newData:'NodeData) (graph: Graph<'NodeData, 'EdgeData>): Graph<'NodeData, 'EdgeData> option =
    match graph.NodeData.TryFind id with
    | Some _ ->
        let newGraph = {graph with NodeData=graph.NodeData |> remove id |> add id newData}
        Some newGraph
    | None -> None  

let removeEdge (nid1:NodeID) (nid2:NodeID) (graph: Graph<'NodeData, 'EdgeData>) : Graph<'NodeData, 'EdgeData> =
    let sid1, sid2 = orderNodeIDs nid1 nid2
    if containsKey sid1 graph.AdjecencyList && containsKey sid2 graph.AdjecencyList[sid1] then
        let inner = remove sid2 graph.AdjecencyList[sid1]
        let outer = remove sid1 graph.AdjecencyList |> add sid1 inner
        {graph with AdjecencyList=outer}
    else
        graph

let changeEdge (nid1:NodeID) (nid2:NodeID) (newEdgeData:'EdgeData) (graph: Graph<'NodeData, 'EdgeData>) : Graph<'NodeData, 'EdgeData> option =
    let sid1, sid2 = orderNodeIDs nid1 nid2
    if containsKey sid1 graph.AdjecencyList && containsKey sid2 graph.AdjecencyList[sid1] then
        removeEdge sid1 sid2 graph |> addEdge {nodes=EdgeNodes.construct sid1 sid2; edgeData=newEdgeData} |> Some
    else
        None
        
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
                        | Some pred -> path.AddLast currentNode.Value |> ignore
                                       Some pred
                        | None -> path.AddLast currentNode.Value |> ignore
                                  None            
    path |> List.ofSeq
    
/// Represents a cycle as a set of edges.
type Cycle = Set<EdgeNodes>

/// Represents the basis with both the cycle edges and their vector forms.
type Basis = {
    Cycles: List<Cycle>
    Vectors: list<int array>
    }
    with
        static member empty = { Cycles=List.empty; Vectors=List.empty}

let empty = { Cycles = []; Vectors = [] }

/// Converts a cycle (set of edges) into a binary vector.
let toVector (edgeIndex: Map<EdgeNodes, int>) (edgeCount: int) (cycle: Cycle) =
    let vec = Array.zeroCreate<int> edgeCount
    for ns in cycle do
        let u,v = ns.node1, ns.node2
        let idx = edgeIndex[(EdgeNodes.construct u v)]
        vec[idx] <- 1
    vec

/// Reduces a vector by the current basis vectors using XOR.
/// Returns the reduced vector.
let private reduceVector (basisVectors: list<int array>) (vec: int array) =
    let mutable tempVec = Array.copy vec
    for basisVec in basisVectors do
        // Ensure tempVec is reduced by basisVec
        // Find pivot (first '1') in basisVec
        match Array.tryFindIndex ((=) 1) basisVec with
        | Some pivotIndex when tempVec[pivotIndex] = 1 ->
            // XOR the vectors
            for i in 0 .. tempVec.Length - 1 do
                tempVec[i] <- (tempVec[i] + basisVec[i]) % 2
        | _ -> ()
    tempVec

/// Tries to add a cycle to the basis.
/// The cycle is added only if it's linearly independent.
let tryAdd (graph: Graph<_,_>) (basis: Basis) (cycle: Cycle) =
    let cycleVec = toVector (nodeSetIndex graph) (numberOfNodes graph) cycle
    let reducedVec = reduceVector basis.Vectors cycleVec

    if Array.exists ((=) 1) reducedVec then
        // The cycle is linearly independent. Add it to the basis.
        // To maintain the basis in a "reduced row echelon form", we pivot and reduce other vectors.
        let pivotIndex = Array.findIndex ((=) 1) reducedVec
        let mutable newBasisVectors = []
        for bv in basis.Vectors do
            if bv[pivotIndex] = 1 then
                let reducedBv = Array.init bv.Length (fun i -> (bv[i] + reducedVec[i]) % 2)
                newBasisVectors <- reducedBv :: newBasisVectors
            else
                newBasisVectors <- bv :: newBasisVectors

        Some {
            Cycles = cycle :: basis.Cycles
            Vectors = reducedVec :: newBasisVectors
        }
    else
        // The cycle is linearly dependent on the current basis.
        None


/// Finds the Minimum Cycle Basis of an unweighted graph using the Mehlhorn-Michail algorithm.
/// Based on Mehlhorn, Kurt, and Dimitrios Michail. “Minimum Cycle Bases: Faster and Simpler.”
/// ACM Transactions on Algorithms 6, no. 1 (December 2009): 1–13.
/// https://doi.org/10.1145/1644015.1644023.
let findMinimumCycleBasis (graph: Graph<_,_>) =
    let m = numberOfEdges graph
    let n = numberOfNodes graph
    let c = 1 // Assuming a connected graph for simplicity
    let basisSize = m - n + c

    let mutable basis = Basis.empty
    let mutable candidateCycles = []

    // 1. Generate all candidate cycles from all SPTs
    for v in nodes graph do
        let spt = shortestPathTree graph v.id
        let predecessors = spt.predecessor
        let sptEdges =
            predecessors
            |> Map.toList
            |> List.map (fun (child, parent) -> EdgeNodes.construct child parent)
            |> Set.ofList

        let nonTreeEdges = Set.difference (nodeSets graph |> Set.ofList) sptEdges

        for ns in nonTreeEdges do
            let u,w = ns.node1, ns.node2

            let path_u = getPathToRoot predecessors u
            let path_w = getPathToRoot predecessors w

            let path_u_set = Set.ofList path_u
            let meetNode = List.find (fun node -> path_u_set.Contains(node)) path_w
            let cyclePath =
                (List.takeWhile ((<>) meetNode) path_u)
                @ [meetNode]
                @ (List.rev (List.takeWhile ((<>) meetNode) path_w))
                

            let cycleEdges =
                List.zip cyclePath (List.tail cyclePath @ [List.head cyclePath])
                |> List.map (fun (n1, n2) -> (min n1 n2, max n1 n2))
                |> Set.ofList

            let cycleLength = cycleEdges.Count
            candidateCycles <- (cycleLength, cycleEdges) :: candidateCycles

    let sortedCandidates =
        candidateCycles
        |> List.distinctBy snd // Remove duplicate cycles
        |> List.sortBy fst |> List.map (fun (i,s) -> i, (s |> Set.map(fun n -> EdgeNodes.construct (fst n) (snd n))) )
        

    for _, cycle in sortedCandidates do
        if basis.Cycles.Length < basisSize then
            match tryAdd graph basis cycle with
            | Some newBasis -> basis <- newBasis
            | None -> ()

    basis.Cycles 
