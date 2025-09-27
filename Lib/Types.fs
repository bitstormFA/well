module Lib.Types
open System.Collections
open System.Text
open FSharp.HashCollections
open FSharpx.Collections
open System.Collections.Generic
open Microsoft.FSharp.Collections
open Priority_Queue
open Microsoft.FSharp.Reflection

type NodeID = int
type EdgeID = int
type Smiles = string

type Element =
    |H|He|Li|Be|B|C|N|O|F|Ne
    |Na|Mg|Al|Si|P|S|Cl|Ar
    |K|Ca|Sc|Ti|V|Cr|Mn|Fe
    |Co|Ni|Cu|Zn|Ga|Ge|As|Se|Br|Kr|Rb|Sr|Y|Zr|Nb|Mo|Tc|Ru|Rh|Pd|Ag|Cd|In
    |Sn|Sb|Te|I|Xe|Cs|Ba|La|Ce|Pr|Nd|Pm|Sm|Eu|Gd|Tb|Dy|Ho|Er|Tm|Yb|Lu|Hf
    |Ta|W|Re|Os|Ir|Pt|Au|Hg|Tl|Pb|Bi|Po|At|Rn|Fr|Ra|Ac|Th|Pa|U|Np|Pu|Am
    |Cm|Bk|Cf|Es|Fm|Md|No|Lr|Rf|Db|Sg|Bh|Hs|Mt|Ds|Rg|Cn|UNK

type BondType =
    | Single
    | Double
    | Triple
    | Quadruple
    | Aromatic
    | Ionic
    | Dative
    | DativeSingle
    | Unknown

type BondDirection =
    | NoDirection
    | Up
    | Down
    | CisTrans1
    | CisTrans2
    | Unknown

let bondTypeValenceContribution (bt:BondType) =
    match bt with
    | Single -> 1.0
    | Double -> 2.0
    | Triple -> 3.0
    | Quadruple -> 4.0
    | Aromatic -> 1.5
    | Dative -> 1.0
    | DativeSingle -> 1.0
    | BondType.Unknown -> 1.0
    | Ionic -> 0.0

type Chirality =
    | Clockwise
    | CounterClockwise

type Bond =
    {
        Type:BondType
        Conjugated: bool
        Direction: BondDirection
    }
    with
    static member fromBondType bt = {Type=bt; Conjugated=false; Direction=NoDirection}

type Hybridization =
    | S
    | SP
    | SP2
    | SP3
    | SP2D
    | SP3D
    | OTHER

type Atom =
    {
        Element: Element
        Isotope: int option
        Chirality: Chirality option
        Hydrogens: int
        ImplicitHydrogens: int
        FormalCharge: int
        IsAromatic: bool
        AtomClass: int option
        Hybridization: Hybridization option
    }
    static member Default =
        {
            Element = Element.C
            Isotope = None
            Chirality = None
            Hydrogens = 0
            ImplicitHydrogens = 0
            FormalCharge = 0
            IsAromatic = false
            AtomClass = None
            Hybridization = None
        }
    override this.ToString() =
        let result = StringBuilder()
        if this.Isotope.IsSome then result.Append(this.Isotope.Value) |> ignore
        if this.Chirality.IsSome then result.Append(this.Chirality) |> ignore
        let symbol = if this.IsAromatic then
                         this.Element.ToString().ToLower()
                     else
                         this.Element.ToString()
        result.Append(symbol) |> ignore
        if this.Hydrogens > 0 then result.Append(this.Hydrogens) |> ignore
        if this.FormalCharge <> 0 then result.Append(this.FormalCharge) |> ignore
        result.ToString()

let orderNodeIDs (id1: NodeID) (id2: NodeID) : NodeID * NodeID =
    if hash id1 < hash id2 then
        id1, id2
    else
        id2, id1

/// Represents an undirected edge between two nodes
type NodeIDSet =
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
      nodes: NodeIDSet 
      edgeData: 'EdgeData }
    
[<Struct>]
type Node<'NodeData> =
    {
        id: NodeID
        data: 'NodeData
    }
    
type GraphAdjecencyList<'EdgeData> = HashMap<NodeID, HashMap<NodeID, 'EdgeData>>

/// Adds an edge to the adjacency list (helper function)
let addEdgeData<'EdgeData> (n1id:NodeID) (n2id:NodeID) (edgeData:'EdgeData) (adjecencyList:GraphAdjecencyList<'EdgeData>) : GraphAdjecencyList<'EdgeData> =
    if HashMap.containsKey n1id adjecencyList then
        let inner = adjecencyList[n1id] |> HashMap.remove n2id |> HashMap.add n2id edgeData
        adjecencyList |> HashMap.remove n1id |> HashMap.add n1id inner 
    else
        let inner = HashMap.empty |> HashMap.add n2id edgeData
        adjecencyList |> HashMap.add n1id inner 

/// Removes all edges connected to a specific node
let removeEdgesContaining (nID:NodeID) (adjecencyList:GraphAdjecencyList<'EdgeData>) : GraphAdjecencyList<'EdgeData> =
    let outer = adjecencyList |> HashMap.remove nID
    let mutable result = HashMap.empty
    for struct(k,v) in outer |> HashMap.toSeq do
        if not (HashMap.containsKey nID v) then
            result <- result |> HashMap.add k (HashMap.remove nID v)
    result

/// Converts adjacency list to a sequence of all edges
let edgesInAdjecencyList (adjecencyList:GraphAdjecencyList<_>) =
    adjecencyList
    |> HashMap.toSeq
    |> Seq.collect (fun struct(nodeID, dict) ->
        dict
        |> HashMap.toSeq
        |> Seq.map (fun struct(n2, ed) ->
            {
              nodes = NodeIDSet.construct nodeID n2
              edgeData = ed }))
    |> List.ofSeq

/// Counts total edges in adjacency list
let numberEdgesInAdjecencyList (adjecencyList:GraphAdjecencyList<_>) : int =
    HashMap.toSeq adjecencyList
    |> Seq.sumBy (fun struct(_,v) -> HashMap.count v)

/// Gets all edges connected to a specific node ID
let edgesWithNodeId (nID:NodeID) (adjecencyList:GraphAdjecencyList<'EdgeData>): Edge<'EdgeData> seq =
    edgesInAdjecencyList adjecencyList |> Seq.filter(fun e -> e.nodes.hasMember nID ) 
    
/// Gets connected nodes for a specific node ID
let connectedNodes (nID:NodeID) (adjecencyList:GraphAdjecencyList<'EdgeData>): NodeID seq =
    HashMap.keys adjecencyList[nID]

type GraphProperties =
    | Comment of string list
    
[<Struct>]
type Graph<'NodeData, 'EdgeData when 'NodeData: equality and 'EdgeData: equality> =
    { 
        NodeData: HashMap<NodeID, 'NodeData>
        AdjecencyList: GraphAdjecencyList<'EdgeData>
        LastId: int
        Properties: HashMap<string,GraphProperties>
      }

    static member Empty:Graph<'EdgeData, 'NodeData> =
        { 
            NodeData = HashMap.empty
            AdjecencyList = HashMap.empty
            LastId = -1
            Properties = HashMap.empty
        }

type MolGraph = Graph<Atom, Bond>

/// Adds a property to the graph
let addProperty name property graph =
    {graph with Properties=HashMap.add name property graph.Properties}

/// Removes a property from the graph
let removeProperty propertyName graph =
    {graph with Properties=HashMap.remove propertyName graph.Properties}
    
/// Gets a property from the graph
let getProperty propertyName graph : GraphProperties option =
    HashMap.tryFind propertyName graph.Properties |> ValueOption.toOption
    
/// Checks if a property exists in the graph
let hasProperty propertyName graph =
    HashMap.containsKey propertyName graph.Properties

/// Gets number of nodes in the graph
let numberOfNodes (graph: Graph<_, _>) : int = HashMap.count graph.NodeData

/// Gets all node IDs in the graph
let nodesIDs graph =
    HashMap.keys graph.NodeData
    
/// Gets all node data values in the graph
let nodeData graph =
    HashMap.values graph.NodeData

/// Gets all nodes in the graph
let nodes graph =
    graph.NodeData
    |> HashMap.toSeq
    |> Seq.map (fun struct(k,v) -> {Node.id=k; Node.data=v})
    |> List.ofSeq

/// Gets all nodes with their indices
let nodeIndex graph =
    nodes graph |> List.mapi (fun i n -> i,n)
     
/// Tries to get node data for a specific ID
let tryGetNodeData (nodeID:NodeID) graph =
    HashMap.tryFind nodeID graph.NodeData |> ValueOption.toOption
    
/// Gets node data for a specific ID (throws if not found)
let getNodeData nodeID graph =
    match tryGetNodeData nodeID graph with
    | Some data -> data
    | None -> failwith $"Node {nodeID} not found"

/// Gets label for a node
let getNodeLabel nodeID  graph =
    let nodeData = tryGetNodeData nodeID graph
    match nodeData with
    | Some nd -> string(nd)
    | None -> ""

/// Gets all edges in the graph
let edges (graph: Graph<_, 'EdgeData>) : Edge<'EdgeData> list =
    graph.AdjecencyList |> edgesInAdjecencyList

/// Gets edge index mapping
let edgeIndex (graph: Graph<_, 'EdgeData>) =
    edges graph |> List.mapi (fun i e -> i,e) |> Map.ofList

/// Gets node sets from edges
let nodeSets graph =
    edges graph |> List.map(fun x -> x.nodes)

/// Gets node set index mapping
let nodeSetIndex (graph: Graph<_, 'EdgeData>) =
    edges graph |> List.mapi (fun i e -> e.nodes, i ) |> Map.ofList

/// Gets all edges connected to a specific node
let nodeEdges (node:NodeID) (graph:Graph<_, 'EdgeData>) : Edge<'EdgeData> list =
    edges graph |>
    List.filter (fun e -> e.nodes.hasMember node)

/// Gets connected nodes for a specific node ID
let getConnectedNodes (nodeID:NodeID) (graph:Graph<_,_>) : NodeID list =
    nodeEdges nodeID graph |>
    List.map (fun e -> e.nodes.otherNode nodeID) |>
    List.choose id

/// Checks if two nodes are directly connected
let isDirectlyConnected (node1:NodeID) (node2:NodeID) (graph:Graph<_,_>) : bool =
    let sorted1, sorted2 = orderNodeIDs node1 node2
    HashMap.containsKey sorted1 graph.AdjecencyList && 
    HashMap.containsKey sorted2 graph.AdjecencyList[sorted1]

/// Gets edge between two nodes if it exists
let getEdgeBetween (node1:NodeID) (node2:NodeID) (graph:Graph<_,_>) : Edge<'EdgeData> option =
    let sorted1, sorted2 = orderNodeIDs node1 node2
    if HashMap.containsKey sorted1 graph.AdjecencyList && 
       HashMap.containsKey sorted2 graph.AdjecencyList[sorted1] then
        Some { nodes=NodeIDSet.construct sorted1 sorted2; edgeData=graph.AdjecencyList[sorted1][sorted2]}
    else
        None 
    
/// Gets number of edges in the graph
let numberOfEdges (graph: Graph<_, _>) =
    graph.AdjecencyList |> numberEdgesInAdjecencyList
    
/// Adds a node to the graph
let addNode (nodeData:'NodeData) (graph: Graph<_, _>) : Graph<_, _> * NodeID =
    let newID = graph.LastId + 1
    {
        Graph.NodeData = HashMap.add newID nodeData graph.NodeData
        Graph.AdjecencyList = graph.AdjecencyList
        LastId = newID
        Properties = graph.Properties
    }, newID
    
/// Adds a node to the graph (flow version)
let addNodeFlow nodeData graph =
    let g, _ = addNode nodeData graph
    g

/// Removes a node from the graph
let removeNode (nodeId:NodeID) (graph: Graph<'NodeData, 'EdgeData>)  =
       {
           Graph.NodeData = HashMap.remove nodeId graph.NodeData
           Graph.AdjecencyList = removeEdgesContaining nodeId graph.AdjecencyList
           LastId = graph.LastId
           Properties = graph.Properties
       }
       
/// Finds a node by ID
let findNodeWithID (nodeId:NodeID)(graph: Graph<'NodeData, 'EdgeData>) : 'NodeData option =
    HashMap.tryFind nodeId graph.NodeData |> ValueOption.toOption

/// Adds a new node connected to an existing node
let addNodeToNode (nodeData: 'NodeData) (connectID: NodeID) (edgeData: 'EdgeData) (graph: Graph<'NodeData, 'EdgeData>) : Graph<_,_> * (Edge<'EdgeData> * NodeID) option =
    if not (HashMap.containsKey connectID graph.NodeData) then 
        graph, None  // the node to which should be connected is unknown
    else
        let graph, newNodeID = graph |> addNode nodeData
        let n1ID, n2ID = orderNodeIDs newNodeID connectID  // make sure bonds are always in the same order so they can be identified
        let edge = {nodes=NodeIDSet.construct n1ID n2ID; edgeData=edgeData}
        let graph = {graph with AdjecencyList=graph.AdjecencyList |> addEdgeData n1ID n2ID edgeData}
        graph, Some (edge, newNodeID)

/// Adds a new node connected to an existing node (flow version)
let addNodeToNodeFlow (nodeData: 'NodeData) (connectID: NodeID) (edgeData: 'EdgeData) (graph: Graph<'NodeData, 'EdgeData>) : Graph<_,_> =
    let g, _ = addNodeToNode nodeData connectID edgeData graph
    g

/// Tries to add an edge to the graph
let tryAddEdge (edge:Edge<'EdgeData>) (graph: Graph<'NodeData, 'EdgeData>): Graph<'NodeData, 'EdgeData> option =
    if HashMap.containsKey edge.nodes.node1 graph.NodeData && 
       HashMap.containsKey edge.nodes.node2 graph.NodeData then
        Some {graph with AdjecencyList=addEdgeData edge.nodes.node1 edge.nodes.node2 edge.edgeData graph.AdjecencyList}
    else
        None

/// Adds an edge to the graph (fails if nodes don't exist)
let addEdge (edge:Edge<'EdgeData>) (graph: Graph<'NodeData, 'EdgeData>): Graph<'NodeData, 'EdgeData> =
    match tryAddEdge edge graph with
    | Some newGraph -> newGraph
    | None -> graph

/// Changes node data
let changeNode (id:NodeID) (newData:'NodeData) (graph: Graph<'NodeData, 'EdgeData>): Graph<'NodeData, 'EdgeData> option =
    match HashMap.tryFind id graph.NodeData |> ValueOption.toOption with
    | Some _ ->
        let newGraph = {graph with NodeData=graph.NodeData |> HashMap.remove id |> HashMap.add id newData}
        Some newGraph
    | None -> None  

/// Removes an edge between two nodes
let removeEdge (nid1:NodeID) (nid2:NodeID) (graph: Graph<'NodeData, 'EdgeData>) : Graph<'NodeData, 'EdgeData> =
    let sid1, sid2 = orderNodeIDs nid1 nid2
    if HashMap.containsKey sid1 graph.AdjecencyList && 
       HashMap.containsKey sid2 graph.AdjecencyList[sid1] then
        let inner = HashMap.remove sid2 graph.AdjecencyList[sid1]
        let outer = HashMap.remove sid1 graph.AdjecencyList |> HashMap.add sid1 inner
        {graph with AdjecencyList=outer}
    else
        graph

/// Changes edge data between two nodes
let changeEdge (nid1:NodeID) (nid2:NodeID) (newEdgeData:'EdgeData) (graph: Graph<'NodeData, 'EdgeData>) : Graph<'NodeData, 'EdgeData> option =
    let sid1, sid2 = orderNodeIDs nid1 nid2
    if HashMap.containsKey sid1 graph.AdjecencyList && 
       HashMap.containsKey sid2 graph.AdjecencyList[sid1] then
        removeEdge sid1 sid2 graph |> addEdge {nodes=NodeIDSet.construct sid1 sid2; edgeData=newEdgeData} |> Some
    else
        None
        
/// Checks if there's an edge between two nodes
let hasEdgeBetween (n1:NodeID) (n2: NodeID) (graph: Graph<_, _>) : bool =
    isDirectlyConnected n1 n2 graph

/// Gets atoms from a molecular graph
let atoms (mol:MolGraph) =
    edges mol

/// Converts graph to DOT format string
let dotGraph graph : string =               
    let result = StringBuilder()
    result.Append("graph {\n") |> ignore
    graph.NodeData
    |> Map.ofDict
    |> Map.fold (fun (state:StringBuilder) k _ -> 
        state.Append($"node_{k} [label=\"{getNodeLabel k graph}\"];\n")) result |> ignore
    edges graph
    |> Seq.fold (fun (result:StringBuilder) edge -> 
        result.Append($"node_{edge.nodes.node1}--node_{edge.nodes.node2}[label=\"{edge.edgeData}\"];\n")) result |> ignore
    result.Append("}\n") |> ignore
    result.ToString()


type SearchType =
    | DFS
    | BFS

/// Gets nodes using specified search type (DFS or BFS)
let getNodes (startNode: NodeID) (searchType:SearchType) (graph: Graph<_, _>) : NodeID seq =
    let dq = LinkedList<NodeID>()
    let result = Set.empty
    dq.AddFirst(startNode) |> ignore
    while dq.Count > 0 do
        let current = dq.First.Value
        dq.RemoveFirst()
        if not (result.Contains current) then
            result.Add current |> ignore
            getConnectedNodes current graph
            |> Seq.iter (fun x -> 
                if searchType = DFS then 
                    dq.AddFirst(x) |> ignore 
                else 
                    dq.AddLast(x) |> ignore)
    result 
               

/// Performs depth-first search
let dfs (startNode: NodeID) (graph: Graph<_, _>) : NodeID seq =
    getNodes startNode SearchType.DFS graph
    
/// Performs breadth-first search
let bfs (startNode: NodeID) (graph: Graph<_, _>) : NodeID seq =
    getNodes startNode SearchType.BFS graph

/// Finds shortest path with weights using Dijkstra's algorithm
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
 
/// Finds shortest path without weights
let shortestPath (startID:NodeID) (endID:NodeID) (graph: Graph<'NodeData, 'EdgeData>) : float * NodeID list =
    shortestPathWithWeights startID endID (fun _ -> 1.0) graph 
    
/// Implements Floyd-Warshall algorithm for all-pairs shortest paths
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

let elementNames = FSharpType.GetUnionCases(typeof<Element>)
                   |> Array.map (fun case -> string(case.Name))

let elementMap:Map<string, Element> = FSharpType.GetUnionCases(typeof<Element>) |>
                                      Array.map (fun caseInfo ->
                                         let keywordString = caseInfo.Name
                                         let useName = if keywordString = "UNK" then
                                                           "*"
                                                       else
                                                           keywordString
                                         let duCase = FSharpValue.MakeUnion(caseInfo, [||]) :?> Element
                                         (useName, duCase)) |> Map.ofArray

let twoLetterElementNames = elementNames |> Array.filter (fun x -> x.Length = 2)

type ElementInfo = {
    Number: int
    Symbol: string
    Name: string
    Mass: float
    Radius: float option
    DisplayColor: string
    ElectronConfiguration: string
    Electronegativity: float option
    ElectronAffinity: float option
    MinOxidation: int
    MaxOxidation: int
    Valences: int list
}

let capitalizeFirstChar (s: string) =
    if System.String.IsNullOrEmpty(s) then
        s // Or "" depending on desired behavior for null/empty
    else
        let firstChar = System.Char.ToUpper(s[0])
        let restOfString = s[1..]
        string firstChar + restOfString

let tryStringToElement (elementSymbol: string) : Element option =
    let firstUpperName =capitalizeFirstChar elementSymbol
    elementMap.TryFind(firstUpperName)

let stringToElement elementSymbol =
    match tryStringToElement elementSymbol with
    | Some symbol -> symbol
    | None -> Element.UNK

let allElementCasesAsStringList () : list<string> =
    try
        let unionType = typeof<Element>
        FSharpType.GetUnionCases(unionType)
        |> Array.map (fun caseInfo -> caseInfo.Name)
        |> List.ofArray
    with
    | ex ->
        eprintfn $"Error getting element case names: %s{ex.Message}"
        [] // Return an empty list on error

let bondMap= Map [
    "-", BondType.Single
    "=", BondType.Double
    "#", BondType.Triple
    "$", BondType.Quadruple
    ":", BondType.Aromatic
    "/", BondType.Single
    @"\", BondType.Single
]

let elementInfo = Map [
    Element.H,{Number = 1; Symbol = "H"; Name = "Hydrogen"; Mass = 1.00794; DisplayColor = "#ffffff"; ElectronConfiguration = "1s1"; Electronegativity = Some 2.2; ElectronAffinity = Some -73; Radius = Some 37; MinOxidation = -1; MaxOxidation = 1; Valences = [1; -1]}
    Element.He,{Number = 2; Symbol = "He"; Name = "Helium"; Mass = 4.002602; DisplayColor = "#d9ffff"; ElectronConfiguration = "1s2"; Electronegativity = None; ElectronAffinity = Some 0; Radius = Some 32; MinOxidation = 0; MaxOxidation = 0; Valences = [0]}
    Element.Li,{Number = 3; Symbol = "Li"; Name = "Lithium"; Mass = 6.941; DisplayColor = "#cc80ff"; ElectronConfiguration = "[He] 2s1"; Electronegativity = Some 0.98; ElectronAffinity = Some -60; Radius = Some 134; MinOxidation = 0; MaxOxidation = 1; Valences = [1]}
    Element.Be,{Number = 4; Symbol = "Be"; Name = "Beryllium"; Mass = 9.012182; DisplayColor = "#c2ff00"; ElectronConfiguration = "[He] 2s2"; Electronegativity = Some 1.57; ElectronAffinity = Some 0; Radius = Some 90; MinOxidation = 0; MaxOxidation = 2; Valences = [2]}
    Element.B,{Number = 5; Symbol = "B"; Name = "Boron"; Mass = 10.811; DisplayColor = "#ffb5b5"; ElectronConfiguration = "[He] 2s2 2p1"; Electronegativity = Some 2.04; ElectronAffinity = Some -27; Radius = Some 82; MinOxidation = 0; MaxOxidation = 3; Valences = [3]}
    Element.C,{Number = 6; Symbol = "C"; Name = "Carbon"; Mass = 12.0107; DisplayColor = "#909090"; ElectronConfiguration = "[He] 2s2 2p2"; Electronegativity = Some 2.55; ElectronAffinity = Some -154; Radius = Some 77; MinOxidation = -4; MaxOxidation = 4; Valences = [4; 2; -4]}
    Element.N,{Number = 7; Symbol = "N"; Name = "Nitrogen"; Mass = 14.0067; DisplayColor = "#3050f8"; ElectronConfiguration = "[He] 2s2 2p3"; Electronegativity = Some 3.04; ElectronAffinity = Some -7; Radius = Some 75; MinOxidation = -3; MaxOxidation = 5; Valences = [5; 4; 3; 2; 1; -3]}
    Element.O,{Number = 8; Symbol = "O"; Name = "Oxygen"; Mass = 15.9994; DisplayColor = "#ff0d0d"; ElectronConfiguration = "[He] 2s2 2p4"; Electronegativity = Some 3.44; ElectronAffinity = Some -141; Radius = Some 73; MinOxidation = -2; MaxOxidation = 2; Valences = [-2; -1; 2]}
    Element.F,{Number = 9; Symbol = "F"; Name = "Fluorine"; Mass = 18.9984032; DisplayColor = "#90e050"; ElectronConfiguration = "[He] 2s2 2p5"; Electronegativity = Some 3.98; ElectronAffinity = Some -328; Radius = Some 71; MinOxidation = -1; MaxOxidation = 0; Valences = [-1]}
    Element.Ne,{Number = 10; Symbol = "Ne"; Name = "Neon"; Mass = 20.1797; DisplayColor = "#b3e3f5"; ElectronConfiguration = "[He] 2s2 2p6"; Electronegativity = None; ElectronAffinity = Some 0; Radius = Some 69; MinOxidation = 0; MaxOxidation = 0; Valences = [0]}
    Element.Na,{Number = 11; Symbol = "Na"; Name = "Sodium"; Mass = 22.98976928; DisplayColor = "#ab5cf2"; ElectronConfiguration = "[Ne] 3s1"; Electronegativity = Some 0.93; ElectronAffinity = Some -53; Radius = Some 154; MinOxidation = -1; MaxOxidation = 1; Valences = [1]}
    Element.Mg,{Number = 12; Symbol = "Mg"; Name = "Magnesium"; Mass = 24.305; DisplayColor = "#8aff00"; ElectronConfiguration = "[Ne] 3s2"; Electronegativity = Some 1.31; ElectronAffinity = Some 0; Radius = Some 130; MinOxidation = 0; MaxOxidation = 2; Valences = [2]}
    Element.Al,{Number = 13; Symbol = "Al"; Name = "Aluminum or Aluminium"; Mass = 26.9815386; DisplayColor = "#bfa6a6"; ElectronConfiguration = "[Ne] 3s2 3p1"; Electronegativity = Some 1.61; ElectronAffinity = Some -43; Radius = Some 118; MinOxidation = 0; MaxOxidation = 3; Valences = [3]}
    Element.Si,{Number = 14; Symbol = "Si"; Name = "Silicon"; Mass = 28.0855; DisplayColor = "#f0c8a0"; ElectronConfiguration = "[Ne] 3s2 3p2"; Electronegativity = Some 1.9; ElectronAffinity = Some -134; Radius = Some 111; MinOxidation = -4; MaxOxidation = 4; Valences = [4; 2; -4]}
    Element.P,{Number = 15; Symbol = "P"; Name = "Phosphorus"; Mass = 30.973762; DisplayColor = "#ff8000"; ElectronConfiguration = "[Ne] 3s2 3p3"; Electronegativity = Some 2.19; ElectronAffinity = Some -72; Radius = Some 106; MinOxidation = -3; MaxOxidation = 5; Valences = [5; 3; -3]}
    Element.S,{Number = 16; Symbol = "S"; Name = "Sulfur"; Mass = 32.065; DisplayColor = "#ffff30"; ElectronConfiguration = "[Ne] 3s2 3p4"; Electronegativity = Some 2.58; ElectronAffinity = Some -200; Radius = Some 102; MinOxidation = -2; MaxOxidation = 6; Valences = [6; 4; 2; -2]}
    Element.Cl,{Number = 17; Symbol = "Cl"; Name = "Chlorine"; Mass = 35.453; DisplayColor = "#1ff01f"; ElectronConfiguration = "[Ne] 3s2 3p5"; Electronegativity = Some 3.16; ElectronAffinity = Some -349; Radius = Some 99; MinOxidation = -1; MaxOxidation = 7; Valences = [7; 5; 3; 1; -1]}
    Element.Ar,{Number = 18; Symbol = "Ar"; Name = "Argon"; Mass = 39.948; DisplayColor = "#80d1e3"; ElectronConfiguration = "[Ne] 3s2 3p6"; Electronegativity = None; ElectronAffinity = Some 0; Radius = Some 97; MinOxidation = 0; MaxOxidation = 0; Valences = [0]}
    Element.K,{Number = 19; Symbol = "K"; Name = "Potassium"; Mass = 39.0983; DisplayColor = "#8f40d4"; ElectronConfiguration = "[Ar] 4s1"; Electronegativity = Some 0.82; ElectronAffinity = Some -48; Radius = Some 196; MinOxidation = 0; MaxOxidation = 1; Valences = [1]}
    Element.Ca,{Number = 20; Symbol = "Ca"; Name = "Calcium"; Mass = 40.078; DisplayColor = "#3dff00"; ElectronConfiguration = "[Ar] 4s2"; Electronegativity = Some 1; ElectronAffinity = Some -2; Radius = Some 174; MinOxidation = 0; MaxOxidation = 2; Valences = [2]}
    Element.Sc,{Number = 21; Symbol = "Sc"; Name = "Scandium"; Mass = 44.955912; DisplayColor = "#e6e6e6"; ElectronConfiguration = "[Ar] 3d1 4s2"; Electronegativity = Some 1.36; ElectronAffinity = Some -18; Radius = Some 144; MinOxidation = 0; MaxOxidation = 3; Valences = [3]}
    Element.Ti,{Number = 22; Symbol = "Ti"; Name = "Titanium"; Mass = 47.867; DisplayColor = "#bfc2c7"; ElectronConfiguration = "[Ar] 3d2 4s2"; Electronegativity = Some 1.54; ElectronAffinity = Some -8; Radius = Some 136; MinOxidation = -1; MaxOxidation = 4; Valences = [4; 3; 2]}
    Element.V,{Number = 23; Symbol = "V"; Name = "Vanadium"; Mass = 50.9415; DisplayColor = "#a6a6ab"; ElectronConfiguration = "[Ar] 3d3 4s2"; Electronegativity = Some 1.63; ElectronAffinity = Some -51; Radius = Some 125; MinOxidation = -1; MaxOxidation = 4; Valences = [4; 3; 2]}
    Element.Cr,{Number = 24; Symbol = "Cr"; Name = "Chromium"; Mass = 51.9961; DisplayColor = "#8a99c7"; ElectronConfiguration = "[Ar] 3d5 4s1"; Electronegativity = Some 1.66; ElectronAffinity = Some -64; Radius = Some 127; MinOxidation = -2; MaxOxidation = 6; Valences = [6; 3; 2]}
    Element.Mn,{Number = 25; Symbol = "Mn"; Name = "Manganese"; Mass = 54.938045; DisplayColor = "#9c7ac7"; ElectronConfiguration = "[Ar] 3d5 4s2"; Electronegativity = Some 1.55; ElectronAffinity = Some 0; Radius = Some 139; MinOxidation = -3; MaxOxidation = 7; Valences = [7; 6; 4; 3; 2]}
    Element.Fe,{Number = 26; Symbol = "Fe"; Name = "Iron"; Mass = 55.845; DisplayColor = "#e06633"; ElectronConfiguration = "[Ar] 3d6 4s2"; Electronegativity = Some 1.83; ElectronAffinity = Some -16; Radius = Some 125; MinOxidation = -2; MaxOxidation = 6; Valences = [6; 3; 2]}
    Element.Co,{Number = 27; Symbol = "Co"; Name = "Cobalt"; Mass = 58.933195; DisplayColor = "#f090a0"; ElectronConfiguration = "[Ar] 3d7 4s2"; Electronegativity = Some 1.88; ElectronAffinity = Some -64; Radius = Some 126; MinOxidation = -1; MaxOxidation = 5; Valences = [5; 4; 3; 2]}
    Element.Ni,{Number = 28; Symbol = "Ni"; Name = "Nickel"; Mass = 58.6934; DisplayColor = "#50d050"; ElectronConfiguration = "[Ar] 3d8 4s2"; Electronegativity = Some 1.91; ElectronAffinity = Some -112; Radius = Some 121; MinOxidation = -1; MaxOxidation = 4; Valences = [4; 3; 2; 0]}
    Element.Cu,{Number = 29; Symbol = "Cu"; Name = "Copper"; Mass = 63.546; DisplayColor = "#c88033"; ElectronConfiguration = "[Ar] 3d10 4s1"; Electronegativity = Some 1.9; ElectronAffinity = Some -118; Radius = Some 138; MinOxidation = 0; MaxOxidation = 4; Valences = [4; 2; 1]}
    Element.Zn,{Number = 30; Symbol = "Zn"; Name = "Zinc"; Mass = 65.38; DisplayColor = "#7d80b0"; ElectronConfiguration = "[Ar] 3d10 4s2"; Electronegativity = Some 1.65; ElectronAffinity = Some 0; Radius = Some 131; MinOxidation = 0; MaxOxidation = 2; Valences = [2]}
    Element.Ga,{Number = 31; Symbol = "Ga"; Name = "Gallium"; Mass = 69.723; DisplayColor = "#c28f8f"; ElectronConfiguration = "[Ar] 3d10 4s2 4p1"; Electronegativity = Some 1.81; ElectronAffinity = Some -29; Radius = Some 126; MinOxidation = 0; MaxOxidation = 3; Valences = [3; 1]}
    Element.Ge,{Number = 32; Symbol = "Ge"; Name = "Germanium"; Mass = 72.64; DisplayColor = "#668f8f"; ElectronConfiguration = "[Ar] 3d10 4s2 4p2"; Electronegativity = Some 2.01; ElectronAffinity = Some -119; Radius = Some 122; MinOxidation = -4; MaxOxidation = 4; Valences = [4; 2]}
    Element.As,{Number = 33; Symbol = "As"; Name = "Arsenic"; Mass = 74.9216; DisplayColor = "#bd80e3"; ElectronConfiguration = "[Ar] 3d10 4s2 4p3"; Electronegativity = Some 2.18; ElectronAffinity = Some -78; Radius = Some 119; MinOxidation = -3; MaxOxidation = 5; Valences = [5; 3; -3]}
    Element.Se,{Number = 34; Symbol = "Se"; Name = "Selenium"; Mass = 78.96; DisplayColor = "#ffa100"; ElectronConfiguration = "[Ar] 3d10 4s2 4p4"; Electronegativity = Some 2.55; ElectronAffinity = Some -195; Radius = Some 116; MinOxidation = -2; MaxOxidation = 6; Valences = [6; 4; 2; -2]}
    Element.Br,{Number = 35; Symbol = "Br"; Name = "Bromine"; Mass = 79.904; DisplayColor = "#a62929"; ElectronConfiguration = "[Ar] 3d10 4s2 4p5"; Electronegativity = Some 2.96; ElectronAffinity = Some -325; Radius = Some 114; MinOxidation = -1; MaxOxidation = 7; Valences = [7; 5; 3; 1; -1]}
    Element.Kr,{Number = 36; Symbol = "Kr"; Name = "Krypton"; Mass = 83.798; DisplayColor = "#5cb8d1"; ElectronConfiguration = "[Ar] 3d10 4s2 4p6"; Electronegativity = None; ElectronAffinity = Some 0; Radius = Some 110; MinOxidation = 0; MaxOxidation = 2; Valences = [2; 0]}
    Element.Rb,{Number = 37; Symbol = "Rb"; Name = "Rubidium"; Mass = 85.4678; DisplayColor = "#702eb0"; ElectronConfiguration = "[Kr] 5s1"; Electronegativity = Some 0.82; ElectronAffinity = Some -47; Radius = Some 211; MinOxidation = 0; MaxOxidation = 1; Valences = [1]}
    Element.Sr,{Number = 38; Symbol = "Sr"; Name = "Strontium"; Mass = 87.62; DisplayColor = "#00ff00"; ElectronConfiguration = "[Kr] 5s2"; Electronegativity = Some 0.95; ElectronAffinity = Some -5; Radius = Some 192; MinOxidation = 0; MaxOxidation = 2; Valences = [2]}
    Element.Y,{Number = 39; Symbol = "Y"; Name = "Yttrium"; Mass = 88.90585; DisplayColor = "#94ffff"; ElectronConfiguration = "[Kr] 4d1 5s2"; Electronegativity = Some 1.22; ElectronAffinity = Some -30; Radius = Some 162; MinOxidation = 0; MaxOxidation = 3; Valences = [3]}
    Element.Zr,{Number = 40; Symbol = "Zr"; Name = "Zirconium"; Mass = 91.224; DisplayColor = "#94e0e0"; ElectronConfiguration = "[Kr] 4d2 5s2"; Electronegativity = Some 1.33; ElectronAffinity = Some -41; Radius = Some 148; MinOxidation = 0; MaxOxidation = 4; Valences = [4]}
    Element.Nb,{Number = 41; Symbol = "Nb"; Name = "Niobium"; Mass = 92.90638; DisplayColor = "#73c2c9"; ElectronConfiguration = "[Kr] 4d4 5s1"; Electronegativity = Some 1.6; ElectronAffinity = Some -86; Radius = Some 137; MinOxidation = -1; MaxOxidation = 5; Valences = [5; 3]}
    Element.Mo,{Number = 42; Symbol = "Mo"; Name = "Molybdenum"; Mass = 95.96; DisplayColor = "#54b5b5"; ElectronConfiguration = "[Kr] 4d5 5s1"; Electronegativity = Some 2.16; ElectronAffinity = Some -72; Radius = Some 145; MinOxidation = -2; MaxOxidation = 6; Valences = [6; 5; 4; 3; 2]}
    Element.Tc,{Number = 43; Symbol = "Tc"; Name = "Technetium"; Mass = 98; DisplayColor = "#3b9e9e"; ElectronConfiguration = "[Kr] 4d5 5s2"; Electronegativity = Some 1.9; ElectronAffinity = Some -53; Radius = Some 156; MinOxidation = -3; MaxOxidation = 7; Valences = [7; 6; 4]}
    Element.Ru,{Number = 44; Symbol = "Ru"; Name = "Ruthenium"; Mass = 101.07; DisplayColor = "#248f8f"; ElectronConfiguration = "[Kr] 4d7 5s1"; Electronegativity = Some 2.2; ElectronAffinity = Some -101; Radius = Some 126; MinOxidation = -2; MaxOxidation = 8; Valences = [8; 6; 4; 3; 2]}
    Element.Rh,{Number = 45; Symbol = "Rh"; Name = "Rhodium"; Mass = 102.9055; DisplayColor = "#0a7d8c"; ElectronConfiguration = "[Kr] 4d8 5s1"; Electronegativity = Some 2.28; ElectronAffinity = Some -110; Radius = Some 135; MinOxidation = -1; MaxOxidation = 6; Valences = [6; 4; 3; 2; 1]}
    Element.Pd,{Number = 46; Symbol = "Pd"; Name = "Palladium"; Mass = 106.42; DisplayColor = "#006985"; ElectronConfiguration = "[Kr] 4d10"; Electronegativity = Some 2.2; ElectronAffinity = Some -54; Radius = Some 131; MinOxidation = 0; MaxOxidation = 4; Valences = [4; 2; 0]}
    Element.Ag,{Number = 47; Symbol = "Ag"; Name = "Silver"; Mass = 107.8682; DisplayColor = "#c0c0c0"; ElectronConfiguration = "[Kr] 4d10 5s1"; Electronegativity = Some 1.93; ElectronAffinity = Some -126; Radius = Some 153; MinOxidation = 0; MaxOxidation = 3; Valences = [3; 2; 1]}
    Element.Cd,{Number = 48; Symbol = "Cd"; Name = "Cadmium"; Mass = 112.411; DisplayColor = "#ffd98f"; ElectronConfiguration = "[Kr] 4d10 5s2"; Electronegativity = Some 1.69; ElectronAffinity = Some 0; Radius = Some 148; MinOxidation = 0; MaxOxidation = 2; Valences = [2]}
    Element.In,{Number = 49; Symbol = "In"; Name = "Indium"; Mass = 114.818; DisplayColor = "#a67573"; ElectronConfiguration = "[Kr] 4d10 5s2 5p1"; Electronegativity = Some 1.78; ElectronAffinity = Some -29; Radius = Some 144; MinOxidation = 0; MaxOxidation = 3; Valences = [3; 1]}
    Element.Sn,{Number = 50; Symbol = "Sn"; Name = "Tin"; Mass = 118.71; DisplayColor = "#668080"; ElectronConfiguration = "[Kr] 4d10 5s2 5p2"; Electronegativity = Some 1.96; ElectronAffinity = Some -107; Radius = Some 141; MinOxidation = -4; MaxOxidation = 4; Valences = [4; 2]}
    Element.Sb,{Number = 51; Symbol = "Sb"; Name = "Antimony"; Mass = 121.76; DisplayColor = "#9e63b5"; ElectronConfiguration = "[Kr] 4d10 5s2 5p3"; Electronegativity = Some 2.05; ElectronAffinity = Some -103; Radius = Some 138; MinOxidation = -3; MaxOxidation = 5; Valences = [5; 3; -3]}
    Element.Te,{Number = 52; Symbol = "Te"; Name = "Tellurium"; Mass = 127.6; DisplayColor = "#d47a00"; ElectronConfiguration = "[Kr] 4d10 5s2 5p4"; Electronegativity = Some 2.1; ElectronAffinity = Some -190; Radius = Some 135; MinOxidation = -2; MaxOxidation = 6; Valences = [6; 4; 2; -2]}
    Element.I,{Number = 53; Symbol = "I"; Name = "Iodine"; Mass = 126.90447; DisplayColor = "#940094"; ElectronConfiguration = "[Kr] 4d10 5s2 5p5"; Electronegativity = Some 2.66; ElectronAffinity = Some -295; Radius = Some 133; MinOxidation = -1; MaxOxidation = 7; Valences = [7; 5; 3; 1; -1]}
    Element.Xe,{Number = 54; Symbol = "Xe"; Name = "Xenon"; Mass = 131.293; DisplayColor = "#429eb0"; ElectronConfiguration = "[Kr] 4d10 5s2 5p6"; Electronegativity = None; ElectronAffinity = Some 0; Radius = Some 130; MinOxidation = 0; MaxOxidation = 8; Valences = [8; 6; 4; 2; 0]}
    Element.Cs,{Number = 55; Symbol = "Cs"; Name = "Cesium"; Mass = 132.9054519; DisplayColor = "#57178f"; ElectronConfiguration = "[Xe] 6s1"; Electronegativity = Some 0.79; ElectronAffinity = Some -46; Radius = Some 225; MinOxidation = 0; MaxOxidation = 1; Valences = [1]}
    Element.Ba,{Number = 56; Symbol = "Ba"; Name = "Barium"; Mass = 137.327; DisplayColor = "#00c900"; ElectronConfiguration = "[Xe] 6s2"; Electronegativity = Some 0.89; ElectronAffinity = Some -14; Radius = Some 198; MinOxidation = 0; MaxOxidation = 2; Valences = [2]}
    Element.La,{Number = 57; Symbol = "La"; Name = "Lanthanum"; Mass = 138.90547; DisplayColor = "#70d4ff"; ElectronConfiguration = "[Xe] 5d1 6s2"; Electronegativity = Some 1.1; ElectronAffinity = Some -48; Radius = Some 169; MinOxidation = 0; MaxOxidation = 3; Valences = [3]}
    Element.Ce,{Number = 58; Symbol = "Ce"; Name = "Cerium"; Mass = 140.116; DisplayColor = "#ffffc7"; ElectronConfiguration = "[Xe] 4f1 5d1 6s2"; Electronegativity = Some 1.12; ElectronAffinity = Some -50; Radius = None; MinOxidation = 0; MaxOxidation = 4; Valences = [4; 3]}
    Element.Pr,{Number = 59; Symbol = "Pr"; Name = "Praseodymium"; Mass = 140.90765; DisplayColor = "#d9ffc7"; ElectronConfiguration = "[Xe] 4f3 6s2"; Electronegativity = Some 1.13; ElectronAffinity = Some -50; Radius = None; MinOxidation = 0; MaxOxidation = 4; Valences = [4; 3]}
    Element.Nd,{Number = 60; Symbol = "Nd"; Name = "Neodymium"; Mass = 144.242; DisplayColor = "#c7ffc7"; ElectronConfiguration = "[Xe] 4f4 6s2"; Electronegativity = Some 1.14; ElectronAffinity = Some -50; Radius = None; MinOxidation = 0; MaxOxidation = 3; Valences = [3]}
    Element.Pm,{Number = 61; Symbol = "Pm"; Name = "Promethium"; Mass = 145; DisplayColor = "#a3ffc7"; ElectronConfiguration = "[Xe] 4f5 6s2"; Electronegativity = Some 1.13; ElectronAffinity = Some -50; Radius = None; MinOxidation = 0; MaxOxidation = 3; Valences = [3]}
    Element.Sm,{Number = 62; Symbol = "Sm"; Name = "Samarium"; Mass = 150.36; DisplayColor = "#8fffc7"; ElectronConfiguration = "[Xe] 4f6 6s2"; Electronegativity = Some 1.17; ElectronAffinity = Some -50; Radius = None; MinOxidation = 0; MaxOxidation = 3; Valences = [3; 2]}
    Element.Eu,{Number = 63; Symbol = "Eu"; Name = "Europium"; Mass = 151.964; DisplayColor = "#61ffc7"; ElectronConfiguration = "[Xe] 4f7 6s2"; Electronegativity = Some 1.2; ElectronAffinity = Some -50; Radius = None; MinOxidation = 0; MaxOxidation = 3; Valences = [3; 2]}
    Element.Gd,{Number = 64; Symbol = "Gd"; Name = "Gadolinium"; Mass = 157.25; DisplayColor = "#45ffc7"; ElectronConfiguration = "[Xe] 4f7 5d1 6s2"; Electronegativity = Some 1.2; ElectronAffinity = Some -50; Radius = None; MinOxidation = 0; MaxOxidation = 3; Valences = [3]}
    Element.Tb,{Number = 65; Symbol = "Tb"; Name = "Terbium"; Mass = 158.92535; DisplayColor = "#30ffc7"; ElectronConfiguration = "[Xe] 4f9 6s2"; Electronegativity = Some 1.2; ElectronAffinity = Some -50; Radius = None; MinOxidation = 0; MaxOxidation = 4; Valences = [4; 3]}
    Element.Dy,{Number = 66; Symbol = "Dy"; Name = "Dysprosium"; Mass = 162.5; DisplayColor = "#1fffc7"; ElectronConfiguration = "[Xe] 4f10 6s2"; Electronegativity = Some 1.22; ElectronAffinity = Some -50; Radius = None; MinOxidation = 0; MaxOxidation = 3; Valences = [3]}
    Element.Ho,{Number = 67; Symbol = "Ho"; Name = "Holmium"; Mass = 164.93032; DisplayColor = "#00ff9c"; ElectronConfiguration = "[Xe] 4f11 6s2"; Electronegativity = Some 1.23; ElectronAffinity = Some -50; Radius = None; MinOxidation = 0; MaxOxidation = 3; Valences = [3]}
    Element.Er,{Number = 68; Symbol = "Er"; Name = "Erbium"; Mass = 167.259; DisplayColor = "#00e675"; ElectronConfiguration = "[Xe] 4f12 6s2"; Electronegativity = Some 1.24; ElectronAffinity = Some -50; Radius = None; MinOxidation = 0; MaxOxidation = 3; Valences = [3]}
    Element.Tm,{Number = 69; Symbol = "Tm"; Name = "Thulium"; Mass = 168.93421; DisplayColor = "#00d452"; ElectronConfiguration = "[Xe] 4f13 6s2"; Electronegativity = Some 1.25; ElectronAffinity = Some -50; Radius = None; MinOxidation = 0; MaxOxidation = 3; Valences = [3; 2]}
    Element.Yb,{Number = 70; Symbol = "Yb"; Name = "Ytterbium"; Mass = 173.054; DisplayColor = "#00bf38"; ElectronConfiguration = "[Xe] 4f14 6s2"; Electronegativity = Some 1.1; ElectronAffinity = Some -50; Radius = None; MinOxidation = 0; MaxOxidation = 3; Valences = [3; 2]}
    Element.Lu,{Number = 71; Symbol = "Lu"; Name = "Lutetium"; Mass = 174.9668; DisplayColor = "#00ab24"; ElectronConfiguration = "[Xe] 4f14 5d1 6s2"; Electronegativity = Some 1.27; ElectronAffinity = Some -50; Radius = Some 160; MinOxidation = 0; MaxOxidation = 3; Valences = [3]}
    Element.Hf,{Number = 72; Symbol = "Hf"; Name = "Hafnium"; Mass = 178.49; DisplayColor = "#4dc2ff"; ElectronConfiguration = "[Xe] 4f14 5d2 6s2"; Electronegativity = Some 1.3; ElectronAffinity = Some 0; Radius = Some 150; MinOxidation = 0; MaxOxidation = 4; Valences = [4]}
    Element.Ta,{Number = 73; Symbol = "Ta"; Name = "Tantalum"; Mass = 180.94788; DisplayColor = "#4da6ff"; ElectronConfiguration = "[Xe] 4f14 5d3 6s2"; Electronegativity = Some 1.5; ElectronAffinity = Some -31; Radius = Some 138; MinOxidation = -1; MaxOxidation = 5; Valences = [5]}
    Element.W,{Number = 74; Symbol = "W"; Name = "Tungsten"; Mass = 183.84; DisplayColor = "#2194d6"; ElectronConfiguration = "[Xe] 4f14 5d4 6s2"; Electronegativity = Some 2.36; ElectronAffinity = Some -79; Radius = Some 146; MinOxidation = -2; MaxOxidation = 6; Valences = [6; 5; 4; 3; 2]}
    Element.Re,{Number = 75; Symbol = "Re"; Name = "Rhenium"; Mass = 186.207; DisplayColor = "#267dab"; ElectronConfiguration = "[Xe] 4f14 5d5 6s2"; Electronegativity = Some 1.9; ElectronAffinity = Some -15; Radius = Some 159; MinOxidation = -3; MaxOxidation = 7; Valences = [7; 6; 4; 2; -1]}
    Element.Os,{Number = 76; Symbol = "Os"; Name = "Osmium"; Mass = 190.23; DisplayColor = "#266696"; ElectronConfiguration = "[Xe] 4f14 5d6 6s2"; Electronegativity = Some 2.2; ElectronAffinity = Some -106; Radius = Some 128; MinOxidation = -2; MaxOxidation = 8; Valences = [8; 6; 4; 3; 2]}
    Element.Ir,{Number = 77; Symbol = "Ir"; Name = "Iridium"; Mass = 192.217; DisplayColor = "#175487"; ElectronConfiguration = "[Xe] 4f14 5d7 6s2"; Electronegativity = Some 2.2; ElectronAffinity = Some -151; Radius = Some 137; MinOxidation = -3; MaxOxidation = 6; Valences = [6; 4; 3; 2; 1]}
    Element.Pt,{Number = 78; Symbol = "Pt"; Name = "Platinum"; Mass = 195.084; DisplayColor = "#d0d0e0"; ElectronConfiguration = "[Xe] 4f14 5d9 6s1"; Electronegativity = Some 2.28; ElectronAffinity = Some -205; Radius = Some 128; MinOxidation = 0; MaxOxidation = 6; Valences = [6; 5; 4; 2; 1; 0]}
    Element.Au,{Number = 79; Symbol = "Au"; Name = "Gold"; Mass = 196.966569; DisplayColor = "#ffd123"; ElectronConfiguration = "[Xe] 4f14 5d10 6s1"; Electronegativity = Some 2.54; ElectronAffinity = Some -223; Radius = Some 144; MinOxidation = -1; MaxOxidation = 5; Valences = [5; 3; 1; -1]}
    Element.Hg,{Number = 80; Symbol = "Hg"; Name = "Mercury"; Mass = 200.59; DisplayColor = "#b8b8d0"; ElectronConfiguration = "[Xe] 4f14 5d10 6s2"; Electronegativity = Some 2; ElectronAffinity = Some 0; Radius = Some 149; MinOxidation = 0; MaxOxidation = 4; Valences = [4; 2; 1]}
    Element.Tl,{Number = 81; Symbol = "Tl"; Name = "Thallium"; Mass = 204.3833; DisplayColor = "#a6544d"; ElectronConfiguration = "[Xe] 4f14 5d10 6s2 6p1"; Electronegativity = Some 2.04; ElectronAffinity = Some -19; Radius = Some 148; MinOxidation = 0; MaxOxidation = 3; Valences = [3; 1]}
    Element.Pb,{Number = 82; Symbol = "Pb"; Name = "Lead"; Mass = 207.2; DisplayColor = "#575961"; ElectronConfiguration = "[Xe] 4f14 5d10 6s2 6p2"; Electronegativity = Some 2.33; ElectronAffinity = Some -35; Radius = Some 147; MinOxidation = -4; MaxOxidation = 4; Valences = [4; 2]}
    Element.Bi,{Number = 83; Symbol = "Bi"; Name = "Bismuth"; Mass = 208.9804; DisplayColor = "#9e4fb5"; ElectronConfiguration = "[Xe] 4f14 5d10 6s2 6p3"; Electronegativity = Some 2.02; ElectronAffinity = Some -91; Radius = Some 146; MinOxidation = -3; MaxOxidation = 5; Valences = [5; 3]}
    Element.Po,{Number = 84; Symbol = "Po"; Name = "Polonium"; Mass = 209; DisplayColor = "#ab5c00"; ElectronConfiguration = "[Xe] 4f14 5d10 6s2 6p4"; Electronegativity = Some 2; ElectronAffinity = Some -183; Radius = None; MinOxidation = -2; MaxOxidation = 6; Valences = [6; 4; 2; -2]}
    Element.At,{Number = 85; Symbol = "At"; Name = "Astatine"; Mass = 210; DisplayColor = "#754f45"; ElectronConfiguration = "[Xe] 4f14 5d10 6s2 6p5"; Electronegativity = Some 2.2; ElectronAffinity = Some -270; Radius = None; MinOxidation = -1; MaxOxidation = 5; Valences = [5; 3; 1; -1]}
    Element.Rn,{Number = 86; Symbol = "Rn"; Name = "Radon"; Mass = 222; DisplayColor = "#428296"; ElectronConfiguration = "[Xe] 4f14 5d10 6s2 6p6"; Electronegativity = None; ElectronAffinity = None; Radius = Some 145; MinOxidation = 0; MaxOxidation = 2; Valences = [2; 0]}
    Element.Fr,{Number = 87; Symbol = "Fr"; Name = "Francium"; Mass = 223; DisplayColor = "#420066"; ElectronConfiguration = "[Rn] 7s1"; Electronegativity = Some 0.7; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 1; Valences = [1]}
    Element.Ra,{Number = 88; Symbol = "Ra"; Name = "Radium"; Mass = 226; DisplayColor = "#007d00"; ElectronConfiguration = "[Rn] 7s2"; Electronegativity = Some 0.9; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 2; Valences = [2]}
    Element.Ac,{Number = 89; Symbol = "Ac"; Name = "Actinium"; Mass = 227; DisplayColor = "#70abfa"; ElectronConfiguration = "[Rn] 6d1 7s2"; Electronegativity = Some 1.1; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 3; Valences = [3]}
    Element.Th,{Number = 90; Symbol = "Th"; Name = "Thorium"; Mass = 232.03806; DisplayColor = "#00baff"; ElectronConfiguration = "[Rn] 6d2 7s2"; Electronegativity = Some 1.3; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 4; Valences = [4; 3; 2]}
    Element.Pa,{Number = 91; Symbol = "Pa"; Name = "Protactinium"; Mass = 231.03588; DisplayColor = "#00a1ff"; ElectronConfiguration = "[Rn] 5f2 6d1 7s2"; Electronegativity = Some 1.5; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 5; Valences = [5; 4]}
    Element.U,{Number = 92; Symbol = "U"; Name = "Uranium"; Mass = 238.02891; DisplayColor = "#008fff"; ElectronConfiguration = "[Rn] 5f3 6d1 7s2"; Electronegativity = Some 1.38; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 6; Valences = [6; 5; 4; 3]}
    Element.Np,{Number = 93; Symbol = "Np"; Name = "Neptunium"; Mass = 237; DisplayColor = "#0080ff"; ElectronConfiguration = "[Rn] 5f4 6d1 7s2"; Electronegativity = Some 1.36; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 7; Valences = [7; 6; 5; 4; 3]}
    Element.Pu,{Number = 94; Symbol = "Pu"; Name = "Plutonium"; Mass = 244; DisplayColor = "#006bff"; ElectronConfiguration = "[Rn] 5f6 7s2"; Electronegativity = Some 1.28; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 7; Valences = [7; 6; 5; 4; 3]}
    Element.Am,{Number = 95; Symbol = "Am"; Name = "Americium"; Mass = 243; DisplayColor = "#545cf2"; ElectronConfiguration = "[Rn] 5f7 7s2"; Electronegativity = Some 1.3; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 6; Valences = [6; 5; 4; 3; 2]}
    Element.Cm,{Number = 96; Symbol = "Cm"; Name = "Curium"; Mass = 247; DisplayColor = "#785ce3"; ElectronConfiguration = "[Rn] 5f7 6d1 7s2"; Electronegativity = Some 1.3; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 4; Valences = [4; 3]}
    Element.Bk,{Number = 97; Symbol = "Bk"; Name = "Berkelium"; Mass = 247; DisplayColor = "#8a4fe3"; ElectronConfiguration = "[Rn] 5f9 7s2"; Electronegativity = Some 1.3; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 4; Valences = [4; 3]}
    Element.Cf,{Number = 98; Symbol = "Cf"; Name = "Californium"; Mass = 251; DisplayColor = "#a136d4"; ElectronConfiguration = "[Rn] 5f10 7s2"; Electronegativity = Some 1.3; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 4; Valences = [4; 3; 2]}
    Element.Es,{Number = 99; Symbol = "Es"; Name = "Einsteinium"; Mass = 252; DisplayColor = "#b31fd4"; ElectronConfiguration = "[Rn] 5f11 7s2"; Electronegativity = Some 1.3; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 3; Valences = [3; 2]}
    Element.Fm,{Number = 100; Symbol = "Fm"; Name = "Fermium"; Mass = 257; DisplayColor = "#b31fba"; ElectronConfiguration = "[Rn] 5f12 7s2"; Electronegativity = Some 1.3; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 3; Valences = [3; 2]}
    Element.Md,{Number = 101; Symbol = "Md"; Name = "Mendelevium"; Mass = 258; DisplayColor = "#b30da6"; ElectronConfiguration = "[Rn] 5f13 7s2"; Electronegativity = Some 1.3; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 3; Valences = [3; 2]}
    Element.No,{Number = 102; Symbol = "No"; Name = "Nobelium"; Mass = 259; DisplayColor = "#bd0d87"; ElectronConfiguration = "[Rn] 5f14 7s2"; Electronegativity = Some 1.3; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 3; Valences = [3; 2]}
    Element.Lr,{Number = 103; Symbol = "Lr"; Name = "Lawrencium"; Mass = 262; DisplayColor = "#c70066"; ElectronConfiguration = "[Rn] 5f14 7s2 7p1"; Electronegativity = Some 1.3; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 3; Valences = [3]}
    Element.Rf,{Number = 104; Symbol = "Rf"; Name = "Rutherfordium"; Mass = 267; DisplayColor = "#cc0059"; ElectronConfiguration = "[Rn] 5f14 6d2 7s2"; Electronegativity = None; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 4; Valences = [4]}
    Element.Db,{Number = 105; Symbol = "Db"; Name = "Dubnium"; Mass = 268; DisplayColor = "#d1004f"; ElectronConfiguration = "[Rn] 5f14 6d3 7s2"; Electronegativity = None; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 0; Valences = [5; 4; 3]}
    Element.Sg,{Number = 106; Symbol = "Sg"; Name = "Seaborgium"; Mass = 271; DisplayColor = "#d90045"; ElectronConfiguration = "[Rn] 5f14 6d4 7s2"; Electronegativity = None; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 0; Valences = [6; 5; 4]}
    Element.Bh,{Number = 107; Symbol = "Bh"; Name = "Bohrium"; Mass = 272; DisplayColor = "#e00038"; ElectronConfiguration = "[Rn] 5f14 6d5 7s2"; Electronegativity = None; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 0; Valences = [7; 5; 4]}
    Element.Hs,{Number = 108; Symbol = "Hs"; Name = "Hassium"; Mass = 270; DisplayColor = "#e6002e"; ElectronConfiguration = "[Rn] 5f14 6d6 7s2"; Electronegativity = None; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 0; Valences = [8; 6; 4; 2]}
    Element.Mt,{Number = 109; Symbol = "Mt"; Name = "Meitnerium"; Mass = 276; DisplayColor = "#eb0026"; ElectronConfiguration = "[Rn] 5f14 6d7 7s2"; Electronegativity = None; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 0; Valences = [6; 4; 3]}
    Element.Ds,{Number = 110; Symbol = "Ds"; Name = "Darmstadtium"; Mass = 281; DisplayColor = ""; ElectronConfiguration = "[Rn] 5f14 6d9 7s1"; Electronegativity = None; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 0; Valences = [6; 4; 2; 0]}
    Element.Rg,{Number = 111; Symbol = "Rg"; Name = "Roentgenium"; Mass = 280; DisplayColor = ""; ElectronConfiguration = "[Rn] 5f14 6d10 7s1"; Electronegativity = None; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 0; Valences = [5; 3; 1]}
    Element.Cn,{Number = 112; Symbol = "Cn"; Name = "Copernicium"; Mass = 285; DisplayColor = ""; ElectronConfiguration = "[Rn] 5f14 6d10 7s2"; Electronegativity = None; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 0; Valences = [2; 4]}
]

let maxValences (element:Element) =
    elementInfo[element].Valences |> List.max

let aliphaticOrganic =
    [ "Cl", Element.Cl; "Br", Element.Br; "B", Element.B; "C", Element.C; "N", Element.N; "O", Element.O
      "S", Element.S; "P", Element.P; "F", Element.F; "I", Element.I ] |> Map.ofList

let aromaticOrganic =
    ["b", Element.B; "c", Element.C; "n", Element.N; "o", Element.O
     "s", Element.S; "p", Element.P] |> Map.ofList