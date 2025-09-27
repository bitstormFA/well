module Molecule

open System.Collections.Generic
open Lib.SmilesParser
open Lib.Types
open Lib.Graph
open FSharp.HashCollections

type AtomID = NodeID

type BondedAtom =
    {Bond: BondData; Atom: AtomData }

type Atom = Node<AtomData>

type Bond = Edge<BondData>

let atomIDs (molecule:MolGraph) : AtomID seq =
    nodesIDs molecule

let updateAtomData (updater: AtomData -> AtomData) (molecule:MolGraph) : MolGraph =
    updateAllNodeData updater molecule
    
let updateBondData (updater: BondData -> BondData) (molecule:MolGraph) : MolGraph =
    updateAllEdgeData updater molecule

type AtomCheckError =
    | TooManyValences of AtomID * int

type AtomChecker = AtomID -> MolGraph -> AtomCheckError option

type AtomUpdater = AtomID -> MolGraph -> AtomData

let updateAtomsWithContext (updater: AtomUpdater) (molecule:MolGraph) : MolGraph =
    let newNodeData = nodesIDs molecule |>
                      Seq.map (fun id -> id, (updater id molecule)) |>
                      Seq.map KeyValuePair |>
                      HashMap.ofSeq
    {molecule with Nodes=newNodeData}
    
let checkMolAtomsWithContext (checker: AtomChecker) (molecule:MolGraph) : AtomCheckError list =
    nodesIDs molecule |>
    Seq.map (fun id -> checker id molecule) |> Seq.choose id |> List.ofSeq

let getAtom (id:AtomID) (molecule:MolGraph) : AtomData =
    getNodeData id molecule

let addAtom (atom: AtomData) (molecule:MolGraph) =
    addNodeFromNodeData atom molecule
    
let removeAtom (atomID:AtomID) (molecule:MolGraph) =
    removeNode atomID molecule
    
let tryAddBond (atomID1: AtomID) (atomID2: AtomID) (bond: BondData) (molecule:MolGraph)=
    let edge = {edgeData=bond; nodes = NodeIDSet.construct atomID1 atomID2 }
    tryAddEdge edge molecule
    
let addBond (atomID1: AtomID) (atomID2: AtomID) (bond: BondData) (molecule:MolGraph) =
    let edge = {Edge.nodes=NodeIDSet.construct atomID1 atomID2; edgeData=bond}
    addEdge edge molecule
    
let removeBond (atomID1: AtomID) (atomID2: AtomID) (molecule:MolGraph) =
    removeEdge atomID1 atomID2 molecule
    
let tryChangeAtom (atomID: AtomID) (atom: AtomData) (molecule:MolGraph) =
    changeNode atomID atom molecule
    
let changeAtom (atomID: AtomID) (atom: AtomData) (molecule:MolGraph) =
    match tryChangeAtom atomID atom molecule with
    | Some newMol -> newMol
    | None -> molecule
    
let tryChangeBond (atomID1: AtomID) (atomID2: AtomID) (bond: BondData) (molecule:MolGraph) =
    changeEdge atomID1 atomID2 bond molecule
    
let changeBond (atomID1: AtomID) (atomID2: AtomID) (bond: BondData) (molecule:MolGraph) =
    match tryChangeBond atomID1 atomID2 bond molecule with
    | Some newMol -> newMol
    | None -> molecule
    
let tryFromSmiles (smi:string) : MolGraph list option =
    smilesToMol smi

let getConnectedAtomsAndBonds (atomID:AtomID) (molecule:MolGraph) : BondedAtom list =
    let connectedEdged = nodeEdges atomID molecule
    connectedEdged |> List.map (fun edge -> edge.edgeData, (getNodeData (edge.nodes.otherNode atomID).Value molecule)) |> List.map (fun (b,a) -> {Bond=b; Atom=a})
    
let getBonds (atomID:AtomID) (molecule:MolGraph) : BondData list =
    nodeEdges atomID molecule |> List.map _.edgeData

let bondsToValences (bonds: BondData list) : float =
    bonds |> List.map(_.Type) |> List.map bondTypeValenceContribution |> List.sum

let updateImplicitHydrogens (molecule:MolGraph) : MolGraph =
    let updater (atomID:AtomID) (contextMol:MolGraph) : AtomData =
        let bonds = getBonds atomID contextMol
        let atom = getAtom atomID molecule
        let explicitHydrogens = atom.Hydrogens 
        let usedValences = (bonds |> bondsToValences) + float(explicitHydrogens)
        let charge = atom.FormalCharge
        let unusedValences = (maxValences atom.Element) - (int usedValences) - charge
        let implicitHydrogens = if unusedValences < 0 then 0 else unusedValences
        {atom with ImplicitHydrogens=implicitHydrogens}
              
    molecule |> updateAtomsWithContext updater
    
let checkValences (molecule:MolGraph): AtomCheckError list =
    let checker (atomID: AtomID) (contextMol:MolGraph) : AtomCheckError option =
        let atom = getAtom atomID contextMol
        let bonds = getBonds atomID contextMol
        let allHydrogens = atom.Hydrogens + atom.ImplicitHydrogens
        let charge = atom.FormalCharge  
        let usedValences = int(bonds |> bondsToValences) + allHydrogens + charge 
        if usedValences > maxValences atom.Element then Some (TooManyValences (atomID, usedValences)) else None
    checkMolAtomsWithContext checker molecule 

let allBonds (molecule:MolGraph) =
    edges molecule
    
let singleBondedMolecule (m:MolGraph) =
    let updater (b: BondData) : BondData = {b with Type=BondType.Single}
    updateBondData updater m 

// Helper function to kekulize a single ring
let kekulizeRing (molecule: MolGraph) (cycle: Set<NodeIDSet>) : MolGraph =
    // Get the edges in this cycle
    let cycleEdgesList = cycleEdges cycle molecule |> List.ofSeq

    // Filter to only aromatic bonds in this cycle
    let aromaticEdgesInCycle =
        cycleEdgesList
        |> List.filter (fun edge -> edge.edgeData.Type = BondType.Aromatic)

    // If no aromatic bonds in this cycle, return unchanged
    if List.isEmpty aromaticEdgesInCycle then
        molecule
    else
        // Try to assign alternating single/double bonds
        // Start with the first edge as single, then alternate
        let rec assignBonds (mol: MolGraph) (edges: Edge<BondData> list) (isDouble: bool) =
            match edges with
            | [] -> mol
            | edge::rest ->
                let newBondType = if isDouble then BondType.Double else BondType.Single
                let newBondData = {edge.edgeData with Type = newBondType}
                let updatedMol = changeBond edge.nodes.node1 edge.nodes.node2 newBondData mol
                assignBonds updatedMol rest (not isDouble)

        // For rings with even number of aromatic bonds, start with single
        // For rings with odd number (which shouldn't happen in valid aromatic rings), handle gracefully
        assignBonds molecule aromaticEdgesInCycle false

let kekulize (molecule:MolGraph): MolGraph =
    // Find all cycles in the molecule
    let cycles = findMinimumCycleBasis molecule

    // Get all aromatic bonds that need to be kekulized
    let aromaticBonds =
        allBonds molecule
        |> List.filter (fun bond -> bond.edgeData.Type = BondType.Aromatic)

    // If no aromatic bonds, return the molecule unchanged
    if List.isEmpty aromaticBonds then
        molecule
    else
        // Find cycles that contain aromatic bonds
        let aromaticCycles =
            cycles
            |> List.filter (fun cycle ->
                let cycleEdgeData = cycleEdges cycle molecule |> List.ofSeq
                cycleEdgeData |> List.exists (fun edge -> edge.edgeData.Type = BondType.Aromatic))

        // Start with the original molecule and try to kekulize each aromatic cycle
        let rec kekulizeCycles (mol: MolGraph) (remainingCycles: Set<NodeIDSet> list) =
            match remainingCycles with
            | [] -> mol
            | cycle::rest ->
                let kekulizedMol = kekulizeRing mol cycle
                kekulizeCycles kekulizedMol rest

        kekulizeCycles molecule aromaticCycles
