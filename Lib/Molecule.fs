module Molecule

open System.Collections.Generic
open Lib.SmilesParser
open Lib.Types
open Lib.Graph
open FSharp.HashCollections

type AtomID = NodeID

type BondedAtom =
    {Bond:Bond; Atom:Atom}


let atomIDs (molecule:MolGraph) : AtomID seq =
    nodesIDs molecule

let updateAtoms (updater:Atom -> Atom) (molecule:MolGraph) : MolGraph =
    let newNodeData = molecule.NodeData |>
                      Seq.map (|KeyValue|) |>
                      Seq.map (fun (id, atom) -> id, (updater atom)) |>
                      Seq.map KeyValuePair |>
                      HashMap.ofSeq
    {molecule with NodeData=newNodeData}

type AtomUpdater = AtomID -> MolGraph -> Atom

type AtomCheckError =
    | TooManyValences of AtomID * int

type AtomChecker = AtomID -> MolGraph -> AtomCheckError option

let updateAtomsWithContext (updater: AtomUpdater) (molecule:MolGraph) : MolGraph =
    let newNodeData = nodesIDs molecule |>
                      Seq.map (fun id -> id, (updater id molecule)) |>
                      Seq.map KeyValuePair |>
                      HashMap.ofSeq
    {molecule with NodeData=newNodeData}
    
let checkMolAtomsWithContext (checker: AtomChecker) (molecule:MolGraph) : AtomCheckError list =
    nodesIDs molecule |>
    Seq.map (fun id -> checker id molecule) |> Seq.choose id |> List.ofSeq

let getAtom (id:AtomID) (molecule:MolGraph) : Atom =
    getNodeData id molecule

let addAtom (atom:Atom) (molecule:MolGraph) =
    addNodeFromNodeData atom molecule
    
let removeAtom (atomID:AtomID) (molecule:MolGraph) =
    removeNode atomID molecule
    
let tryAddBond (atomID1: AtomID) (atomID2: AtomID) (bond:Bond) (molecule:MolGraph)=
    let edge = {edgeData=bond; nodes = NodeIDSet.construct atomID1 atomID2 }
    tryAddEdge edge molecule
    
let addBond (atomID1: AtomID) (atomID2: AtomID) (bond:Bond) (molecule:MolGraph) =
    let edge = {Edge.nodes=NodeIDSet.construct atomID1 atomID2; edgeData=bond}
    addEdge edge molecule
    
let removeBond (atomID1: AtomID) (atomID2: AtomID) (molecule:MolGraph) =
    removeEdge atomID1 atomID2 molecule
    
let tryChangeAtom (atomID: AtomID) (atom:Atom) (molecule:MolGraph) =
    changeNode atomID atom molecule
    
let changeAtom (atomID: AtomID) (atom:Atom) (molecule:MolGraph) =
    match tryChangeAtom atomID atom molecule with
    | Some newMol -> newMol
    | None -> molecule
    
let tryChangeBond (atomID1: AtomID) (atomID2: AtomID) (bond:Bond) (molecule:MolGraph) =
    changeEdge atomID1 atomID2 bond molecule
    
let changeBond (atomID1: AtomID) (atomID2: AtomID) (bond:Bond) (molecule:MolGraph) =
    match tryChangeBond atomID1 atomID2 bond molecule with
    | Some newMol -> newMol
    | None -> molecule
    
let tryFromSmiles (smi:string) : MolGraph list option =
    smilesToMol smi

let getConnectedAtomsAndBonds (atomID:AtomID) (molecule:MolGraph) : BondedAtom list =
    let connectedEdged = nodeEdges atomID molecule
    connectedEdged |> List.map (fun edge -> edge.edgeData, (getNodeData (edge.nodes.otherNode atomID).Value molecule)) |> List.map (fun (b,a) -> {Bond=b; Atom=a})
    
let getBonds (atomID:AtomID) (molecule:MolGraph) : Bond list =
    nodeEdges atomID molecule |> List.map _.edgeData

let bondsToValences (bonds: Bond list) : float =
    bonds |> List.map(_.Type) |> List.map bondTypeValenceContribution |> List.sum

let updateImplicitHydrogens (molecule:MolGraph) : MolGraph =
    let updater (atomID:AtomID) (contextMol:MolGraph) : Atom =
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



let kekulize (molecule:MolGraph): MolGraph =
    // // First, set all bonds to single bonds
    // let singleBondMol =
    //     let edges = molecule.Edges |> Seq.toList
    //     let updateEdge edge =
    //         { edge with edgeData = { edge.edgeData with Type = BondType.Single } }
    //     { molecule with Edges = edges |> List.map updateEdge |> HashSet.ofList }

    // // Helper to get connected atoms and their bonds for a given atom
    // let getConnectedAtomsWithBonds (atomID: AtomID) (mol: MolGraph) : (Atom * Bond) list =
    //     let edges = nodeEdges atomID mol
    //     edges
    //     |> List.map (fun edge ->
    //     let mutable adjusted = false
    //     let newEdges =
    //         currentMol.Edges
    //         |> Set.toList
    //         |> List.map (fun edge ->
    //             let atom1ID = edge.nodes.FirstNode
    //             let atom2ID = edge.nodes.otherNode atom1ID |> Option.get
    //             let atom1 = getNodeData atom1ID currentMol
    //             let atom2 = getNodeData atom2ID currentMol

    //             // Check if we can make this a double bond
    //             let canBeDouble =
    //                 match atom1.Element, atom2.Element with
    //                 | Element.C, Element.C -> true  // Carbon-carbon double bonds are common
    //                 | Element.N, Element.C when atom2.Hydrogens >= 1 -> true  // Nitrogen with hydrogen
    //                 | Element.O, Element.C when atom2.Hydrogens >= 1 -> true  // Oxygen with hydrogen
    //                 | _ -> false

    //             if canBeDouble && edge.edgeData.Type = BondType.Single then
    //                 adjusted <- true
    //                 { edge with edgeData = { edge.edgeData with Type = BondType.Double } }
    //             else
    //                 edge)
    //         |> HashSet.ofList

    //     if adjusted then
    //         tryAdjustBonds { currentMol with Edges = newEdges } (iterations - 1)
    //     else
    //         currentMol

    // // Start with all single bonds and try to adjust
    // tryAdjustBonds singleBondMol 10

    molecule
