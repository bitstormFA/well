module Lib.SmilesParser

open System
open FParsec
open FParsec.Pipes
open Lib.Types
open Lib.Graph
open System.Collections.Generic


type RingClosure =
    { 
      atomId: NodeID
    }

type ParseState = {
   mutable branches: Stack<NodeID>
   mutable ringConnect: Dictionary<int, RingClosure>
   mutable mol: MolGraph
   mutable currentBondType: BondType option
   mutable connectID: NodeID option
   mutable ez: int Option
   }
   with
    static member Default =
                              {
                                branches=Stack()
                                ringConnect=Dictionary()
                                mol=Graph.Empty
                                currentBondType = None
                                connectID = None
                                ez = None 
                                }
    member this.setCurrentBond b = this.currentBondType <- b
    member this.getCurrentBond: Bond = match this.currentBondType with
                                           | None -> Bond.fromBondType BondType.Single
                                           | Some bt -> Bond.fromBondType bt                                          
    member this.addAtomDefault a =
        match this.connectID with
        | Some connectTo ->
            let connectAtom = tryGetNodeData connectTo this.mol
            let bondType = if this.currentBondType.IsNone && connectAtom.Value.IsAromatic && a.IsAromatic then BondType.Aromatic else this.getCurrentBond.Type
            let newMol, newItems = if this.ez.IsNone then
                                       (addNodeToNode a connectTo (Bond.fromBondType bondType) this.mol)
                                   else
                                       let direction = match this.ez with
                                                        | Some 1 -> BondDirection.CisTrans1
                                                        | Some 2 -> BondDirection.CisTrans2
                                                        | _ -> BondDirection.NoDirection
                                       (addNodeToNode a connectTo {Bond.Type = bondType; Conjugated=false; Direction=direction} this.mol)
                
            this.mol <- newMol
            match newItems with
            | Some (_, newNodeID) -> this.connectID <- Some newNodeID; this.currentBondType <- None; Some newNodeID
            | None -> None  
        | None ->
            let newMol, newNodeId = addNode a this.mol
            this.mol <- newMol
            this.connectID <- Some newNodeId
            Some this.connectID.Value
    
    member this.openBranch =
        match this.connectID with
        | Some cid -> this.branches.Push cid
        | None -> raise (IndexOutOfRangeException("structure can't start with a branch"))
        this.connectID
        
    member this.closeBranch =
        match this.branches.Count > 0 with
        | true -> this.connectID <- Some (this.branches.Pop())
        | false -> raise (IndexOutOfRangeException("Closing branch without any open"))
        
    member this.openRing ringID =
        let rc = {atomId=this.connectID.Value}
        this.ringConnect.Add(ringID, rc)
        
    member this.closeRing ringID =
        match this.ringConnect.ContainsKey ringID with
        | true ->
            let rc = this.ringConnect[ringID]
            let bondType = if this.currentBondType.IsNone && this.mol.NodeData[rc.atomId].IsAromatic && this.mol.NodeData[this.connectID.Value].IsAromatic then BondType.Aromatic else this.getCurrentBond.Type
            let edge = {nodes=NodeIDSet.construct rc.atomId this.connectID.Value; edgeData=Bond.fromBondType bondType}
            let newMol = addEdge edge this.mol
            this.mol <- newMol
        | false -> raise (IndexOutOfRangeException($"Trying to close ring with id {ringID} that isn't open"))

let aliphatic_organic: Parser<Atom, ParseState> =  %[ "Cl"; "Br"; "B"; "C"; "N"; "O"; "S"; "P"; "F"; "I" ] |>>
                                                   (fun s -> {Atom.Default with Element =(stringToElement s); IsAromatic=false})
    
let aromatic_organic: Parser<Atom, ParseState> = %["b"; "c"; "n"; "o"; "s"; "p"] |>>
                                                 (fun s -> {Atom.Default with Element =(stringToElement s); IsAromatic=true})

let ws: Parser<unit, ParseState> = spaces

let isotope: Parser<int option, ParseState> = opt pint32

let chiral: Parser<Chirality option, ParseState> = opt ((stringReturn "@@" Chirality.Clockwise) <|> (stringReturn "@" Chirality.CounterClockwise))

let hCount: Parser<int option, ParseState> = opt (
                 %% "H"
                 -- +. opt pint32  -%>
                 fun x -> match x with
                          | Some n -> n
                          | None -> 1)


let psign: Parser<int, ParseState> = %% +. %['+'; '-'] -%> fun x -> if x = '+' then 1 else -1

let charge: Parser<int, ParseState> = %% +. psign -- +. opt pint32 -%>
                                      (fun s i -> match i with
                                                  | Some n -> s * n
                                                  | None -> s * 1)

let atom_class: Parser<int option, ParseState> = opt ( %":" >>. pint32)

let symbol  = %(
    elementMap.Keys|>
    Seq.toList) |>> (fun s -> stringToElement s, Char.IsLower(s[0]))



let bracket_atom: Parser<Atom, ParseState>  =
    %% "["
    -- +. isotope
    -- +. symbol
    -- +. chiral
    -- +. hCount
    -- +. opt charge
    -- +. atom_class
    -- "]"
    -%> fun isotope symbol chiral hCount charge sClass   -> {Atom.Isotope=isotope
                                                             Element=fst symbol
                                                             Chirality=chiral
                                                             Hydrogens=Option.defaultValue 0 hCount
                                                             FormalCharge=Option.defaultValue 0 charge
                                                             ImplicitHydrogens=0
                                                             AtomClass=sClass
                                                             IsAromatic=snd symbol
                                                             Hybridization=None 
                                                             }

let changeState  f p =
    p >>= fun x -> updateUserState (f x) >>% ()
   
let changeState2  f p =
    p >>= fun x -> updateUserState (f x) >>% x
    
let bonds = %List.ofSeq(bondMap.Keys)
let bond: Parser<unit, ParseState> = %% +. bonds -%> (fun bk -> bk, bondMap[bk])
                                     |> changeState (fun bt state ->
                                         state.currentBondType <- Some (snd bt)
                                         match fst bt with
                                         | "/" -> state.ez <- Some 1
                                         | @"\" -> state.ez <- Some 2
                                         | _ -> state.ez <- None
                                         state)
                                     

let ringID: Parser<unit, ParseState> = digit |>> int <|>  %% %'%' -- +. digit -- +. digit -%>( fun d1 d2-> int(d1+d2))
                                       |> changeState (fun id state ->
                                           match state.ringConnect.ContainsKey id with
                                           |true -> state.closeRing(id)
                                           |false -> state.openRing(id)
                                           |> ignore
                                           state) 
                                    
let atom = bracket_atom <|> aliphatic_organic <|> aromatic_organic
           |> changeState (fun atom state ->
               state.addAtomDefault(atom) |> ignore
               state) 

let openBranch  = %'(' |> changeState (fun _ (state:ParseState) ->
                                                state.openBranch |> ignore
                                                state)
                                                


let closeBranch = %')' |> changeState (fun _ (state:ParseState) ->
                                                 state.closeBranch
                                                 state)

let chain = many (atom <|> openBranch <|> closeBranch <|> ringID <|> bond) >>. getUserState |>> _.mol 

let smiles =  sepBy chain (pstring ".")

let runParser p str  =
    let initState = ParseState.Default
    let parseResult = runParserOnString p initState "" str 
    match parseResult with 
    | ParserResult.Success (result, _, _) -> Some result
    | ParserResult.Failure _ -> None
    
let smilesToMol (smi:string) =
    runParser smiles smi