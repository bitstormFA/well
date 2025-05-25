module Lib.SmilesParser

open System
open FParsec
open FParsec.Pipes
open Microsoft.FSharp.Reflection
open Lib.Types
open Lib.Graph
open System.Collections.Generic


type RingClosure =
    { 
      atomId: NodeID
      bondType: BondType
      }


type ParseState = {
   mutable branches: Stack<NodeID>
   mutable ringConnect: Dictionary<int, RingClosure>
   mutable mol: MolGraph
   mutable currentBond: BondType option
   mutable connectID: NodeID option
   }
   with
    static member Default = { branches=Stack(); ringConnect=Dictionary(); mol=MutableGraph.Empty; currentBond = None; connectID = None}
    member this.setCurrentBond b = this.currentBond <- b
    member this.getCurrentBond: BondType = match this.currentBond with
                                           | None -> BondType.Single
                                           | Some bt -> bt                                          
    member this.addAtomDefault a =
        match this.connectID with
        | Some connectTo ->
            match (addNodeAt a connectTo this.getCurrentBond this.mol) with
            | Some (newEdge, newNodeID) -> this.connectID <- Some newNodeID; this.currentBond <- None; Some newNodeID
            | None -> None  
        | None ->
            this.connectID <- Some (addNode a this.mol)
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
        let rc = {atomId=this.connectID.Value; bondType=this.getCurrentBond}
        this.ringConnect.Add(ringID, rc)
        
    member this.closeRing ringID =
        match this.ringConnect.ContainsKey ringID with
        | true ->
            let rc = this.ringConnect[ringID]
            let edge = {fromID=rc.atomId; toID=this.connectID.Value; edgeData=rc.bondType}
            addEdge edge this.mol |> ignore
        | false -> raise (IndexOutOfRangeException($"Trying to close ring with id {ringID} that isn't open"))

let aliphatic_organic: Parser<Atom, ParseState> =  %[ "Cl"; "Br"; "B"; "C"; "N"; "O"; "S"; "P"; "F"; "I" ] |>>
                                                   (fun s -> {Atom.Default with Symbol =(stringToElement s); IsAromatic=false})
    
let aromatic_organic: Parser<Atom, ParseState> = %["b"; "c"; "n"; "o"; "s"; "p"] |>>
                                                 (fun s -> {Atom.Default with Symbol =(stringToElement s); IsAromatic=true})

let ws: Parser<unit, ParseState> = spaces

let isotope: Parser<int option, ParseState> = opt pint32

let chiral: Parser<Chirality option, ParseState> = opt ((stringReturn "@@" Chirality.Clockwise) <|> (stringReturn "@" Chirality.CounterClockwise))

let hCount: Parser<int option, ParseState> = opt (
                 %% "H"
                 -- +. opt pint32  -%>
                 fun x -> match x with
                          | Some n -> n
                          | None -> 1)

let charge: Parser<int option, ParseState>= opt (%["+";"-"] >>. pint32)

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
    -- +. charge
    -- +. atom_class
    -- "]"
    -%> fun isotope symbol chiral hCount charge sClass   -> {Atom.Isotope=isotope
                                                             Symbol=fst symbol
                                                             Chirality=chiral
                                                             Hydrogens=hCount
                                                             Charge=charge
                                                             AtomClass=sClass
                                                             IsAromatic=snd symbol
                                                             }

let changeState  f p =
    p >>= fun x -> updateUserState (f x) >>% ()
   
let changeState2  f p =
    p >>= fun x -> updateUserState (f x) >>% x
    
let bonds = %List.ofSeq(bondMap.Keys)
let bond: Parser<unit, ParseState> = %% +. bonds -%> (fun bk -> bondMap[bk])
                                     |> changeState (fun bt state ->
                                         state.currentBond <- Some bt
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

let openBranch  = %'(' |> changeState (fun c (state:ParseState) ->
                                                state.openBranch |> ignore
                                                state)
                                                


let closeBranch = %')' |> changeState (fun c (state:ParseState) ->
                                                 state.closeBranch
                                                 state)

let dot: Parser<unit,ParseState> = %'.' >>% ()

let chain = many (atom <|> openBranch <|> closeBranch <|> ringID <|> bond)

                          