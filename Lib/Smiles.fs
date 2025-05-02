module Lib.SmilesParser

open System
open FParsec
open FParsec.Pipes
open Microsoft.FSharp.Reflection
open Lib.Types
open System.Collections.Generic


type RingClosure =
    { 
      To: int
      BondType: BondType option
      }


type ParseState = { Idx: int; Atoms: Element list; Rings: Map<int, RingClosure> }
   with
    static member Default = { Idx=0; Atoms=[]; Rings=Map []} 

let addAtom {Idx=idx;Atoms=atoms; Rings=rings} (atom:Element) =
    {Idx=idx+1;Atoms=atom :: atoms;Rings=rings}
    

let aliphatic_organic: Parser<Element * bool, ParseState> =  %[ "Cl"; "Br"; "B"; "C"; "N"; "O"; "S"; "P"; "F"; "I" ] |>> (fun s -> (stringToElement s, false))
    
let aromatic_organic: Parser<Element * bool, ParseState> = %["b"; "c"; "n"; "o"; "s"; "p"] |>> (fun s -> (stringToElement s, true))

let ws: Parser<unit, ParseState> = spaces

let isotope: Parser<int option, ParseState> = opt pint32

let chiral: Parser<Chirality option, ParseState> = opt ((stringReturn "@@" Chirality.Clockwise) <|> (stringReturn "@" Chirality.CounterClockwise))

let hcount: Parser<int option, ParseState> =
                 %% "H"
                 -- +. opt pint32
                 -%> auto

let charge: Parser<int option, ParseState>= opt (%["+";"-"] >>. pint32)

let atom_class: Parser<int option, ParseState> = opt ( %":" >>. pint32)

let symbol  = %(
    FSharpType.GetUnionCases(typeof<Element>)
    |> Array.map (fun case -> case.Name)
    |> Array.sortByDescending String.length
    |> Array.map (fun s -> if s = "UNK" then "*" else s)
    |> Array.toList) |>> (fun s -> stringToElement s, Char.IsLower(s.[0]))

let bracket_atom: Parser<Atom, ParseState>  =
    %% "["
    -- +. isotope
    -- +. symbol
    -- +. chiral
    -- +. hcount
    -- +. charge
    -- +. atom_class
    -- "]"
    -%> fun isotope symbol chiral hcount charge sclass   -> {Atom.Isotope=isotope
                                                             Symbol=fst symbol
                                                             Chirality=chiral
                                                             Hydrogens=hcount
                                                             Charge=charge
                                                             AtomClass=sclass
                                                             IsAromatic=snd symbol
                                                             }