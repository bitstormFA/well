module Lib.Types

type Element =
    | H = 1
    | He = 2
    | Li = 3
    | Be = 4
    | B = 5
    | C = 6
    | N = 7
    | O = 8
    | F = 9
    | Ne = 10
    | Na = 11
    | Mg = 12
    | Al = 13
    | Si = 14
    | P = 15
    | S = 16
    | Cl = 17
    | Ar = 18
    | K = 19
    | Ca = 20
    | Sc = 21
    | Ti = 22
    | V = 23
    | Cr = 24
    | Mn = 25
    | Fe = 26
    | Co = 27
    | Ni = 28
    | Cu = 29
    | Zn = 30
    | Ga = 31
    | Ge = 32
    | As = 33
    | Se = 34
    | Br = 35
    | Kr = 36
    | Rb = 37
    | Sr = 38
    | Y = 39
    | Zr = 40
    | Nb = 41
    | Mo = 42
    | Tc = 43
    | Ru = 44
    | Rh = 45
    | Pd = 46
    | Ag = 47
    | Cd = 48
    | In = 49
    | Sn = 50
    | Sb = 51
    | Te = 52
    | I = 53
    | Xe = 54
    | Cs = 55
    | Ba = 56
    | La = 57
    | Ce = 58
    | Pr = 59
    | Nd = 60
    | Pm = 61
    | Sm = 62
    | Eu = 63
    | Gd = 64
    | Tb = 65
    | Dy = 66
    | Ho = 67
    | Er = 68
    | Tm = 69
    | Yb = 70
    | Lu = 71
    | Hf = 72
    | Ta = 73
    | W = 74
    | Re = 75
    | Os = 76
    | Ir = 77
    | Pt = 78
    | Au = 79
    | Hg = 80
    | Tl = 81
    | Pb = 82
    | Bi = 83
    | Po = 84
    | At = 85
    | Rn = 86
    | Fr = 87
    | Ra = 88
    | Ac = 89
    | Th = 90
    | Pa = 91
    | U = 92
    | Np = 93
    | Pu = 94
    | Am = 95
    | Cm = 96
    | Bk = 97
    | Cf = 98
    | Es = 99
    | Fm = 100
    | Md = 101
    | No = 102
    | Lr = 103
    | Rf = 104
    | Db = 105
    | Sg = 106
    | Bh = 107
    | Hs = 108
    | Mt = 109
    | Ds = 110
    | Rg = 111
    | Cn = 112
    
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
}

let elementInfo = Map [
    Element.H,{Number = 1; Symbol = "H"; Name = "Hydrogen"; Mass = 1.00794; DisplayColor = "#ffffff"; ElectronConfiguration = "1"; Electronegativity = Some 2.2; ElectronAffinity = Some -73; Radius = Some 37; MinOxidation = -1; MaxOxidation = 1}
    Element.He,{Number = 2; Symbol = "He"; Name = "Helium"; Mass = 4.002602; DisplayColor = "#d9ffff"; ElectronConfiguration = "2"; Electronegativity = None; ElectronAffinity = Some 0; Radius = Some 32; MinOxidation = 0; MaxOxidation = 0}
    Element.Li,{Number = 3; Symbol = "Li"; Name = "Lithium"; Mass = 6.941; DisplayColor = "#cc80ff"; ElectronConfiguration = "[He] 2s1"; Electronegativity = Some 0.98; ElectronAffinity = Some -60; Radius = Some 134; MinOxidation = 0; MaxOxidation = 1}
    Element.Be,{Number = 4; Symbol = "Be"; Name = "Beryllium"; Mass = 9.012182; DisplayColor = "#c2ff00"; ElectronConfiguration = "[He] 2s2"; Electronegativity = Some 1.57; ElectronAffinity = Some 0; Radius = Some 90; MinOxidation = 0; MaxOxidation = 2}
    Element.B,{Number = 5; Symbol = "B"; Name = "Boron"; Mass = 10.811; DisplayColor = "#ffb5b5"; ElectronConfiguration = "[He] 2s2 2p1"; Electronegativity = Some 2.04; ElectronAffinity = Some -27; Radius = Some 82; MinOxidation = 0; MaxOxidation = 3}
    Element.C,{Number = 6; Symbol = "C"; Name = "Carbon"; Mass = 12.0107; DisplayColor = "#909090"; ElectronConfiguration = "[He] 2s2 2p2"; Electronegativity = Some 2.55; ElectronAffinity = Some -154; Radius = Some 77; MinOxidation = -4; MaxOxidation = 4}
    Element.N,{Number = 7; Symbol = "N"; Name = "Nitrogen"; Mass = 14.0067; DisplayColor = "#3050f8"; ElectronConfiguration = "[He] 2s2 2p3"; Electronegativity = Some 3.04; ElectronAffinity = Some -7; Radius = Some 75; MinOxidation = -3; MaxOxidation = 5}
    Element.O,{Number = 8; Symbol = "O"; Name = "Oxygen"; Mass = 15.9994; DisplayColor = "#ff0d0d"; ElectronConfiguration = "[He] 2s2 2p4"; Electronegativity = Some 3.44; ElectronAffinity = Some -141; Radius = Some 73; MinOxidation = -2; MaxOxidation = 2}
    Element.F,{Number = 9; Symbol = "F"; Name = "Fluorine"; Mass = 18.9984032; DisplayColor = "#90e050"; ElectronConfiguration = "[He] 2s2 2p5"; Electronegativity = Some 3.98; ElectronAffinity = Some -328; Radius = Some 71; MinOxidation = -1; MaxOxidation = 0}
    Element.Ne,{Number = 10; Symbol = "Ne"; Name = "Neon"; Mass = 20.1797; DisplayColor = "#b3e3f5"; ElectronConfiguration = "[He] 2s2 2p6"; Electronegativity = None; ElectronAffinity = Some 0; Radius = Some 69; MinOxidation = 0; MaxOxidation = 0}
    Element.Na,{Number = 11; Symbol = "Na"; Name = "Sodium"; Mass = 22.98976928; DisplayColor = "#ab5cf2"; ElectronConfiguration = "[Ne] 3s1"; Electronegativity = Some 0.93; ElectronAffinity = Some -53; Radius = Some 154; MinOxidation = -1; MaxOxidation = 1}
    Element.Mg,{Number = 12; Symbol = "Mg"; Name = "Magnesium"; Mass = 24.305; DisplayColor = "#8aff00"; ElectronConfiguration = "[Ne] 3s2"; Electronegativity = Some 1.31; ElectronAffinity = Some 0; Radius = Some 130; MinOxidation = 0; MaxOxidation = 2}
    Element.Al,{Number = 13; Symbol = "Al"; Name = "Aluminum or Aluminium"; Mass = 26.9815386; DisplayColor = "#bfa6a6"; ElectronConfiguration = "[Ne] 3s2 3p1"; Electronegativity = Some 1.61; ElectronAffinity = Some -43; Radius = Some 118; MinOxidation = 0; MaxOxidation = 3}
    Element.Si,{Number = 14; Symbol = "Si"; Name = "Silicon"; Mass = 28.0855; DisplayColor = "#f0c8a0"; ElectronConfiguration = "[Ne] 3s2 3p2"; Electronegativity = Some 1.9; ElectronAffinity = Some -134; Radius = Some 111; MinOxidation = -4; MaxOxidation = 4}
    Element.P,{Number = 15; Symbol = "P"; Name = "Phosphorus"; Mass = 30.973762; DisplayColor = "#ff8000"; ElectronConfiguration = "[Ne] 3s2 3p3"; Electronegativity = Some 2.19; ElectronAffinity = Some -72; Radius = Some 106; MinOxidation = -3; MaxOxidation = 5}
    Element.S,{Number = 16; Symbol = "S"; Name = "Sulfur"; Mass = 32.065; DisplayColor = "#ffff30"; ElectronConfiguration = "[Ne] 3s2 3p4"; Electronegativity = Some 2.58; ElectronAffinity = Some -200; Radius = Some 102; MinOxidation = -2; MaxOxidation = 6}
    Element.Cl,{Number = 17; Symbol = "Cl"; Name = "Chlorine"; Mass = 35.453; DisplayColor = "#1ff01f"; ElectronConfiguration = "[Ne] 3s2 3p5"; Electronegativity = Some 3.16; ElectronAffinity = Some -349; Radius = Some 99; MinOxidation = -1; MaxOxidation = 7}
    Element.Ar,{Number = 18; Symbol = "Ar"; Name = "Argon"; Mass = 39.948; DisplayColor = "#80d1e3"; ElectronConfiguration = "[Ne] 3s2 3p6"; Electronegativity = None; ElectronAffinity = Some 0; Radius = Some 97; MinOxidation = 0; MaxOxidation = 0}
    Element.K,{Number = 19; Symbol = "K"; Name = "Potassium"; Mass = 39.0983; DisplayColor = "#8f40d4"; ElectronConfiguration = "[Ar] 4s1"; Electronegativity = Some 0.82; ElectronAffinity = Some -48; Radius = Some 196; MinOxidation = 0; MaxOxidation = 1}
    Element.Ca,{Number = 20; Symbol = "Ca"; Name = "Calcium"; Mass = 40.078; DisplayColor = "#3dff00"; ElectronConfiguration = "[Ar] 4s2"; Electronegativity = Some 1; ElectronAffinity = Some -2; Radius = Some 174; MinOxidation = 0; MaxOxidation = 2}
    Element.Sc,{Number = 21; Symbol = "Sc"; Name = "Scandium"; Mass = 44.955912; DisplayColor = "#e6e6e6"; ElectronConfiguration = "[Ar] 3d1 4s2"; Electronegativity = Some 1.36; ElectronAffinity = Some -18; Radius = Some 144; MinOxidation = 0; MaxOxidation = 3}
    Element.Ti,{Number = 22; Symbol = "Ti"; Name = "Titanium"; Mass = 47.867; DisplayColor = "#bfc2c7"; ElectronConfiguration = "[Ar] 3d2 4s2"; Electronegativity = Some 1.54; ElectronAffinity = Some -8; Radius = Some 136; MinOxidation = -1; MaxOxidation = 4}
    Element.V,{Number = 23; Symbol = "V"; Name = "Vanadium"; Mass = 50.9415; DisplayColor = "#a6a6ab"; ElectronConfiguration = "[Ar] 3d3 4s2"; Electronegativity = Some 1.63; ElectronAffinity = Some -51; Radius = Some 125; MinOxidation = -1; MaxOxidation = 4}
    Element.Cr,{Number = 24; Symbol = "Cr"; Name = "Chromium"; Mass = 51.9961; DisplayColor = "#8a99c7"; ElectronConfiguration = "[Ar] 3d5 4s1"; Electronegativity = Some 1.66; ElectronAffinity = Some -64; Radius = Some 127; MinOxidation = -2; MaxOxidation = 6}
    Element.Mn,{Number = 25; Symbol = "Mn"; Name = "Manganese"; Mass = 54.938045; DisplayColor = "#9c7ac7"; ElectronConfiguration = "[Ar] 3d5 4s2"; Electronegativity = Some 1.55; ElectronAffinity = Some 0; Radius = Some 139; MinOxidation = -3; MaxOxidation = 7}
    Element.Fe,{Number = 26; Symbol = "Fe"; Name = "Iron"; Mass = 55.845; DisplayColor = "#e06633"; ElectronConfiguration = "[Ar] 3d6 4s2"; Electronegativity = Some 1.83; ElectronAffinity = Some -16; Radius = Some 125; MinOxidation = -2; MaxOxidation = 6}
    Element.Co,{Number = 27; Symbol = "Co"; Name = "Cobalt"; Mass = 58.933195; DisplayColor = "#f090a0"; ElectronConfiguration = "[Ar] 3d7 4s2"; Electronegativity = Some 1.88; ElectronAffinity = Some -64; Radius = Some 126; MinOxidation = -1; MaxOxidation = 5}
    Element.Ni,{Number = 28; Symbol = "Ni"; Name = "Nickel"; Mass = 58.6934; DisplayColor = "#50d050"; ElectronConfiguration = "[Ar] 3d8 4s2"; Electronegativity = Some 1.91; ElectronAffinity = Some -112; Radius = Some 121; MinOxidation = -1; MaxOxidation = 4}
    Element.Cu,{Number = 29; Symbol = "Cu"; Name = "Copper"; Mass = 63.546; DisplayColor = "#c88033"; ElectronConfiguration = "[Ar] 3d10 4s1"; Electronegativity = Some 1.9; ElectronAffinity = Some -118; Radius = Some 138; MinOxidation = 0; MaxOxidation = 4}
    Element.Zn,{Number = 30; Symbol = "Zn"; Name = "Zinc"; Mass = 65.38; DisplayColor = "#7d80b0"; ElectronConfiguration = "[Ar] 3d10 4s2"; Electronegativity = Some 1.65; ElectronAffinity = Some 0; Radius = Some 131; MinOxidation = 0; MaxOxidation = 2}
    Element.Ga,{Number = 31; Symbol = "Ga"; Name = "Gallium"; Mass = 69.723; DisplayColor = "#c28f8f"; ElectronConfiguration = "[Ar] 3d10 4s2 4p1"; Electronegativity = Some 1.81; ElectronAffinity = Some -29; Radius = Some 126; MinOxidation = 0; MaxOxidation = 3}
    Element.Ge,{Number = 32; Symbol = "Ge"; Name = "Germanium"; Mass = 72.64; DisplayColor = "#668f8f"; ElectronConfiguration = "[Ar] 3d10 4s2 4p2"; Electronegativity = Some 2.01; ElectronAffinity = Some -119; Radius = Some 122; MinOxidation = -4; MaxOxidation = 4}
    Element.As,{Number = 33; Symbol = "As"; Name = "Arsenic"; Mass = 74.9216; DisplayColor = "#bd80e3"; ElectronConfiguration = "[Ar] 3d10 4s2 4p3"; Electronegativity = Some 2.18; ElectronAffinity = Some -78; Radius = Some 119; MinOxidation = -3; MaxOxidation = 5}
    Element.Se,{Number = 34; Symbol = "Se"; Name = "Selenium"; Mass = 78.96; DisplayColor = "#ffa100"; ElectronConfiguration = "[Ar] 3d10 4s2 4p4"; Electronegativity = Some 2.55; ElectronAffinity = Some -195; Radius = Some 116; MinOxidation = -2; MaxOxidation = 6}
    Element.Br,{Number = 35; Symbol = "Br"; Name = "Bromine"; Mass = 79.904; DisplayColor = "#a62929"; ElectronConfiguration = "[Ar] 3d10 4s2 4p5"; Electronegativity = Some 2.96; ElectronAffinity = Some -325; Radius = Some 114; MinOxidation = -1; MaxOxidation = 7}
    Element.Kr,{Number = 36; Symbol = "Kr"; Name = "Krypton"; Mass = 83.798; DisplayColor = "#5cb8d1"; ElectronConfiguration = "[Ar] 3d10 4s2 4p6"; Electronegativity = None; ElectronAffinity = Some 0; Radius = Some 110; MinOxidation = 0; MaxOxidation = 2}
    Element.Rb,{Number = 37; Symbol = "Rb"; Name = "Rubidium"; Mass = 85.4678; DisplayColor = "#702eb0"; ElectronConfiguration = "[Kr] 5s1"; Electronegativity = Some 0.82; ElectronAffinity = Some -47; Radius = Some 211; MinOxidation = 0; MaxOxidation = 1}
    Element.Sr,{Number = 38; Symbol = "Sr"; Name = "Strontium"; Mass = 87.62; DisplayColor = "#00ff00"; ElectronConfiguration = "[Kr] 5s2"; Electronegativity = Some 0.95; ElectronAffinity = Some -5; Radius = Some 192; MinOxidation = 0; MaxOxidation = 2}
    Element.Y,{Number = 39; Symbol = "Y"; Name = "Yttrium"; Mass = 88.90585; DisplayColor = "#94ffff"; ElectronConfiguration = "[Kr] 4d1 5s2"; Electronegativity = Some 1.22; ElectronAffinity = Some -30; Radius = Some 162; MinOxidation = 0; MaxOxidation = 3}
    Element.Zr,{Number = 40; Symbol = "Zr"; Name = "Zirconium"; Mass = 91.224; DisplayColor = "#94e0e0"; ElectronConfiguration = "[Kr] 4d2 5s2"; Electronegativity = Some 1.33; ElectronAffinity = Some -41; Radius = Some 148; MinOxidation = 0; MaxOxidation = 4}
    Element.Nb,{Number = 41; Symbol = "Nb"; Name = "Niobium"; Mass = 92.90638; DisplayColor = "#73c2c9"; ElectronConfiguration = "[Kr] 4d4 5s1"; Electronegativity = Some 1.6; ElectronAffinity = Some -86; Radius = Some 137; MinOxidation = -1; MaxOxidation = 5}
    Element.Mo,{Number = 42; Symbol = "Mo"; Name = "Molybdenum"; Mass = 95.96; DisplayColor = "#54b5b5"; ElectronConfiguration = "[Kr] 4d5 5s1"; Electronegativity = Some 2.16; ElectronAffinity = Some -72; Radius = Some 145; MinOxidation = -2; MaxOxidation = 6}
    Element.Tc,{Number = 43; Symbol = "Tc"; Name = "Technetium"; Mass = 98; DisplayColor = "#3b9e9e"; ElectronConfiguration = "[Kr] 4d5 5s2"; Electronegativity = Some 1.9; ElectronAffinity = Some -53; Radius = Some 156; MinOxidation = -3; MaxOxidation = 7}
    Element.Ru,{Number = 44; Symbol = "Ru"; Name = "Ruthenium"; Mass = 101.07; DisplayColor = "#248f8f"; ElectronConfiguration = "[Kr] 4d7 5s1"; Electronegativity = Some 2.2; ElectronAffinity = Some -101; Radius = Some 126; MinOxidation = -2; MaxOxidation = 8}
    Element.Rh,{Number = 45; Symbol = "Rh"; Name = "Rhodium"; Mass = 102.9055; DisplayColor = "#0a7d8c"; ElectronConfiguration = "[Kr] 4d8 5s1"; Electronegativity = Some 2.28; ElectronAffinity = Some -110; Radius = Some 135; MinOxidation = -1; MaxOxidation = 6}
    Element.Pd,{Number = 46; Symbol = "Pd"; Name = "Palladium"; Mass = 106.42; DisplayColor = "#006985"; ElectronConfiguration = "[Kr] 4d10"; Electronegativity = Some 2.2; ElectronAffinity = Some -54; Radius = Some 131; MinOxidation = 0; MaxOxidation = 4}
    Element.Ag,{Number = 47; Symbol = "Ag"; Name = "Silver"; Mass = 107.8682; DisplayColor = "#c0c0c0"; ElectronConfiguration = "[Kr] 4d10 5s1"; Electronegativity = Some 1.93; ElectronAffinity = Some -126; Radius = Some 153; MinOxidation = 0; MaxOxidation = 3}
    Element.Cd,{Number = 48; Symbol = "Cd"; Name = "Cadmium"; Mass = 112.411; DisplayColor = "#ffd98f"; ElectronConfiguration = "[Kr] 4d10 5s2"; Electronegativity = Some 1.69; ElectronAffinity = Some 0; Radius = Some 148; MinOxidation = 0; MaxOxidation = 2}
    Element.In,{Number = 49; Symbol = "In"; Name = "Indium"; Mass = 114.818; DisplayColor = "#a67573"; ElectronConfiguration = "[Kr] 4d10 5s2 5p1"; Electronegativity = Some 1.78; ElectronAffinity = Some -29; Radius = Some 144; MinOxidation = 0; MaxOxidation = 3}
    Element.Sn,{Number = 50; Symbol = "Sn"; Name = "Tin"; Mass = 118.71; DisplayColor = "#668080"; ElectronConfiguration = "[Kr] 4d10 5s2 5p2"; Electronegativity = Some 1.96; ElectronAffinity = Some -107; Radius = Some 141; MinOxidation = -4; MaxOxidation = 4}
    Element.Sb,{Number = 51; Symbol = "Sb"; Name = "Antimony"; Mass = 121.76; DisplayColor = "#9e63b5"; ElectronConfiguration = "[Kr] 4d10 5s2 5p3"; Electronegativity = Some 2.05; ElectronAffinity = Some -103; Radius = Some 138; MinOxidation = -3; MaxOxidation = 5}
    Element.Te,{Number = 52; Symbol = "Te"; Name = "Tellurium"; Mass = 127.6; DisplayColor = "#d47a00"; ElectronConfiguration = "[Kr] 4d10 5s2 5p4"; Electronegativity = Some 2.1; ElectronAffinity = Some -190; Radius = Some 135; MinOxidation = -2; MaxOxidation = 6}
    Element.I,{Number = 53; Symbol = "I"; Name = "Iodine"; Mass = 126.90447; DisplayColor = "#940094"; ElectronConfiguration = "[Kr] 4d10 5s2 5p5"; Electronegativity = Some 2.66; ElectronAffinity = Some -295; Radius = Some 133; MinOxidation = -1; MaxOxidation = 7}
    Element.Xe,{Number = 54; Symbol = "Xe"; Name = "Xenon"; Mass = 131.293; DisplayColor = "#429eb0"; ElectronConfiguration = "[Kr] 4d10 5s2 5p6"; Electronegativity = None; ElectronAffinity = Some 0; Radius = Some 130; MinOxidation = 0; MaxOxidation = 8}
    Element.Cs,{Number = 55; Symbol = "Cs"; Name = "Cesium"; Mass = 132.9054519; DisplayColor = "#57178f"; ElectronConfiguration = "[Xe] 6s1"; Electronegativity = Some 0.79; ElectronAffinity = Some -46; Radius = Some 225; MinOxidation = 0; MaxOxidation = 1}
    Element.Ba,{Number = 56; Symbol = "Ba"; Name = "Barium"; Mass = 137.327; DisplayColor = "#00c900"; ElectronConfiguration = "[Xe] 6s2"; Electronegativity = Some 0.89; ElectronAffinity = Some -14; Radius = Some 198; MinOxidation = 0; MaxOxidation = 2}
    Element.La,{Number = 57; Symbol = "La"; Name = "Lanthanum"; Mass = 138.90547; DisplayColor = "#70d4ff"; ElectronConfiguration = "[Xe] 5d1 6s2"; Electronegativity = Some 1.1; ElectronAffinity = Some -48; Radius = Some 169; MinOxidation = 0; MaxOxidation = 3}
    Element.Ce,{Number = 58; Symbol = "Ce"; Name = "Cerium"; Mass = 140.116; DisplayColor = "#ffffc7"; ElectronConfiguration = "[Xe] 4f1 5d1 6s2"; Electronegativity = Some 1.12; ElectronAffinity = Some -50; Radius = None; MinOxidation = 0; MaxOxidation = 4}
    Element.Pr,{Number = 59; Symbol = "Pr"; Name = "Praseodymium"; Mass = 140.90765; DisplayColor = "#d9ffc7"; ElectronConfiguration = "[Xe] 4f3 6s2"; Electronegativity = Some 1.13; ElectronAffinity = Some -50; Radius = None; MinOxidation = 0; MaxOxidation = 4}
    Element.Nd,{Number = 60; Symbol = "Nd"; Name = "Neodymium"; Mass = 144.242; DisplayColor = "#c7ffc7"; ElectronConfiguration = "[Xe] 4f4 6s2"; Electronegativity = Some 1.14; ElectronAffinity = Some -50; Radius = None; MinOxidation = 0; MaxOxidation = 3}
    Element.Pm,{Number = 61; Symbol = "Pm"; Name = "Promethium"; Mass = 145; DisplayColor = "#a3ffc7"; ElectronConfiguration = "[Xe] 4f5 6s2"; Electronegativity = Some 1.13; ElectronAffinity = Some -50; Radius = None; MinOxidation = 0; MaxOxidation = 3}
    Element.Sm,{Number = 62; Symbol = "Sm"; Name = "Samarium"; Mass = 150.36; DisplayColor = "#8fffc7"; ElectronConfiguration = "[Xe] 4f6 6s2"; Electronegativity = Some 1.17; ElectronAffinity = Some -50; Radius = None; MinOxidation = 0; MaxOxidation = 3}
    Element.Eu,{Number = 63; Symbol = "Eu"; Name = "Europium"; Mass = 151.964; DisplayColor = "#61ffc7"; ElectronConfiguration = "[Xe] 4f7 6s2"; Electronegativity = Some 1.2; ElectronAffinity = Some -50; Radius = None; MinOxidation = 0; MaxOxidation = 3}
    Element.Gd,{Number = 64; Symbol = "Gd"; Name = "Gadolinium"; Mass = 157.25; DisplayColor = "#45ffc7"; ElectronConfiguration = "[Xe] 4f7 5d1 6s2"; Electronegativity = Some 1.2; ElectronAffinity = Some -50; Radius = None; MinOxidation = 0; MaxOxidation = 3}
    Element.Tb,{Number = 65; Symbol = "Tb"; Name = "Terbium"; Mass = 158.92535; DisplayColor = "#30ffc7"; ElectronConfiguration = "[Xe] 4f9 6s2"; Electronegativity = Some 1.2; ElectronAffinity = Some -50; Radius = None; MinOxidation = 0; MaxOxidation = 4}
    Element.Dy,{Number = 66; Symbol = "Dy"; Name = "Dysprosium"; Mass = 162.5; DisplayColor = "#1fffc7"; ElectronConfiguration = "[Xe] 4f10 6s2"; Electronegativity = Some 1.22; ElectronAffinity = Some -50; Radius = None; MinOxidation = 0; MaxOxidation = 3}
    Element.Ho,{Number = 67; Symbol = "Ho"; Name = "Holmium"; Mass = 164.93032; DisplayColor = "#00ff9c"; ElectronConfiguration = "[Xe] 4f11 6s2"; Electronegativity = Some 1.23; ElectronAffinity = Some -50; Radius = None; MinOxidation = 0; MaxOxidation = 3}
    Element.Er,{Number = 68; Symbol = "Er"; Name = "Erbium"; Mass = 167.259; DisplayColor = "#00e675"; ElectronConfiguration = "[Xe] 4f12 6s2"; Electronegativity = Some 1.24; ElectronAffinity = Some -50; Radius = None; MinOxidation = 0; MaxOxidation = 3}
    Element.Tm,{Number = 69; Symbol = "Tm"; Name = "Thulium"; Mass = 168.93421; DisplayColor = "#00d452"; ElectronConfiguration = "[Xe] 4f13 6s2"; Electronegativity = Some 1.25; ElectronAffinity = Some -50; Radius = None; MinOxidation = 0; MaxOxidation = 3}
    Element.Yb,{Number = 70; Symbol = "Yb"; Name = "Ytterbium"; Mass = 173.054; DisplayColor = "#00bf38"; ElectronConfiguration = "[Xe] 4f14 6s2"; Electronegativity = Some 1.1; ElectronAffinity = Some -50; Radius = None; MinOxidation = 0; MaxOxidation = 3}
    Element.Lu,{Number = 71; Symbol = "Lu"; Name = "Lutetium"; Mass = 174.9668; DisplayColor = "#00ab24"; ElectronConfiguration = "[Xe] 4f14 5d1 6s2"; Electronegativity = Some 1.27; ElectronAffinity = Some -50; Radius = Some 160; MinOxidation = 0; MaxOxidation = 3}
    Element.Hf,{Number = 72; Symbol = "Hf"; Name = "Hafnium"; Mass = 178.49; DisplayColor = "#4dc2ff"; ElectronConfiguration = "[Xe] 4f14 5d2 6s2"; Electronegativity = Some 1.3; ElectronAffinity = Some 0; Radius = Some 150; MinOxidation = 0; MaxOxidation = 4}
    Element.Ta,{Number = 73; Symbol = "Ta"; Name = "Tantalum"; Mass = 180.94788; DisplayColor = "#4da6ff"; ElectronConfiguration = "[Xe] 4f14 5d3 6s2"; Electronegativity = Some 1.5; ElectronAffinity = Some -31; Radius = Some 138; MinOxidation = -1; MaxOxidation = 5}
    Element.W,{Number = 74; Symbol = "W"; Name = "Tungsten"; Mass = 183.84; DisplayColor = "#2194d6"; ElectronConfiguration = "[Xe] 4f14 5d4 6s2"; Electronegativity = Some 2.36; ElectronAffinity = Some -79; Radius = Some 146; MinOxidation = -2; MaxOxidation = 6}
    Element.Re,{Number = 75; Symbol = "Re"; Name = "Rhenium"; Mass = 186.207; DisplayColor = "#267dab"; ElectronConfiguration = "[Xe] 4f14 5d5 6s2"; Electronegativity = Some 1.9; ElectronAffinity = Some -15; Radius = Some 159; MinOxidation = -3; MaxOxidation = 7}
    Element.Os,{Number = 76; Symbol = "Os"; Name = "Osmium"; Mass = 190.23; DisplayColor = "#266696"; ElectronConfiguration = "[Xe] 4f14 5d6 6s2"; Electronegativity = Some 2.2; ElectronAffinity = Some -106; Radius = Some 128; MinOxidation = -2; MaxOxidation = 8}
    Element.Ir,{Number = 77; Symbol = "Ir"; Name = "Iridium"; Mass = 192.217; DisplayColor = "#175487"; ElectronConfiguration = "[Xe] 4f14 5d7 6s2"; Electronegativity = Some 2.2; ElectronAffinity = Some -151; Radius = Some 137; MinOxidation = -3; MaxOxidation = 6}
    Element.Pt,{Number = 78; Symbol = "Pt"; Name = "Platinum"; Mass = 195.084; DisplayColor = "#d0d0e0"; ElectronConfiguration = "[Xe] 4f14 5d9 6s1"; Electronegativity = Some 2.28; ElectronAffinity = Some -205; Radius = Some 128; MinOxidation = 0; MaxOxidation = 6}
    Element.Au,{Number = 79; Symbol = "Au"; Name = "Gold"; Mass = 196.966569; DisplayColor = "#ffd123"; ElectronConfiguration = "[Xe] 4f14 5d10 6s1"; Electronegativity = Some 2.54; ElectronAffinity = Some -223; Radius = Some 144; MinOxidation = -1; MaxOxidation = 5}
    Element.Hg,{Number = 80; Symbol = "Hg"; Name = "Mercury"; Mass = 200.59; DisplayColor = "#b8b8d0"; ElectronConfiguration = "[Xe] 4f14 5d10 6s2"; Electronegativity = Some 2; ElectronAffinity = Some 0; Radius = Some 149; MinOxidation = 0; MaxOxidation = 4}
    Element.Tl,{Number = 81; Symbol = "Tl"; Name = "Thallium"; Mass = 204.3833; DisplayColor = "#a6544d"; ElectronConfiguration = "[Xe] 4f14 5d10 6s2 6p1"; Electronegativity = Some 2.04; ElectronAffinity = Some -19; Radius = Some 148; MinOxidation = 0; MaxOxidation = 3}
    Element.Pb,{Number = 82; Symbol = "Pb"; Name = "Lead"; Mass = 207.2; DisplayColor = "#575961"; ElectronConfiguration = "[Xe] 4f14 5d10 6s2 6p2"; Electronegativity = Some 2.33; ElectronAffinity = Some -35; Radius = Some 147; MinOxidation = -4; MaxOxidation = 4}
    Element.Bi,{Number = 83; Symbol = "Bi"; Name = "Bismuth"; Mass = 208.9804; DisplayColor = "#9e4fb5"; ElectronConfiguration = "[Xe] 4f14 5d10 6s2 6p3"; Electronegativity = Some 2.02; ElectronAffinity = Some -91; Radius = Some 146; MinOxidation = -3; MaxOxidation = 5}
    Element.Po,{Number = 84; Symbol = "Po"; Name = "Polonium"; Mass = 209; DisplayColor = "#ab5c00"; ElectronConfiguration = "[Xe] 4f14 5d10 6s2 6p4"; Electronegativity = Some 2; ElectronAffinity = Some -183; Radius = None; MinOxidation = -2; MaxOxidation = 6}
    Element.At,{Number = 85; Symbol = "At"; Name = "Astatine"; Mass = 210; DisplayColor = "#754f45"; ElectronConfiguration = "[Xe] 4f14 5d10 6s2 6p5"; Electronegativity = Some 2.2; ElectronAffinity = Some -270; Radius = None; MinOxidation = -1; MaxOxidation = 5}
    Element.Rn,{Number = 86; Symbol = "Rn"; Name = "Radon"; Mass = 222; DisplayColor = "#428296"; ElectronConfiguration = "[Xe] 4f14 5d10 6s2 6p6"; Electronegativity = None; ElectronAffinity = None; Radius = Some 145; MinOxidation = 0; MaxOxidation = 2}
    Element.Fr,{Number = 87; Symbol = "Fr"; Name = "Francium"; Mass = 223; DisplayColor = "#420066"; ElectronConfiguration = "[Rn] 7s1"; Electronegativity = Some 0.7; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 1}
    Element.Ra,{Number = 88; Symbol = "Ra"; Name = "Radium"; Mass = 226; DisplayColor = "#007d00"; ElectronConfiguration = "[Rn] 7s2"; Electronegativity = Some 0.9; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 2}
    Element.Ac,{Number = 89; Symbol = "Ac"; Name = "Actinium"; Mass = 227; DisplayColor = "#70abfa"; ElectronConfiguration = "[Rn] 6d1 7s2"; Electronegativity = Some 1.1; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 3}
    Element.Th,{Number = 90; Symbol = "Th"; Name = "Thorium"; Mass = 232.03806; DisplayColor = "#00baff"; ElectronConfiguration = "[Rn] 6d2 7s2"; Electronegativity = Some 1.3; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 4}
    Element.Pa,{Number = 91; Symbol = "Pa"; Name = "Protactinium"; Mass = 231.03588; DisplayColor = "#00a1ff"; ElectronConfiguration = "[Rn] 5f2 6d1 7s2"; Electronegativity = Some 1.5; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 5}
    Element.U,{Number = 92; Symbol = "U"; Name = "Uranium"; Mass = 238.02891; DisplayColor = "#008fff"; ElectronConfiguration = "[Rn] 5f3 6d1 7s2"; Electronegativity = Some 1.38; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 6}
    Element.Np,{Number = 93; Symbol = "Np"; Name = "Neptunium"; Mass = 237; DisplayColor = "#0080ff"; ElectronConfiguration = "[Rn] 5f4 6d1 7s2"; Electronegativity = Some 1.36; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 7}
    Element.Pu,{Number = 94; Symbol = "Pu"; Name = "Plutonium"; Mass = 244; DisplayColor = "#006bff"; ElectronConfiguration = "[Rn] 5f6 7s2"; Electronegativity = Some 1.28; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 7}
    Element.Am,{Number = 95; Symbol = "Am"; Name = "Americium"; Mass = 243; DisplayColor = "#545cf2"; ElectronConfiguration = "[Rn] 5f7 7s2"; Electronegativity = Some 1.3; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 6}
    Element.Cm,{Number = 96; Symbol = "Cm"; Name = "Curium"; Mass = 247; DisplayColor = "#785ce3"; ElectronConfiguration = "[Rn] 5f7 6d1 7s2"; Electronegativity = Some 1.3; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 4}
    Element.Bk,{Number = 97; Symbol = "Bk"; Name = "Berkelium"; Mass = 247; DisplayColor = "#8a4fe3"; ElectronConfiguration = "[Rn] 5f9 7s2"; Electronegativity = Some 1.3; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 4}
    Element.Cf,{Number = 98; Symbol = "Cf"; Name = "Californium"; Mass = 251; DisplayColor = "#a136d4"; ElectronConfiguration = "[Rn] 5f10 7s2"; Electronegativity = Some 1.3; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 4}
    Element.Es,{Number = 99; Symbol = "Es"; Name = "Einsteinium"; Mass = 252; DisplayColor = "#b31fd4"; ElectronConfiguration = "[Rn] 5f11 7s2"; Electronegativity = Some 1.3; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 3}
    Element.Fm,{Number = 100; Symbol = "Fm"; Name = "Fermium"; Mass = 257; DisplayColor = "#b31fba"; ElectronConfiguration = "[Rn] 5f12 7s2"; Electronegativity = Some 1.3; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 3}
    Element.Md,{Number = 101; Symbol = "Md"; Name = "Mendelevium"; Mass = 258; DisplayColor = "#b30da6"; ElectronConfiguration = "[Rn] 5f13 7s2"; Electronegativity = Some 1.3; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 3}
    Element.No,{Number = 102; Symbol = "No"; Name = "Nobelium"; Mass = 259; DisplayColor = "#bd0d87"; ElectronConfiguration = "[Rn] 5f14 7s2"; Electronegativity = Some 1.3; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 3}
    Element.Lr,{Number = 103; Symbol = "Lr"; Name = "Lawrencium"; Mass = 262; DisplayColor = "#c70066"; ElectronConfiguration = "[Rn] 5f14 7s2 7p1"; Electronegativity = Some 1.3; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 3}
    Element.Rf,{Number = 104; Symbol = "Rf"; Name = "Rutherfordium"; Mass = 267; DisplayColor = "#cc0059"; ElectronConfiguration = "[Rn] 5f14 6d2 7s2"; Electronegativity = None; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 4}
    Element.Db,{Number = 105; Symbol = "Db"; Name = "Dubnium"; Mass = 268; DisplayColor = "#d1004f"; ElectronConfiguration = "[Rn].5f14.6d3.7s2"; Electronegativity = None; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 0}
    Element.Sg,{Number = 106; Symbol = "Sg"; Name = "Seaborgium"; Mass = 271; DisplayColor = "#d90045"; ElectronConfiguration = "[Rn].5f14.6d4.7s2"; Electronegativity = None; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 0}
    Element.Bh,{Number = 107; Symbol = "Bh"; Name = "Bohrium"; Mass = 272; DisplayColor = "#e00038"; ElectronConfiguration = "[Rn].5f14.6d5.7s2"; Electronegativity = None; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 0}
    Element.Hs,{Number = 108; Symbol = "Hs"; Name = "Hassium"; Mass = 270; DisplayColor = "#e6002e"; ElectronConfiguration = "[Rn].5f14.6d6.7s2"; Electronegativity = None; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 0}
    Element.Mt,{Number = 109; Symbol = "Mt"; Name = "Meitnerium"; Mass = 276; DisplayColor = "#eb0026"; ElectronConfiguration = "[Rn].5f14.6d7.7s2"; Electronegativity = None; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 0}
    Element.Ds,{Number = 110; Symbol = "Ds"; Name = "Darmstadtium"; Mass = 281; DisplayColor = ""; ElectronConfiguration = "[Rn].5f14.6d9.7s1"; Electronegativity = None; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 0}
    Element.Rg,{Number = 111; Symbol = "Rg"; Name = "Roentgenium"; Mass = 280; DisplayColor = ""; ElectronConfiguration = "[Rn].5f14.6d10.7s1"; Electronegativity = None; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 0}
    Element.Cn,{Number = 112; Symbol = "Cn"; Name = "Copernicium"; Mass = 285; DisplayColor = ""; ElectronConfiguration = "[Rn].5f14.6d10.7s2"; Electronegativity = None; ElectronAffinity = None; Radius = None; MinOxidation = 0; MaxOxidation = 0}
]