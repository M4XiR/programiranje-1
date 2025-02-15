(*----------------------------------------------------------------------------*
 # 4. domača naloga
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Pri tej nalogi boste napisali svoj simulator Turingovih strojev. Zaradi
 preprostosti bomo za abecedo vzeli kar znake tipa `char`, za prazni znak bomo
 izbrali presledek `' '`, stanja pa bomo predstavili z nizi. Za možne premike
 zafiksiramo tip `direction`:
[*----------------------------------------------------------------------------*)

type direction = Left | Right
type state = string

(*----------------------------------------------------------------------------*
 ## Implementacija trakov
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite modul `Tape`, ki implementira spodnjo signaturo, kjer je:

 - `t` tip v obe smeri neomejenih trakov in glavo na danem mestu;
 - `make`, ki naredi nov trak z znaki iz niza ter glavo na prvem znaku;
 - `print`, ki izpiše vsebino traku (brez presledkov na začetku in koncu) ter
 pod njim z `^` označi mesto glave;
 - `read`, ki vrne znak pod glavo;
 - `write`, ki pod glavo zapiše dani znak;
 - `move`, ki glavo premakne v dano smer.

 Zadnji dve funkciji naj vrneta nov trak, obstoječega pa naj pustita
 nespremenjenega.

 Ker je tip `t` abstrakten, si lahko privoščite poljubno implementacijo, zato
 poskrbite tako za učinkovitost kot za preglednost kode.
[*----------------------------------------------------------------------------*)

module type TAPE = sig
  type t

  val make : string -> t
  val print : t -> unit
  val read : t -> char
  val move : direction -> t -> t
  val write : char -> t -> t
end

let string_to_list str = List.init (String.length str) (String.get str)

let test1 = string_to_list "brazilija"

module Tape : TAPE = struct
  type t = {levo : char list; glava : char; desno : char list}
  
  

  let make (str : string) = 
    let sez = string_to_list str in 
    match  sez with
  | [] -> {levo = []; glava = ' '; desno = []}
  | x::xs -> {levo = []; glava = x; desno = xs}

  let move (smer : direction) (tape : t) = 
    match smer with
    |Left -> 
      (match tape.levo with
      | [] -> {levo = []; glava= ' '; desno = tape.glava :: tape.desno}
      | x::xs -> {levo = xs ; glava = x; desno = tape.glava :: tape.desno})
      
    |Right -> 
      (match tape.desno with
      |[] -> {levo = tape.glava :: tape.levo; glava = ' '; desno = []}
      |x::xs -> {levo = tape.glava :: tape.levo; glava = x; desno = xs})
      
  let read tape = tape.glava

  let write char tape = {tape with glava = char}

  let print tape =
    let rec odstrani_zacetne_presledke = function
      | ' ' :: xs -> odstrani_zacetne_presledke xs
      | xs -> xs
    in
    let levi_sez = odstrani_zacetne_presledke (List.rev tape.levo) in
    let celoten_sez = levi_sez @ [tape.glava] @ tape.desno in
    let sez_to_str = String.concat "" (List.map (String.make 1) celoten_sez) in
    let pozicija_glave = String.length (String.concat "" (List.map (String.make 1) levi_sez)) in
    let kazalec =  String.make pozicija_glave ' ' ^ "^" in
    print_endline sez_to_str; 
    print_endline kazalec
end


(*let primer_trak = Tape.(
  make "ABCDE"
  |> move Left
  |> move Left
  |> move Right
  |> move Right
  |> move Right
  |> move Right
  |> write '!'
  |> print 
)*)
(*
AB!DE
  ^
*)
(* val primer_trak : unit = () *)

(*----------------------------------------------------------------------------*
 ## Implementacija Turingovih strojev
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite modul `Machine`, ki implementira spodnjo signaturo, kjer je:

 - `t` tip Turingovih strojev;
 - `make`, ki naredi nov stroj z danim začetnim stanjem in seznamom preostalih
 stanj ter prazno prehodno funkcijo;
 - `initial`, ki vrne začetno stanje stroja;
 - `add_transition`, ki prehodno funkcijo razširi s prehodom $(q, a) \mapsto
 (q', a', d)$;
 - `step`, ki za dano stanje in trak izvede en korak stroja, če je to mogoče.

 Zadnji dve funkciji naj vrneta spremenjene vrednosti, obstoječe argumente pa
 naj pustita nespremenjene. Prav tako pri zadnjih dveh funkcijah lahko
 predpostavite, da ju bomo klicali le na poprej podanih stanjih.

 Tudi tu je tip `t` abstrakten, zato poskrbite za učinkovitost in preglednost
 kode.
[*----------------------------------------------------------------------------*)

module type MACHINE = sig
  type t
  val make : state -> state list -> t
  val initial : t -> state
  val add_transition : state -> char -> state -> char -> direction -> t -> t
  val step : t -> state -> Tape.t -> (state * Tape.t) option
end

module Machine : MACHINE = struct
  module TransitionMap = Map.Make(struct
    type t = state * char
    let compare = compare
  end)

  type t = {
    initial_state : state;
    states : state list;
    transitions : (state * char * direction) TransitionMap.t;
  }

  let make initial_state states =
    { initial_state; states; transitions = TransitionMap.empty }

  let initial mch = mch.initial_state

  let add_transition trenutno_stanje ch naslednje_stanje napisi_ch smer mch =
    let key = (trenutno_stanje, ch) in
    let value = (naslednje_stanje, napisi_ch, smer) in
    { mch with transitions = TransitionMap.add key value mch.transitions }

  let step mch trenutno_stanje tape =
    let trenutni_char = Tape.read tape in
    let key = (trenutno_stanje, trenutni_char) in
    match TransitionMap.find_opt key mch.transitions with
    | None -> None
    | Some (naslednje_stanje, napisi_ch, smer) ->
        let nov_trak = tape |> Tape.write napisi_ch |> Tape.move smer in
        Some (naslednje_stanje, nov_trak)
end



(*----------------------------------------------------------------------------*
 Primer stroja "Binary Increment" na <http://turingmachine.io> lahko
 implementiramo kot:
[*----------------------------------------------------------------------------*)

let binary_increment =
  Machine.(
    make "right" [ "carry"; "done" ]
    |> add_transition "right" '1' "right" '1' Right
    |> add_transition "right" '0' "right" '0' Right
    |> add_transition "right" ' ' "carry" ' ' Left
    |> add_transition "carry" '1' "carry" '0' Left
    |> add_transition "carry" '0' "done" '1' Left
    |> add_transition "carry" ' ' "done" '1' Left
  )

(* val binary_increment : Machine.t = <abstr> *)

(*----------------------------------------------------------------------------*
 Zapišite funkciji `slow_run` in `speed_run` tipa `Machine.t -> str -> unit`, ki
 simulirata Turingov stroj na traku, na katerem je na začetku zapisan dani niz.
 Prva naj izpiše trakove in stanja pri vseh vmesnih korakih, druga pa naj izpiše
 le končni trak. Slednjo bomo uporabljali tudi pri meritvi učinkovitosti
 izvajanja.
[*----------------------------------------------------------------------------*)

let slow_run mch vhod =
  let rec aux state tape =
    Tape.print tape;
    print_endline state;
    match Machine.step mch state tape with
    | None -> () 
    | Some (naslednje_stanje, nov_trak) -> aux naslednje_stanje nov_trak
  in
  let tape = Tape.make vhod in
  aux (Machine.initial mch) tape





(*let primer_slow_run =
  slow_run binary_increment "1011"*)
(*
1011
^
right
1011
  ^
right
1011
  ^
right
1011
    ^
right
1011
    ^
right
1011
    ^
carry
1010
  ^
carry
1000
  ^
carry
1100
^
done
*)
(* val primer_slow_run : unit = () *)

let speed_run mch vhod =
  let rec aux state tape =
    match Machine.step mch state tape with
    | None -> tape 
    | Some (naslednje_stanje, nov_trak) -> aux naslednje_stanje nov_trak
  in
  let tape = Tape.make vhod in
  let koncni_trak = aux (Machine.initial mch) tape in
  Tape.print koncni_trak


(*let primer_speed_run =
  speed_run binary_increment "1011"*)
(*
1100
^
*)
(* val primer_speed_run : unit = () *)

(*----------------------------------------------------------------------------*
 ## Krajši zapis
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Ko definiramo Turingov stroj, prehode običajno združujemo najprej po stanjih,
 nato pa še po znakih. Prav tako pri dosti prehodih samo premikamo glavo, trak
 in stanje pa pustimo pri miru. Zapišite funkcije:

 - `for_state`
 - `for_character`
 - `for_characters`
 - `move`
 - `switch_and_move`
 - `write_and_move`
 - `write_switch_and_move`

 s katerimi bi lahko zgornji primer na krajše zapisali kot spodaj.
 Implementacijo in tipe ugotovite sami.
[*----------------------------------------------------------------------------*)
let for_state state sez_funkcij mch = 
  let rec uporabi_funkcijo m = 
    function
    | [] -> m
    | funkcija :: xs -> uporabi_funkcijo (funkcija state m) xs
  in
  uporabi_funkcijo mch (List.flatten sez_funkcij)

let for_character chr action = 
  [action chr]

let for_characters chrs action =
  let rec aux index acc =
    if index < 0 then acc
    else
      let chr = String.get chrs index in
      aux (index - 1) ((action chr) :: acc)
  in
  aux (String.length chrs - 1) []

let move smer chr stanje = 
  fun mch -> Machine.add_transition stanje chr stanje chr smer mch

let switch_and_move novo_stanje smer chr trenutno_stanje = 
  fun mch -> Machine.add_transition trenutno_stanje chr novo_stanje chr smer mch

let write_and_move chr_za_zapis smer chr state = 
  fun mch -> Machine.add_transition state chr state chr_za_zapis smer mch

let write_switch_and_move chr_za_zapis novo_stanje smer chr trenutno_stanje = 
  fun mch -> Machine.add_transition trenutno_stanje chr novo_stanje chr_za_zapis smer mch

let binary_increment' =
  Machine.make "right" ["carry"; "done"]
  |> for_state "right" [
    for_characters "01" @@ move Right;
    for_character ' ' @@ switch_and_move "carry" Left
  ]
  |> for_state "carry" [
    for_character '1' @@ write_and_move '0' Left;
    for_characters "0 " @@ write_switch_and_move '1' "done" Left
  ]   
(* val binary_increment' : Machine.t = <abstr> *)

(*----------------------------------------------------------------------------*
 ## Primeri Turingovih strojev
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Pri tej nalogi boste sestavljali stroje, ki bodo iz začetnega niza na traku na
 različne načine izračunali nov niz. Pri tem lahko predpostavite, da je začetni
 niz sestavljen iz ničel in enic, preostanek traku pa je prazen. Na koncu
 izvajanja naj bo glava na začetku novega niza, z izjemo tega niza pa naj bo
 trak prazen. Ni pa treba, da se izračunani niz začne na istem mestu na traku,
 kot se je začel prvotni niz.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ### Obračanje niza
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite Turingov stroj, ki začetni niz obrne na glavo.
[*----------------------------------------------------------------------------*)
let reverse =
  Machine.make "primi_in_pojdi_desno" ["desno_do_pravega_znaka0";"desno_do_pravega_znaka1"; "levo_do_pravega_znaka0";
   "obrnil_sem_levo_in_napisal_0"; 
  "obrnil_sem_levo_in_napisal_1"; "obrnil_sem_desno_in_napisal_0"; "obrnil_sem_desno_in_napisal_1" ;
  "do_konca_v_desno_da_popravim";"levo_do_pravega_znaka1";"popravi";"done"]

    |> for_state "primi_in_pojdi_desno" [
         for_character '0' @@ switch_and_move "desno_do_pravega_znaka0" Right;
         for_character '1' @@ switch_and_move "desno_do_pravega_znaka1" Right;
         for_characters "#*" @@ switch_and_move "do_konca_v_desno_da_popravim" Right;
       ]
    |> for_state "desno_do_pravega_znaka0" [
         for_characters "01" @@ move Right;
         for_characters "#* " @@ switch_and_move "obrnil_sem_levo_in_napisal_0" Left
       ]
    |> for_state "desno_do_pravega_znaka1" [
         for_characters "01" @@ move Right;
         for_characters "#* " @@ switch_and_move "obrnil_sem_levo_in_napisal_1" Left
       ]
    |> for_state "levo_do_pravega_znaka0" [
         for_characters "01" @@ move Left;
         for_characters "#* " @@ switch_and_move "obrnil_sem_desno_in_napisal_0" Right
       ]
    |> for_state "levo_do_pravega_znaka1" [
         for_characters "01" @@ move Left;
         for_characters "#* " @@ switch_and_move "obrnil_sem_desno_in_napisal_1" Right
       ]
    |> for_state "obrnil_sem_levo_in_napisal_0" [
         for_character '1' @@ write_switch_and_move '#' "levo_do_pravega_znaka1" Left;
         for_character '0' @@ write_switch_and_move '#' "levo_do_pravega_znaka0" Left;
         for_characters "#*" @@ switch_and_move "do_konca_v_desno_da_popravim" Right;
       ]
    |> for_state "obrnil_sem_levo_in_napisal_1" [
         for_character '1' @@ write_switch_and_move '*' "levo_do_pravega_znaka1" Left;
         for_character '0' @@ write_switch_and_move '*' "levo_do_pravega_znaka0" Left;
         for_characters "#*" @@ switch_and_move "do_konca_v_desno_da_popravim" Right
       ]
    |> for_state "obrnil_sem_desno_in_napisal_0" [
         for_characters "10" @@ write_switch_and_move '#' "primi_in_pojdi_desno" Right;
         for_characters "#*" @@ switch_and_move "do_konca_v_desno_da_popravim" Right;
       ]
    |> for_state "obrnil_sem_desno_in_napisal_1" [
         for_characters "10" @@ write_switch_and_move '*' "primi_in_pojdi_desno" Right;
         for_characters "#*" @@ switch_and_move "do_konca_v_desno_da_popravim" Right
       ]
    |> for_state "do_konca_v_desno_da_popravim" [
         for_characters "01#*" @@ move Right;
         for_character ' ' @@ switch_and_move "popravi" Left
       ]
    |> for_state "popravi" [
         for_character '#' @@ write_and_move '0' Left;
         for_character '*' @@ write_and_move '1' Left;
         for_character ' ' @@ switch_and_move "done" Right
       ]
  

(* 
let primer_reverse = speed_run reverse "11001100000110011011"

1001110000          
^
*)
(* val primer_reverse : unit = () *)

(*----------------------------------------------------------------------------*
 ### Podvajanje niza
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite Turingov stroj, ki podvoji začetni niz.
[*----------------------------------------------------------------------------*)

let duplicate = 
  Machine.make "desno_do_konca" ["premakni_znak_ena_desno";"desno_kopiram_1";"desno_kopiram_0";"preskoci_1_levo";"zacni_znova";"podvojil_sem_1";"podvojil_sem_0";"popravi";"done"]

 |> for_state "desno_do_konca" [
      for_character ' ' @@ switch_and_move "premakni_znak_ena_desno" Left;
      for_characters "01" @@ move Right;
    ]
 |> for_state "premakni_znak_ena_desno" [
      for_character '1' @@ switch_and_move "desno_kopiram_1" Right;
      for_character '0' @@ switch_and_move "desno_kopiram_0" Right;
      for_characters " #*" @@ switch_and_move "zacni_znova" Right;
    ]
 |> for_state "desno_kopiram_1" [
      for_characters "01 " @@ write_switch_and_move '1' "preskoci_1_levo" Left;
    ]
 |> for_state "desno_kopiram_0" [
      for_characters "01 " @@ write_switch_and_move '0' "preskoci_1_levo" Left;
    ]
 |> for_state "preskoci_1_levo" [
      for_characters "01 " @@ switch_and_move "premakni_znak_ena_desno" Left;
    ]
 |> for_state "zacni_znova" [
      for_character '1' @@ switch_and_move "podvojil_sem_1" Right;
      for_character '0' @@ switch_and_move "podvojil_sem_0" Right;
      for_character ' ' @@ switch_and_move "popravi" Left;
    ]
 |> for_state "podvojil_sem_1" [
      for_characters "1" @@ write_switch_and_move '#' "desno_do_konca" Right;
    ]
 |> for_state "podvojil_sem_0" [
      for_characters "0" @@ write_switch_and_move '*' "desno_do_konca" Right;
    ]
 |> for_state "popravi" [
      for_character '#' @@ write_and_move '1' Left;
      for_character '*' @@ write_and_move '0' Left;
      for_characters "01" @@ move Left;
      for_character ' ' @@ switch_and_move "done" Right
    ]
(*
let primer_duplicate = speed_run duplicate "010011"

001100001111       
^
*)
(* val primer_duplicate : unit = () *)

(*----------------------------------------------------------------------------*
 ### Eniški zapis
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite Turingov stroj, ki na začetku na traku sprejme število $n$, zapisano
 v dvojiškem zapisu, na koncu pa naj bo na traku zapisanih natanko $n$ enic.
[*----------------------------------------------------------------------------*)

let to_unary = 
  Machine.make "desno_do_konca" ["iščem_1_v_levo"; "nesem_1_v_desno"; "poisci_prazno_in_zapisi"; "pocisti"; "levo_do_#" ;"done"]

|> for_state "desno_do_konca" [
     for_character ' ' @@ write_switch_and_move '#' "iščem_1_v_levo" Left;
     for_characters "01" @@ move Right;
   ]
|> for_state "iščem_1_v_levo" [
     for_character '1' @@ write_switch_and_move '0' "nesem_1_v_desno" Right;
     for_character '0' @@ move Left;
     for_character ' ' @@ switch_and_move "pocisti" Right;
   ]
|> for_state "nesem_1_v_desno" [
     for_characters "0" @@ write_and_move '1' Right;
     for_character '#' @@ switch_and_move "poisci_prazno_in_zapisi" Right
   ]
|> for_state "poisci_prazno_in_zapisi" [
     for_character '1' @@ move Right;
     for_character ' ' @@ write_switch_and_move '1' "levo_do_#" Left;
   ]
|> for_state "levo_do_#" [
    for_character '1' @@ move Left;
    for_character '#' @@ switch_and_move "iščem_1_v_levo" Left
   ]
|> for_state "pocisti" [
     for_characters "10" @@ write_and_move ' ' Right;
     for_character '#' @@ write_switch_and_move ' ' "done" Right;
   ]

(* 
let primer_to_unary = speed_run to_unary "1010"

1111111111
^
*)
(* val primer_to_unary : unit = () *)

(*----------------------------------------------------------------------------*
 ### Dvojiški zapis
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite ravno obratni Turingov stroj, torej tak, ki na začetku na traku
 sprejme število $n$ enic, na koncu pa naj bo na traku zapisano število $n$ v
 dvojiškem zapisu.
[*----------------------------------------------------------------------------*)

let to_binary = 
  Machine.make "levo_napisem_#" ["pisem_#"; "desno_do_konca"; "obrnem_in_vzamem_1"; "nesem_1_v_levo"; "povecaj_stevec" ;"desno_do_#";"ali_je_konec";"pobrisi_#";"levo_do_konca" ;"done"]
|> for_state "levo_napisem_#" [
    for_character '1' @@ switch_and_move "pisem_#" Left;
   ]
|> for_state "pisem_#" [
    for_character ' ' @@ write_switch_and_move '#' "desno_do_konca" Right;
   ]

|> for_state "desno_do_konca" [
     for_character ' ' @@ switch_and_move "obrnem_in_vzamem_1" Left;
     for_character '1' @@ move Right;
   ]
|> for_state "obrnem_in_vzamem_1" [
     for_character '1' @@ write_switch_and_move ' ' "nesem_1_v_levo" Left;
     for_character '#' @@ write_switch_and_move ' ' "levo_do_konca" Left;
   ]
|> for_state "nesem_1_v_levo" [
     for_character '#' @@ switch_and_move "povecaj_stevec" Left;
     for_character '1' @@ move Left
   ]
|> for_state "povecaj_stevec" [
     for_character '1' @@ write_and_move '0' Left;
     for_characters " 0" @@ write_switch_and_move '1' "desno_do_#" Right
     ;
   ]
|> for_state "desno_do_#" [
    for_characters "01" @@ move Right;
    for_character '#' @@ switch_and_move "ali_je_konec" Right
   ]
|> for_state "ali_je_konec" [
     for_character '1' @@ switch_and_move "desno_do_konca" Right;
     for_character ' ' @@ switch_and_move  "pobrisi_#" Left;
   ]
|> for_state "pobrisi_#" [
    for_character '#' @@ write_switch_and_move ' ' "levo_do_konca" Left;
  ]
|> for_state "levo_do_konca" [
    for_characters "01" @@ move Left;
    for_character ' ' @@ switch_and_move "done" Right
  ]
  

 let primer_to_binary = slow_run to_binary (String.make 42 '1')
(*
101010                                           
^
*)
(* val primer_to_binary : unit = () *)
