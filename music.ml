open Sdlmixer

(* note is tuple list representing each music note and how long the note
   is played *)
type notes = (int * float) list

(* files is a tuple list for the corresponding sound files *)
type files = (string * float) list

(* inc take interger [v] and increases it by certain amount [amount] *)
let inc v amount = v + amount

(* update_points increases score[ points] based on user input [answer] *)
let update_points answer points =
  match answer with
  | "a" | "A" -> inc points 5
  | "b" | "B" -> inc points 10
  | "c" | "C" -> inc points 15
  | "d" | "D" -> inc points 20
  | "e" | "E" -> inc points 25
  | "f" | "F" -> inc points 30
  | _ -> points

(* get_results returns corresponding wav file based on users total score
   [points] *)
let get_results points =
  if points <= 70 then "D4.wav"
  else if points <= 105 then "F4.wav"
  else if points <= 140 then "G4.wav"
  else if points <= 175 then "A4.wav"
  else "B4.wav"

(* quiz recurses through quiz questions [questions] calles update points
   on points according to user input *)
let rec quiz questions points =
  match questions with
  | [] -> get_results points
  | h :: t -> (
      print_endline h;
      match read_line () with
      | exception End_of_file -> ""
      | answer ->
          let new_score = update_points answer points in
          if points = new_score then (
            ANSITerminal.print_string [ ANSITerminal.red ]
              "Your answer is not valid please answer the questions \
               based on it's corresponding letter \n";
            quiz questions points)
          else quiz t new_score)

(*tone matches given frequency [freq] to Corresponding wav file *)
let tone freq =
  if freq < 286 then "D4.wav"
  else if freq >= 286 && freq <= 302 then "D4.wav"
  else if freq >= 303 && freq <= 320 then "D-sharp4.wav"
  else if freq >= 321 && freq <= 339 then "E4.wav"
  else if freq >= 340 && freq <= 359 then "F4.wav"
  else if freq >= 360 && freq <= 380 then "F-sharp4.wav"
  else if freq >= 381 && freq <= 403 then "G4.wav"
  else if freq >= 404 && freq <= 427 then "G-sharp4.wav"
  else if freq >= 428 && freq <= 452 then "A4.wav"
  else if freq >= 453 && freq <= 479 then "A-sharp4.wav"
  else if freq >= 480 && freq <= 508 then "B4.wav"
  else if freq >= 509 && freq <= 538 then "C5.wav"
  else if freq >= 539 && freq <= 570 then "C-sharp5.wav"
  else if freq >= 571 && freq <= 604 then "D5.wav"
  else "D-sharp5.wav"

let lower freq = float_of_int freq *. ((2. ** (1. /. 12.)) ** -1.)

let rec lower_notes
    (lst : (int * float) list)
    (acc : (int * float) list) : (int * float) list =
  match lst with
  | [] -> acc
  | (f, d) :: t ->
      let new_f = int_of_float (lower f) in
      lower_notes t ((new_f, d) :: acc)

let higher freq = float_of_int freq *. ((2. ** (1. /. 12.)) ** 1.)

let rec higher_notes
    (lst : (int * float) list)
    (acc : (int * float) list) : (int * float) list =
  match lst with
  | [] -> acc
  | (f, d) :: t ->
      let new_f = int_of_float (higher f) in
      higher_notes t ((new_f, d) :: acc)

let lower_octave freq = freq / 2

let lower_octave_notes n = List.map lower_octave n

let higher_octave freq = freq * 2

let higher_octave_notes n = List.map higher_octave n

let rec make_song n =
  match n with
  | [] -> []
  | h :: t ->
      let tune =
        [ h; int_of_float (lower h); h; int_of_float (higher h) ]
      in
      tune :: make_song t

let song n = List.flatten (make_song n)

(* file_converter takes in notes [n] and generates corresponding files
   list need to play the song *)
let rec file_converter n =
  match n with
  | [] -> []
  | (a, b) :: t -> (tone a, b) :: file_converter t

(* play_notes take in files [f] created by file_converter to plays each
   note in order for the length of the second tuple value *)
let rec play_notes f =
  match f with
  | [] -> ()
  | (a, b) :: t ->
      (* let x = Sdlmixer.allocate_channels 10000 in print_endline
         (string_of_int x); *)
      Sdlmixer.play_channel (Sdlmixer.loadWAV a);
      Unix.sleepf b;
      play_notes t
