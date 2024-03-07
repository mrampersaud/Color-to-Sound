open Sdlmixer

let pp_string s = "\"" ^ s ^ "\""

let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ ", ") t'
    in
    loop 0 "" lst
  in
  "" ^ pp_elts lst ^ ""

let questions =
  [
    "What is your most preferred music genre? \n\
    \ a) classical  \n\
    \ b) country \n\
    \ c) r&b/soul  \n\
    \ d) rock  \n\
    \ e) hip hop \n\
    \ f) pop ";
    "What is your favorite weather?  \n\
    \ a) summer \n\
    \ b) spring  \n\
    \ c) winter \n\
    \ d) fall \n\
    \ e) Who thinks about the weather? \n\
    \ f) I love all of them!";
    "what is your age?  \n\
    \ a) 65 or older \n\
    \ b) 64-45 \n\
    \ c) 35-44 \n\
    \ d) 25-34 \n\
    \ e) 18-24 \n\
    \ f) 18 or under";
    "Which color do you like the best?\n\
    \  a) green \n\
    \  b) blue  \n\
    \  c) yellow/gold  \n\
    \  d) purple  \n\
    \  e) red \n\
    \  f) something else!";
    "Which vacation is the best to you?  \n\
    \ a) The Caribbean \n\
    \ b) Egypt \n\
    \ c) Australia \n\
    \ d) France \n\
    \ e) Japan \n\
    \ f) my bed!";
    "Which is your favorite beverage? \n\
    \ a) water \n\
    \ b) hot tea/ hot coffee   \n\
    \ c) beer/ Alcoholic bevs  \n\
    \ d) ice tea/ ice coffee  \n\
    \ e) soda \n\
    \ f) juice";
    "Who is your favorite pop artist (even if you favorite genre isnt \
     pop) \n\
    \ a) Whitney Houston\n\
    \ b) Michael Jackson\n\
    \ c) pop music isnt real music! \n\
    \ d) Madonna \n\
    \ e) The weekend \n\
    \ f) Ariana Grande";
  ]

let rec main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the Image to Song Engine. \n";
  print_endline
    "What do you want to do today? press A to check out our Image to \
     Song Engine or press B to play our Music Notes Quiz or press C to \
     quit";
  match read_line () with
  | exception End_of_file -> ()
  | answer -> (
      match answer with
      | "a" | "A" -> (
          print_endline
            "Please enter the name of the PNG image file you want to \
             load.\n";
          print_string "> ";
          (* try *)
          match read_line () with
          | exception End_of_file -> ()
          | file_name -> transform file_name
          (* with _ -> print_endline "The file you used is not
             supported. Please use another \ PNG file"; *)
          (* main () *))
      | "b" | "B" ->
          let note = Music.quiz questions 0 in
          Sdlmixer.open_audio ();
          print_endline
            ("Your personal musical note is: " ^ String.sub note 0 2);
          Music.play_notes [ (note, 1.0) ]
      | "c" | "C" ->
          print_endline "Thanks for stopping by! Have a great day!";
          Sdlmixer.close_audio ();
          ()
      | _ ->
          print_endline " You did not select option A or B";
          main ())

and play_notes sample_notes =
  let file_list = Music.file_converter sample_notes in
  Sdlmixer.open_audio ();
  print_endline "Here is the song that matches this image";
  Music.play_notes file_list;
  mini_menu sample_notes

and mini_menu sample_notes =
  print_endline
    "What do you want to do next? \n\
    \ To play the song again type \n\
    \ 'play again' \n\
    \ To play the song in a shorter form type \n\
    \ 'play shorter' \n\
    \ To play the song with lower pitches type \n\
     \'play lower' \n\
    \ To play the song with higher pitches type \n\
    \ 'play higher' \n\
    \ To go back to main menu type 'main menu'\n\
    \ ";
  match read_line () with
  | answer -> (
      match String.lowercase_ascii answer with
      | "play shorter" ->
          play_notes
            (List.sort_uniq compare
               (List.fold_right (fun x -> fst x) [] sample_notes))
      | "play again" -> play_notes sample_notes
      | "play lower" -> play_notes (Music.lower_notes sample_notes [])
      | "play higher" -> play_notes (Music.higher_notes sample_notes [])
      | "main menu" -> main ()
      | _ -> mini_menu sample_notes)

and transform file_name =
  let pixs = Image_reader.read_image file_name in
  let wave_list = Convert.rgb_to_wl pixs [] in
  let sample_notes =
    Convert.freq_and_dur wave_list (Convert.duration pixs) []
  in
  play_notes sample_notes

let () = main ()
