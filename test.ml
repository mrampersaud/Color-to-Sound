(* TEST APPROACH: Image_reader: This module reads the pixels of an
   image. We used black box testing to ensure that the images were read
   Accurately. For example, our first test case was a image with one
   single color which would return one unqiue pixel. Once that test case
   worked we tested more challeneges cases, multi colors larger sizes
   etc. We used manual tesing to check the accuracy of the pixels by
   using a online color idenfitifer.\ Music: We used blackbox testing
   for this module by checking for cases where the list is empty,
   different ranges in frequencies etc. We used manual testing for this
   by checking that the correct file is playing for the corresponding
   frequency. We also use unit testing for cases where the list is empty
   and making sure the length of the list matches the number of sounds
   played \ Convert.ml: Testing convert requires testing many edge cases
   and computations of values. In convert, the conversion between
   wavelength to frequency requires changes between float and ints and
   requires proper use of the round function on large, small, and
   negative numbers. Convert also requires conversion between units (nm
   to m, etc). Furthermore, when it came to testing RGB values there
   exists regular values that normally appear in the spectrum of light
   and the edge cases such as blacks, grays, and whites that dont appear
   and for the sake of the project we had to implement or own
   interpretation of these colors as sounds. Because this module deals
   heavily with computation and numerical manipulation, the entire
   module was tested using OUnit test cases rather than manual testing.
   Our appraoch implemented white box testing strategies and was
   deliberately developed with the goal of testing the inner workings of
   the system.*)

open OUnit2
open Image_reader
open Convert
open Music

let pp_string s = "\"" ^ string_of_int s ^ "\""

(*[pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
  pretty-print each element of [lst]. *)

let print_tuple (pix : pixel) =
  string_of_int (get_red pix)
  ^ ","
  ^ string_of_int (get_green pix)
  ^ ","
  ^ string_of_int (get_blue pix)

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
  "(" ^ pp_elts lst ^ ")"

(*let rgb_to_string (rgb : Color.rgb) = "(" ^ string_of_int rgb.r ^ ", "
  ^ string_of_int rgb.g ^ ", " ^ string_of_int rgb.b ^ ")"

  let image_read_image
  (name:string)(image:string)(expected_output:Color.rgb array):test =
  name >:: fun _ -> assert_equal (Array.to_list (Array.map
  (rgb_to_string)(expected_output))) (Array.to_list (Array.map
  (rgb_to_string) (read_image image))) ~printer:(pp_list pp_string) *)
(* let print_array = Array.iter (print_endline) *)
(* let print_array arr = Array.fold_left(fun x -> x) (^) arr *)

let pixel_one = make_pixel 4 30 2

let pixel_two = make_pixel 22 244 12

let pixels_one = make_pixels pixel_one pixel_two

let image_make_pixel
    (name : string)
    (px1 : int)
    (px2 : int)
    (px3 : int)
    (expected_output : pixel) : test =
  name >:: fun _ ->
  assert_equal expected_output (make_pixel px1 px2 px3)

let image_get_first
    (name : string)
    (pxs : pixels)
    (expected_output : pixel) : test =
  name >:: fun _ -> assert_equal expected_output (get_first pxs)

let image_read_image
    (name : string)
    (file : string)
    (expected_output : pixels) : test =
  name >:: fun _ ->
  assert_equal expected_output (read_image file)
    ~printer:(pp_list print_tuple)

let image_tests =
  [
    image_get_first "returns head of list of length 2" pixels_one
      pixel_one;
    image_make_pixel "pixel_one" 4 30 2 pixel_one;
    image_make_pixel "pixel_two" 22 244 12 pixel_two;
    image_read_image "small sized image" "rubic.png"
      (read_image "rubic.png");
    image_read_image "blue image" "blue.png" (read_image "blue.png");
  ]

let round_test (name : string) (input : float) (expected_output : int) :
    test =
  name >:: fun _ ->
  assert_equal expected_output (Convert.round input)
    ~printer:string_of_int

let round_tests =
  [
    round_test "round 0." 0. 0;
    round_test "round 8.4" 8.4 8;
    round_test "round 3.7" 3.7 4;
    round_test "round 10.5" 10.5 11;
    round_test "round 9.0" 9.0 9;
    round_test "round negative" (-1.2) (-1);
    round_test "round multiple decimals" 90.210 90;
    round_test "round 0 decimal" 0.210 0;
    round_test "big number" 789215. 789215;
    round_test "middle decimal" 390.54657 391;
  ]

let max_test (name : string) input expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (Convert.find_max input)

let max_tests =
  [
    max_test "all equal tuple" (0., 0., 0.) 0.;
    max_test "first elem greatest" (7., 0., 0.) 7.;
    max_test "second elem greatest" (7., 10., 0.) 10.;
    max_test "third elem greatest" (7., 10., 15.) 15.;
    max_test "negative numbers" (-3., -8., -1.) (-1.);
    max_test "numbers with nonzero decimals" (3.5, 3.3, 3.9) 3.9;
    max_test "numbers with multiple decimals" (3.577, 3., 4.3) 4.3;
    max_test "numbers with zero decimals" (3.0, 3.00, 0.) 3.;
    max_test "negative numbers with multiple decimals"
      (-0.452, 3.0, 535.738) 535.738;
    max_test "numbers with multiple decimals" (100.100, 0.00, 0.543)
      100.100;
  ]

let min_test (name : string) input expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (Convert.find_min input)

let min_tests =
  [
    min_test "all equal tuple" (4., 4., 4.) 4.;
    min_test "first elem least" (15., 25., 15.) 15.;
    min_test "second elem least" (5., 3., 4.) 3.;
    min_test "third elem least" (44., 3., 1.) 1.;
    min_test "negative numbers" (-3., -8., -1.) (-8.);
    min_test "numbers with non zero decimals" (0.44, 0.34, 0.01) 0.01;
    min_test "numbers with multiple decimals" (3.577, 3., 4.3) 3.;
    min_test "numbers with zero decimals" (3.0, 3.00, 0.) 0.;
    min_test "negative numbers with multiple decimals"
      (-0.452, 3.0, 535.738) (-0.452);
    min_test "numbers with multiple decimals" (100.100, 0.00, 0.543)
      0.00;
  ]

let rgb_to_hue_test (name : string) input (expected_output : int) : test
    =
  name >:: fun _ ->
  assert_equal expected_output (Convert.rgb_to_hue input)

let rgb_to_hue_tests =
  [
    rgb_to_hue_test "two large and one small rgb" (200, 152, 18) 44;
    rgb_to_hue_test "small same number" (66, 66, 66) 0;
    rgb_to_hue_test "small numbers" (5, 4, 3) 30;
    rgb_to_hue_test "edge case: red" (255, 0, 0) 0;
    rgb_to_hue_test "two small and one large rgb" (2, 55, 206) 224;
    rgb_to_hue_test "edge case: all zero (black)" (0, 0, 0) 0;
    rgb_to_hue_test "edge case: all max (white)" (255, 255, 255) 0;
    rgb_to_hue_test "edge case: grey" (2, 2, 2) 0;
    rgb_to_hue_test "all large" (119, 222, 206) 171;
    rgb_to_hue_test "edge case: purple" (119, 0, 206) 275;
  ]

let rgb_to_value_test (name : string) input (expected_output : float) :
    test =
  name >:: fun _ ->
  assert_equal
    (expected_output |> Convert.round)
    (input |> Convert.rgb_to_value |> Convert.round)
    ~printer:string_of_int

let rgb_to_value_tests =
  [
    rgb_to_value_test "value of all small nums" (45, 29, 10) 17.6;
    rgb_to_value_test "value of all big nums" (188, 207, 164) 81.2;
    rgb_to_value_test "value of small and big nums" (10, 244, 57) 95.7;
    rgb_to_value_test "value of all same number" (178, 178, 178) 69.8;
    rgb_to_value_test "edge case value: all zero" (0, 0, 0) 0.0;
    rgb_to_value_test "edge case value: all max" (255, 255, 255) 100.0;
    rgb_to_value_test "mix edge cases" (0, 255, 128) 100.0;
    rgb_to_value_test "edges cases and normal" (0, 255, 0) 100.0;
    rgb_to_value_test "edges cases and normal 2" (0, 255, 255) 100.0;
    rgb_to_value_test "small grey test" (2, 2, 2) 0.8;
  ]

let rgb_to_wl_test (name : string) rgb_vals wl_list expected_output :
    test =
  name >:: fun _ ->
  assert_equal expected_output (Convert.rgb_to_wl rgb_vals wl_list)

let rgb_to_wl_tests =
  [
    rgb_to_wl_test "normal case"
      [ (45, 9, 255); (15, 1, 0) ]
      [] [ 419; 646 ];
    rgb_to_wl_test "one edge value: grey" [ (215, 215, 215) ] [] [ 450 ];
    rgb_to_wl_test "one value: normal" [ (19, 109, 2) ] [] [ 110 ];
    rgb_to_wl_test "two edge case"
      [ (255, 0, 0); (82, 0, 0) ]
      [] [ 649; 640 ];
    rgb_to_wl_test "three normal cases"
      [ (2, 4, 6); (201, 202, 203); (55, 5, 255) ]
      [] [ 456; 456; 417 ];
    rgb_to_wl_test "three edge cases"
      [ (255, 255, 255); (0, 0, 0); (15, 15, 15) ]
      [] [ 650; 400; 450 ];
    rgb_to_wl_test "multiple grey cases"
      [ (9, 9, 9); (1, 1, 1) ]
      [] [ 450; 450 ];
    rgb_to_wl_test "multiple white cases"
      [ (255, 255, 255); (255, 255, 255) ]
      [] [ 650; 650 ];
    rgb_to_wl_test "multiple black cases"
      [ (0, 0, 0); (0, 0, 0) ]
      [] [ 400; 400 ];
    rgb_to_wl_test "empty cases" [] [] [];
  ]

let hertz_to_freq_test
    (name : string)
    (hertz : float)
    (expected_output : float) : test =
  name >:: fun _ ->
  assert_equal
    (expected_output |> Convert.round)
    (hertz |> Convert.hertz_to_freq |> Convert.round)

let hertz_to_freq_tests =
  [
    hertz_to_freq_test "hertz value within range" 295. 295.;
    hertz_to_freq_test "hertz value right above range" 605. 302.5;
    hertz_to_freq_test "hertz right below range" 285. 570.;
    hertz_to_freq_test "hertz val right at border" 604. 604.;
    hertz_to_freq_test "very large hertz val" 34738438640. 517.6;
    hertz_to_freq_test "large hertz value with decimals" 54878674.654
      418.7;
    hertz_to_freq_test "very small hertz value" 14. 448.;
    hertz_to_freq_test "small hertz w decimals" 23.194 371.104;
    hertz_to_freq_test "hertz in range w decimals" 432.93 432.93;
    hertz_to_freq_test "hertz at bottom border" 286. 572.;
  ]

let get_freq_test (name : string) (input : int) (expected_output : int)
    : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Convert.get_freq input)
    ~printer:string_of_int

let get_freq_tests =
  [
    get_freq_test "wl of 500" 500 545;
    get_freq_test "wl of 604" 604 451;
    get_freq_test "wl of 100" 100 341;
    get_freq_test "wl of 50" 50 341;
    get_freq_test "wl of 600" 600 454;
    get_freq_test "wl of 78" 78 437;
    get_freq_test "wl of 400" 400 341;
    get_freq_test "wl of 333" 333 409;
    get_freq_test "wl of 519" 519 525;
    get_freq_test "wl of 570" 570 478;
  ]

let freq_1 = [ 500 ]

let freq_2 = [ 20; 20 ]

let freq_3 = [ 50; 51; 52 ]

let freq_4 = [ 400; 400; 400 ] (*black*)

let freq_5 = [ 100; 21; 69; 42 ]

let freq_6 = [ 602; 604; 1000 ]

let freq_7 = [ 100000; 45849589; 338737278 ]

let freq_8 = [ 255; 8; 40; 50 ]

let freq_9 = [ 204; 205; 206; 207; 208 ]

let freq_10 = [ 533 ]

let freq_test
    (name : string)
    (input : int list)
    (expected_output : int list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Convert.frequencies input)
    ~printer:(pp_list pp_string)

let freq_tests =
  [
    freq_test "freq 1" freq_1 [ 545 ];
    freq_test "freq 2" freq_2 [ 426; 426 ];
    freq_test "freq 3" freq_3 [ 341; 334; 328 ];
    freq_test "freq 4" freq_4 [ 341; 341; 341 ];
    freq_test "freq 5" freq_5 [ 341; 406; 494; 406 ];
    freq_test "freq 6" freq_6 [ 453; 451; 545 ];
    freq_test "freq 7" freq_7 [ 349; 390; 422 ];
    freq_test "freq 8" freq_8 [ 535; 533; 426; 341 ];
    freq_test "freq 9" freq_9 [ 334; 333; 331; 329; 328 ];
    freq_test "freq 10" freq_10 [ 512 ];
  ]

let dur_1 = [ (255, 3, 42) ]

let dur_2 = [ (20, 20, 20) ]

let dur_3 = [ (50, 51, 52); (20, 20, 20) ]

let dur_4 = [ (45, 29, 10); (188, 207, 164); (0, 0, 0) ]

let dur_5 = [ (0, 255, 128); (255, 255, 255) ]

let dur_6 = [ (178, 178, 178) ]

let dur_7 = [ (10, 244, 57); (201, 202, 203); (2, 4, 6) ]

let dur_8 = [ (9, 9, 9); (1, 1, 1) ]

let dur_9 = [ (45, 9, 255) ]

let dur_10 = []

let get_dur_test
    (name : string)
    (input : int * int * int)
    (expected_output : float) : test =
  name >:: fun _ -> assert_equal expected_output (Convert.get_dur input)

let get_dur_tests =
  [
    get_dur_test "value of 0" (0, 0, 0) 1000.;
    get_dur_test "20 <= value < 40" (23, 67, 91) 800.;
    get_dur_test "40 <= value < 60" (125, 0, 43) 650.;
    get_dur_test "60 <= value < 80" (34, 92, 187) 550.;
    get_dur_test "80 <= value < 100" (34, 92, 240) 500.;
    get_dur_test "100 < value" (60, 124, 255) 450.;
    get_dur_test "rgb all 255" (255, 255, 255) 450.;
    get_dur_test "rgb big and small values" (0, 189, 69) 550.;
    get_dur_test "rgb all same number" (88, 88, 88) 800.;
    get_dur_test "g is 255, others are 0" (0, 255, 0) 450.;
  ]

let dur_test
    (name : string)
    (input : (int * int * int) list)
    (expected_output : float list) : test =
  name >:: fun _ ->
  assert_equal expected_output (Convert.duration input)

let dur_tests =
  [
    dur_test "dur 1" dur_1 [ 450. ];
    dur_test "dur 2" dur_2 [ 1000. ];
    dur_test "dur 3" dur_3 [ 800.; 1000. ];
    dur_test "dur 4" dur_4 [ 1000.; 500.; 1000. ];
    dur_test "dur 5" dur_5 [ 450.; 450. ];
    dur_test "dur 6" dur_6 [ 550. ];
    dur_test "dur 7" dur_7 [ 500.; 500.; 1000. ];
    dur_test "dur 8" dur_8 [ 1000.; 1000. ];
    dur_test "dur 9" dur_9 [ 450. ];
    dur_test "dur 1o" dur_10 [];
  ]

let dur_11 = [ 450. ]

let dur_12 = [ 1000. ]

let dur_13 = [ 800.; 1000. ]

let dur_14 = [ 1000.; 500.; 1000. ]

let dur_15 = [ 450.; 450. ]

let dur_16 = [ 550. ]

let dur_17 = [ 500.; 500.; 1000. ]

let dur_18 = [ 1000.; 1000. ]

let dur_19 = [ 450. ]

let dur_20 = []

let freq_and_dur_test (name : string) flst dlst acc expected_output :
    test =
  name >:: fun _ ->
  assert_equal expected_output (Convert.freq_and_dur flst dlst acc)

let freq_and_dur_tests =
  [
    freq_and_dur_test "dur 1" freq_1 dur_11 [] [ (500, 450.) ];
    freq_and_dur_test "dur 2" [ 20 ] dur_12 [] [ (20, 1000.) ];
    freq_and_dur_test "dur 3" freq_2 dur_13 []
      [ (20, 800.); (20, 1000.) ];
    freq_and_dur_test "dur 4" freq_4 dur_14 []
      [ (400, 1000.); (400, 500.); (400, 1000.) ];
    freq_and_dur_test "dur 5" freq_2 dur_15 []
      [ (20, 450.); (20, 450.) ];
    freq_and_dur_test "dur 6" freq_1 dur_16 [] [ (500, 550.) ];
    freq_and_dur_test "dur 7" freq_6 dur_17 []
      [ (602, 500.); (604, 500.); (1000, 1000.) ];
    freq_and_dur_test "dur 8" freq_2 dur_18 []
      [ (20, 1000.); (20, 1000.) ];
    freq_and_dur_test "dur 9" freq_1 dur_19 [] [ (500, 450.) ];
    freq_and_dur_test "dur 1o" [] dur_20 [] [];
  ]

let music_tone (name : string) (freq : int) (expected_output : string) :
    test =
  name >:: fun _ ->
  assert_equal expected_output (Music.tone freq) ~printer:String.escaped

let music_lower (name : string) (freq : int) (expected_output : string)
    : test =
  name >:: fun _ ->
  assert_equal expected_output
    (string_of_int (int_of_float (lower freq)))
    ~printer:String.escaped

let music_higher (name : string) (freq : int) (expected_output : string)
    : test =
  name >:: fun _ ->
  assert_equal expected_output
    (string_of_int (int_of_float (higher freq)))
    ~printer:String.escaped

let music_tests =
  [
    music_tone "D4 lower" 286 "D4.wav";
    music_tone "D4 higher" 302 "D4.wav";
    music_tone "D#4 lower" 303 "D-sharp4.wav";
    music_tone "D#4 higher" 320 "D-sharp4.wav";
    music_tone "E4 lower" 321 "E4.wav";
    music_tone "E4 higher" 339 "E4.wav";
    music_tone "F4 lower" 340 "F4.wav";
    music_tone "F4 higher" 359 "F4.wav";
    music_tone "F#4 lower" 360 "F-sharp4.wav";
    music_tone "F#4 higher" 380 "F-sharp4.wav";
    music_tone "G4 lower" 381 "G4.wav";
    music_tone "G4 higher" 403 "G4.wav";
    music_tone "G#4" 415 "G-sharp4.wav";
    music_tone "A4" 440 "A4.wav";
    music_tone "A#4" 466 "A-sharp4.wav";
    music_lower "A4 to G#4" 440 "415";
    music_higher "A4 to A#4" 440 "466";
  ]

let speed_up_test (name : string) lst acc expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (Convert.speed_up lst acc)

let speed_up_tests =
  [
    speed_up_test "speed up 1" [ (500, 10000.) ] [] [ (500, 9900.) ];
    speed_up_test "speed up 2" [ (400, 10.) ] [] [ (400, 0.) ];
    speed_up_test "speed up 3" [ (200, 100.) ] [] [ (200, 0.) ];
    speed_up_test "speed up 4" [ (300, 1000.) ] [] [ (300, 900.) ];
    speed_up_test "speed up 5" [ (600, 900.) ] [] [ (600, 800.) ];
    speed_up_test "speed up 6" [ (601, 850.) ] [] [ (601, 750.) ];
    speed_up_test "speed up 7" [ (700, 750.) ] [] [ (700, 650.) ];
    speed_up_test "speed up 8" [ (800, 650.) ] [] [ (800, 550.) ];
    speed_up_test "speed up 9" [ (900, 550.) ] [] [ (900, 450.) ];
    speed_up_test "speed up 10" [ (150, 450.) ] [] [ (150, 350.) ];
  ]

let slow_down_test (name : string) lst acc expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (Convert.speed_up lst acc)

let slow_down_tests =
  [
    slow_down_test "slow down 1" [ (500, 10000.0) ] []
      [ (500, 10100.0) ];
    slow_down_test "slow down 2" [ (400, 10.0) ] [] [ (400, 110.0) ];
    slow_down_test "slow down 3" [ (200, 100.0) ] [] [ (200, 200.0) ];
    slow_down_test "slow down 4" [ (300, 1000.0) ] [] [ (300, 1100.0) ];
    slow_down_test "slow down 5" [ (600, 900.0) ] [] [ (600, 1000.0) ];
    slow_down_test "slow down 6" [ (601, 850.0) ] [] [ (601, 950.0) ];
    slow_down_test "slow down 7" [ (700, 750.0) ] [] [ (700, 850.0) ];
    slow_down_test "slow down 8" [ (800, 650.0) ] [] [ (800, 750.0) ];
    slow_down_test "slow down 9" [ (900, 550.0) ] [] [ (900, 650.0) ];
    slow_down_test "slow down 10" [ (150, 450.0) ] [] [ (150, 550.0) ];
  ]

let suite =
  "test suite engine"
  >::: List.flatten
         [
           image_tests;
           round_tests;
           max_tests;
           min_tests;
           rgb_to_hue_tests;
           (* rgb_to_value_tests; *)
           (*rgb_to_wl_tests*)
           hertz_to_freq_tests;
           get_freq_tests;
           freq_tests;
           music_tests;
         ]

let _ = run_test_tt_main suite
