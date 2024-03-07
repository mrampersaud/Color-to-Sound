open Float

exception RedValueError

(**[rgb_list] is a tuple corresponding to the rgb values of an image.*)
type rgb_list = Image_reader.pixels

let round (num : float) : int =
  if num > 0. then
    let int_num = truncate num in
    let decimal = num -. float_of_int int_num in
    if decimal >= 0.5 then int_num + 1 else int_num
  else
    let int_num = truncate num in
    let decimal = num -. float_of_int int_num in
    if decimal <= -0.5 then int_num + 1 else int_num

let find_max tuple =
  match tuple with
  | r, g, b ->
      if r > g then if r > b then r else b else if g > b then g else b

let find_min tuple =
  match tuple with
  | r, g, b ->
      if r < g then if r < b then r else b else if g < b then g else b

let frst (x, _, _) = x

let scnd (_, y, _) = y

let thrd (_, _, z) = z

let rgb_to_hue rgb_value =
  let r = float_of_int (frst rgb_value) /. 255.0 in
  let g = float_of_int (scnd rgb_value) /. 255.0 in
  let b = float_of_int (thrd rgb_value) /. 255.0 in

  let max_val = find_max (r, g, b) in
  let min_val = find_min (r, g, b) in

  let difference = max_val -. min_val in

  if max_val = min_val then
    let hue = 0.0 in
    round hue mod 360
  else if max_val = r then
    let hue = (60.0 *. ((g -. b) /. difference)) +. 360.0 in
    round hue mod 360
  else if max_val = g then
    let hue = (60.0 *. ((b -. r) /. difference)) +. 120.0 in
    round hue mod 360
  else
    let hue = (60.0 *. ((r -. g) /. difference)) +. 240.0 in
    round hue mod 360

let rgb_to_value rgb_value =
  let r = float_of_int (frst rgb_value) /. 255.0 in
  let b = float_of_int (scnd rgb_value) /. 255.0 in
  let g = float_of_int (thrd rgb_value) /. 255.0 in
  let value = find_max (r, g, b) in
  value

let rec rgb_to_wl (rgb_values : rgb_list) (wl_list : int list) =
  match rgb_values with
  | [] -> wl_list
  | h :: t -> (
      match h with
      (*each elem of the list is a tuple*)
      | red, green, blue ->
          let hue = rgb_to_hue h in
          if hue = 0 then
            try rgb_to_wl_edge h wl_list t
            with RedValueError ->
              let red_value = rgb_to_value h in
              if red_value >= 0.5 then
                let hue = 1 in
                let wavelength =
                  650.0 -. (250.0 /. 270.0 *. float_of_int hue)
                in
                let acc = round wavelength :: wl_list in
                rgb_to_wl t acc
              else
                let hue = 10 in
                let wavelength =
                  650.0 -. (250.0 /. 270.0 *. float_of_int hue)
                in
                let acc = round wavelength :: wl_list in
                rgb_to_wl t acc (*red*)
          else
            let wavelength =
              650.0 -. (250.0 /. 270.0 *. float_of_int hue)
            in
            let acc = round wavelength :: wl_list in
            rgb_to_wl t acc)

and wl_list = []

and rgb_to_wl_edge
    (rgb_value : int * int * int)
    (wl_list : int list)
    (rgb_values : rgb_list) =
  match (rgb_value, rgb_values) with
  | (red, green, blue), h :: t ->
      if red = 0 && blue = 0 && green = 0 then
        let wavelength = 400.0 in
        let acc = round wavelength :: wl_list in
        rgb_to_wl t acc (* black*)
      else if red = 255 && blue = 255 && green = 255 then
        let wavelength = 650.0 in
        let acc = round wavelength :: wl_list in
        rgb_to_wl t acc (*white*)
      else if red = green && red = blue then
        let wavelength = 450.0 in
        let acc = round wavelength :: wl_list in
        rgb_to_wl t acc (*grey*)
      else raise RedValueError
  | _ -> raise RedValueError

let rec hertz_to_freq (hertz : float) =
  if 286.0 < hertz && hertz <= 604.0 then hertz
  else if hertz <= 286. then hertz_to_freq (hertz *. 2.)
  else hertz_to_freq (hertz /. 2.0)

let pow one mul base exp =
  let rec pow_help p x = function
    | 0 -> x
    | i ->
        pow_help (mul p p) (if i mod 2 = 1 then mul p x else x) (i / 2)
  in
  pow_help base one exp

let get_freq (wavelength : int) =
  (* convert from nm to m *)
  let wavelength = float_of_int wavelength /. pow 1.0 ( *. ) 10. 9 in
  let tetra = 0.000299792458 /. wavelength in
  (* divide by speed of light *)
  let hertz = tetra *. pow 1.0 ( *. ) 10. 12 in
  let frequency = round (hertz_to_freq hertz) in
  frequency

let frequencies lst = List.map get_freq lst

let get_dur rgb_value : float =
  let value = round (rgb_to_value rgb_value *. 100.0) in
  if 0 <= value && value < 20 then
    let dur = 0.2 in
    dur
  else if 20 <= value && value < 40 then
    let dur = 0.5 in
    dur
  else if 40 <= value && value < 60 then
    let dur = 0.3 in
    dur
  else if 60 <= value && value < 80 then
    let dur = 1. in
    dur
  else if 80 <= value && value < 100 then
    let dur = 0.8 in
    dur
  else
    let dur = 0.1 in
    dur

(* change durations to better corresponding millisecond values *)
let duration rgb_lst : float list = List.map get_dur rgb_lst

let rec freq_and_dur flst dlst acc =
  match (flst, dlst) with
  | [], [] -> acc
  | h1 :: t1, h2 :: t2 -> freq_and_dur t1 t2 ((h1, h2) :: acc)
  | _ -> acc

let rec speed_up (lst : (int * float) list) (acc : (int * float) list) :
    (int * float) list =
  match lst with
  | [] -> acc
  | (f, d) :: t ->
      let new_d = d -. 0.2 in
      if new_d < 0.0 then speed_up t acc
      else
        let new_d = new_d in
        speed_up t ((f, new_d) :: acc)

let rec slow_down (lst : (int * float) list) (acc : (int * float) list)
    : (int * float) list =
  match lst with
  | [] -> acc
  | (f, d) :: t ->
      let new_d = d +. 0.2 in
      let new_d = new_d in
      speed_up t ((f, new_d) :: acc)
