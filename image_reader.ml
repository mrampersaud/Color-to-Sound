open Images

type pixel = int * int * int

type pixels = pixel list

(* a single pixel is a tuple of red green blue values *)
(* [pixels] is a list of pixels *)
(* make_pixel creates a pixel using the inputted red green blue values
   [a] [b] [c] *)
let make_pixel a b c = (a, b, c)

(* make_pixels groups the two single pixels [pixel_one] [pixel_two]
   together and returns type pixels*)
let make_pixels (pixel_one : pixel) (pixel_two : pixel) : pixels =
  [ pixel_one; pixel_two ]

(* get_red returns the red value in pixel *)
let get_red = function x, y, z -> x

(* get_green returns the green value in pixel *)
let get_green = function x, y, z -> y

(* get_blue returns the blue value in pixel *)
let get_blue = function x, y, z -> z

(* get_first takes in type pixels [lst] and returns the first pixel of
   image *)
let get_first (lst : pixels) : pixel =
  match lst with [] -> (0, 0, 0) | h :: t -> h

let empty_pixels = []

(* rgb24_pixels takes every pixel in a rgb type image [img] and builds
   pixel format *)
let rgb24_pixels img =
  let pix_list = ref [] in
  for x = 0 to img#width - 1 do
    for y = 0 to img#height - 1 do
      let color = img#get x y in
      let pix = make_pixel color.r color.g color.b in
      let () = pix_list := pix :: !pix_list in
      ()
    done
  done;
  List.sort_uniq compare !pix_list

(* index_pixels takes every pixel in a standard png type image [img] and
   builds pixel format *)
let index_pixels img =
  let pix_list = ref [] in
  for x = 0 to img#width - 1 do
    for y = 0 to img#height - 1 do
      let color = img#get_color x y in
      let pix = make_pixel color.r color.g color.b in
      let () = pix_list := pix :: !pix_list in
      ()
    done
  done;
  List.sort_uniq compare !pix_list

(* rgba32_pixels takes every pixel in a rgba32 type image [img] and
   builds pixel format *)
let rgba32_pixels img =
  let pix_list = ref [] in
  for x = 0 to img#width - 1 do
    for y = 0 to img#height - 1 do
      let color = img#get x y in
      let pix = make_pixel color.color.r color.color.g color.color.b in
      let () = pix_list := pix :: !pix_list in
      ()
    done
  done;
  List.sort_uniq compare !pix_list

(* cmyk32_pixels takes every pixel in a cmyk32 type image [img] and
   builds pixel format *)
let cmyk32_pixels img =
  let pix_list = ref [] in
  for x = 0 to img#width - 1 do
    for y = 0 to img#height - 1 do
      let color = img#get x y in
      let pix = make_pixel color.c color.m color.y in
      let () = pix_list := pix :: !pix_list in
      ()
    done
  done;
  List.sort_uniq compare !pix_list

(* read_image takes in a png image file [str] and convert it to it
   proper camlimages class to it can be in pixels format *)
let read_image str : pixels =
  let img = OImages.load str [] in
  match img#image_class with
  | OImages.ClassRgb24 -> rgb24_pixels (OImages.rgb24 img)
  | OImages.ClassIndex8 -> index_pixels (OImages.index8 img)
  | OImages.ClassIndex16 -> index_pixels (OImages.index16 img)
  | OImages.ClassRgba32 -> rgba32_pixels (OImages.rgba32 img)
  | OImages.ClassCmyk32 -> cmyk32_pixels (OImages.cmyk32 img)
