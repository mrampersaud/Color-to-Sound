(** Handles image processing and storing the pixels in an image for
    later use *)

(** a tuple expressing individual rgb (red green blue) values *)
type pixel = int * int * int

(** a list with elements of type pixel *)
type pixels = pixel list

(** creates a pixel from three input integers *)
val make_pixel : int -> int -> int -> pixel

(** creates a list of type pixels by appending two pixels *)
val make_pixels : pixel -> pixel -> pixels

(** returns the red value of the pixel*)
val get_red : pixel -> int

(** returns the blue value of the pixel *)
val get_blue : pixel -> int

(** returns the green value of the pixel *)
val get_green : pixel -> int

(* get_first_pixel returns the first [pixel] in list [pixels]*)
val get_first : pixels -> pixel

(* read_image will return all rbg values of [string] file name to return
   each value in a list [pixels] *)
val read_image : string -> pixels
