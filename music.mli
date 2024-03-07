(** Handles the generation of pitch frequencies and plays sound to the
    user *)

(** a list of frequencies and durations *)
type notes = (int * float) list

(** a list of .wav file names*)
type files = (string * float) list

(** converts a frequency into the name of its respective .wav file *)
val tone : int -> string

(** converts a list of notes into a list of .wav file names*)
val file_converter : notes -> files

(** reads each .wav file and plays the respective note to the user *)
val play_notes : files -> unit

(** lowers the frequency of a note to the next lowest note *)
val lower : int -> float

(** lowers the frequency of a list of notes to their next lowest notes *)
val lower_notes :
  (int * float) list -> (int * float) list -> (int * float) list

(** raises the frequency of a note to the next highest note *)
val higher : int -> float

(** raises the frequency of a list of notes to their next highest notes *)
val higher_notes :
  (int * float) list -> (int * float) list -> (int * float) list

(** converts the list of frequencies into something more song-like *)
val song : int list -> int list

(* handles the operations of the music notes quiz *)
val quiz : string list -> int -> string

(* returns corresponding wav file based on users total score *)
val get_results : int -> string

(* update_points increases score[ points] based on user input [answer] *)
val update_points : string -> int -> int

(* increases the input number by a certain amount *)
val inc : int -> int -> int
