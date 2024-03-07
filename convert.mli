(** This module implements the algorithm for turning images from rbg
    pixels into sound frequencies *)

(** The abstract type of values representing rgb values. *)
type rgb_list = (int * int * int) list

(** [round num] rounds [num] to nearest whole number*)
val round : float -> int

(** [find_max tuple] returns max value in three element tuple *)
val find_max : float * float * float -> float

(** [find_min tuple] returns min value in three element tuple *)
val find_min : float * float * float -> float

(** [frst] returns the first element in three element tuple *)
val frst : 'a * 'a * 'a -> 'a

(** [scnd] returns the second element in three element tuple *)
val scnd : 'a * 'a * 'a -> 'a

(** [thrd] returns the third element in three element tuple *)
val thrd : 'a * 'a * 'a -> 'a

(** [rgb_to_hue rgb_value] returns the conversion of an rgb_value to its
    hue *)
val rgb_to_hue : int * int * int -> int

(** [rgb_to_value rgb_value] returns the conversion of an rgb_value to
    its value *)
val rgb_to_value : int * int * int -> float

(** [rgb_to_wl rgb_values wl_list] returns the conversion of an
    rgb_value to its wavelength*)
val rgb_to_wl : rgb_list -> int list -> int list

(** [hertz_to_freq hertz] returns the conversion of hertz value to
    frequency*)
val hertz_to_freq : float -> float

(** [get_freq wavelength] returns the conversion of a wavelength to its
    frequency*)
val get_freq : int -> int

(** [frequencies lst] returns the conversion of a wavelength list to a
    frequency list*)
val frequencies : int list -> int list

(** [get_dur rgb_value] returns the conversion of an rgb_value to its
    value*)
val get_dur : int * int * int -> float

(** [duration rgb_list] returns the conversion of an rgb list to a
    duration list*)
val duration : (int * int * int) list -> float list

(** [freq_and_dur] returns the combined tuple list of a frequency list
    and duration list*)
val freq_and_dur :
  int list -> float list -> (int * float) list -> (int * float) list

(** [speed_up lst acc] returns the frequency and duration tuple with a
    shorter duration *)
val speed_up :
  (int * float) list -> (int * float) list -> (int * float) list

(** [slow_down lst acc] returns the frequency and duration tuple with a
    longer duration *)
val slow_down :
  (int * float) list -> (int * float) list -> (int * float) list
