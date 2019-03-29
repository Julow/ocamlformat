(** test *)
module A = B

(** @open *)
include A

(** @open *)
include B

include A

type t = C of int  (** docstring comment *)

type t = C of int [@ocaml.doc " docstring attribute "]

(** comment *)
include Mod

(** before *)
let x = 2
(** after *)

(**floatting1*)
(**floatting2*)

(**before*)
and y = 2
(** after *)

(** A *)
let a = 0
(** A' *)

module Comment_placement : sig
  (** Type *)
  type t

  (** Module *)
  module A : B

  val a : b
  (** Val *)

  (** Exception *)
  exception E

  (** Include *)
  include M

  (** Open *)
  open M

  external a : b = "c"
  (** External *)

  (** Rec module *)
  module rec A : B

  (** Module type *)
  module type A

  (** Class *)
  class a : b

  (** Class type *)
  class type a = b

  (* [@@@some attribute] *)
  (* (** Attribute *) *)

  [%%some extension]  (** Extension *)

  (** A *)
  external a : b = "double_comment"
  (** B *)
end = struct
  (** Type *)
  type t = {a: int}

  (** Variant declaration *)
  type t = T

  (** Type extension *)
  type t += T

  (** Module *)
  module A = B

  (** Let *)
  let a = b

  (** Exception *)
  exception E

  (** Include *)
  include M

  (** Open *)
  open M

  external a : b = "c"
  (** External *)

  (** Rec module *)
  module rec A : B = C

  (** Module type *)
  module type A = B

  (** Class *)
  class a = b

  (** Class type *)
  class type a = b

  (* [@@@some attribute] *)
  (* (** Attribute *) *)

  (** Extension *)[%%some
  extension]

  (* ;; *)
  (* (** Eval *) *)
  (* 1 + 1 *)
  (* ;; *)

  (** A *)
  external a : b = "double_comment"
  (** B *)
end
