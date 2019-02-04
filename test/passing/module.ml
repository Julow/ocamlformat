module AAAAAAAAAAAAAAAAAAA =
  Soooooooooooooooooooooooome.Loooooooooooooooooooooooong.Mod

let _ =
  let module A = B in
  let module AAAAAAAAAAAAAAAAAAA =
    Soooooooooooooooooooooooome.Loooooooooooooooooooooooong.Mod
  in
  t

let create (type a b) t i w p =
  let module T = (val (t : (a, b) t)) in
  T.create i w p

module C = struct
  module rec A : sig
    type t

    module rec B : sig
      type t

      type z
    end

    and A : B
  end =
    A

  and B : sig end = B
end

module X = struct end

module X = struct
  let () = ()
end

module X : sig
  type t
end = struct
  let () = ()
end

module X : sig
  type t
end = struct end

module X :
  sig
    type t
  end
  with type t = t = struct
  let () = ()
end

module X :
  sig
    type t
  end
  with type t = t = struct end

module X :
  sig
    (* b *)

    type t
  end
  with type t = t (* c *) = (* a *) struct
  (* d *)

  let () = ()
end

module X :
  sig
    (** b *)
    type t
  end
  with type t = t = struct
  (** d *)
  let () = ()
end
