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

let x =
  ( module Soooooooooooooooooooooooome.Tooooooooooo
           .Loooooooooooooooooooooooong
           .Mod )

let x :
    (module AAAAAAAAAAAAAAAAAAA
       with type loooooooooooong_type = also_long_type) =
  x

let x :
    (module
     Soooooooooooooooooooooooome.Toooooooooooo.Loooooooooooooooooooooooong
     .Mod with type loooooooooooong_type = also_long_type) =
  x
