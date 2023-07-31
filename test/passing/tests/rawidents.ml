module type A = sig
  type ('\#let, '\#a) \#virtual = ('\#let * '\#a) as '\#mutable
  val foo : '\#let '\#a . '\#a -> '\#let -> unit
  type \#foo = { \#let : int }
end

module M = struct
  let (\#let,\#foo) as \#val = (\#mutable,\#baz)
  let _ = fun (type \#let) (type \#foo) -> 1
  let f g ~\#let ?\#and ?(\#for = \#and) () =
    g ~\#let ?\#and ()
  class \#let = object
    inherit \#val \#let as \#mutable
  end
end
