class t =
  object
    method meth aaaaaaaaaaa bbbbbbbbbbbbbb ccccccccccccccccccc
        ddddddddddddddddddddd eeeeeeeeeeeeeee =
      body
  end

let func aaaaaaaaaaa bbbbbbbbbbbbbb ccccccccccccccccccc
    ddddddddddddddddddddd eeeeeeeeeeeeeee =
  body

let rec func aaaaaaaaaaa bbbbbbbbbbbbbb ccccccccccccccccccc
    ddddddddddddddddddddd eeeeeeeeeeeeeee =
  body

let to_loc_trace
    ?(desc_of_source =
      fun source ->
        let callsite = Source.call_site source in
        Format.asprintf "return from %a" Typ.Procname.pp
          (CallSite.pname callsite)) ?(source_should_nest = fun _ -> true)
    ?(desc_of_sink =
      fun sink ->
        let callsite = Sink.call_site sink in
        Format.asprintf "call to %a" Typ.Procname.pp
          (CallSite.pname callsite)) ?(sink_should_nest = fun _ -> true)
    (passthroughs, sources, sinks) =
  ()

let translate_captured
    { Clang_ast_t.lci_captured_var
    ; lci_init_captured_vardecl
    ; lci_capture_this
    ; lci_capture_kind } ((trans_results_acc, captured_vars_acc) as acc) =
  ()

class t =
  object
    method meth aaaaaaaaaaa bbbbbbbbbbbbbb ccccccccccccccccccc
        ddddddddddddddddddddd eeeeeeeeeeeeeee =
      body
  end

let func aaaaaaaaaaa bbbbbbbbbbbbbb ccccccccccccccccccc
    ddddddddddddddddddddd eeeeeeeeeeeeeee =
  body

let rec func aaaaaaaaaaa bbbbbbbbbbbbbb ccccccccccccccccccc
    ddddddddddddddddddddd eeeeeeeeeeeeeee =
  body

let to_loc_trace
    ?(desc_of_source =
      fun source ->
        let callsite = Source.call_site source in
        Format.asprintf "return from %a" Typ.Procname.pp
          (CallSite.pname callsite)) ?(source_should_nest = fun _ -> true)
    ?(desc_of_sink =
      fun sink ->
        let callsite = Sink.call_site sink in
        Format.asprintf "call to %a" Typ.Procname.pp
          (CallSite.pname callsite)) ?(sink_should_nest = fun _ -> true)
    (passthroughs, sources, sinks) =
  ()

let translate_captured
    { Clang_ast_t.lci_captured_var
    ; lci_init_captured_vardecl
    ; lci_capture_this
    ; lci_capture_kind } ((trans_results_acc, captured_vars_acc) as acc) =
  ()

let f ssssssssss =
  String.fold ssssssssss ~init:innnnnnnnnnit ~f:(fun accuuuuuuuuuum ->
    function
    | '0' -> g accuuuuuuuuuum
    | '1' -> h accuuuuuuuuuum
    | _ -> i accuuuuuuuuuum )

val fffffffff : aaaaaa -> bbbbbbbbbb ccccccccc -> dddd

val fffffffff :
  aaaaaa -> bbbbbbbbbb ccccccccc -> dddd -> dddd -> dddd -> dddd

val fffffffff :
  aaaaaa -> (bbbbbbbbbb ccccccccc -> int) -> bbbbbbbbbb ccccccccc -> dddd

val fffffffff :
     eeee:('a, 'b) aaaaaa
  -> (bbbbbbbbbb ccccccccc -> int)
  -> bbbbbbbbbb ccccccccc
  -> dddd
  -> dddd

val m : (module S with type t = t)

val f :
  ( 'aaaaaaaaaaaaaaaaaaaa
  ,    xxxxxxxxxxxxxxxxxxxxxxxxx
    -> yyyyyyyyyyyyyyyyyyyyyyyyy
    -> bbbbbbbbbbbbbbbbbbbb
  , 'dddddddddddddddddddd )
  s

type t =
  | Cstr of
      (   xxxxxxxxxxxxxxxxxxxxxxxxx
       -> yyyyyyyyyyyyyyyyyyyyyyyyy
       -> aaaaaaaaaaaaaaaaaaaa)
      * bbbbbbbbbbbbbbbbbbbb

type t =
  | Cstr of
      aaaaaaaaaaaaaaaaaaaa
      * (   xxxxxxxxxxxxxxxxxxxxxxxxx
         -> yyyyyyyyyyyyyyyyyyyyyyyyy
         -> bbbbbbbbbbbbbbbbbbbb)
      * cccccccccccccccccccc

type ('aaaa, 'bbbb, 'cccc) t =
     llll:('aaaa, 'bbbb, 'cccc) s
  -> dddddd list
  -> 'aaaa * 'cccc
  -> 'bbbb uuuuu

external ident : a -> b -> c -> d = "something"

external ident : a -> b -> c -> d = "something" "else"

val ident : a -> b -> c -> d

val ident :
  arg1_is_loooooooooooooooooooooooooooooooong -> arg2 -> arg3 -> arg4

external ident :
  arg1_is_loooooooooooooooooooooooooooooooong -> arg2 -> arg3 -> arg4
  = "something" "else"

type t = {field1: a -> b -> c; field2: int; field3: a -> b -> c -> d -> e}

type t =
  {field1: a -> b -> c; field2: int; field3: a -> b -> c -> d -> e -> f}

type t =
  { field1: a -> b -> c
  ; field2: int
  ; field3:
         a_is_loooooooooooooooooooooooooooooooong
      -> b_is_loooooooooooooooooooooooooooooooong
      -> c
      -> d
      -> e
  ; field4:
      a_is_loooooooooooooooooooong -> b_is_loooooooooong -> c -> d -> e
  ; field5:
      a loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong
      typ }
