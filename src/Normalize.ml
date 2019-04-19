(**********************************************************************
 *                                                                    *
 *                            OCamlFormat                             *
 *                                                                    *
 *  Copyright (c) 2017-present, Facebook, Inc.  All rights reserved.  *
 *                                                                    *
 *  This source code is licensed under the MIT license found in the   *
 *  LICENSE file in the root directory of this source tree.           *
 *                                                                    *
 **********************************************************************)

(** Normalize abstract syntax trees *)

open Migrate_ast
open Asttypes
open Parsetree
open Ast_helper

let comment s =
  (* normalize consecutive whitespace chars to a single space *)
  String.concat ~sep:" "
    (List.filter ~f:(Fn.non String.is_empty)
       (String.split_on_chars s ~on:['\t'; '\n'; '\011'; '\012'; '\r'; ' ']))

let list f fmt l =
  let pp_sep fmt () = Format.fprintf fmt "" in
  Format.pp_print_list ~pp_sep f fmt l

let str fmt s = Format.fprintf fmt "%s" (comment s)

let ign_loc f fmt with_loc = f fmt with_loc.Odoc__model.Location_.value

let fpf = Format.fprintf

open Odoc__parser.Ast
module Names = Odoc__model.Names

let odoc_style fmt = function
  | `Bold -> fpf fmt "Bold"
  | `Italic -> fpf fmt "Italic"
  | `Emphasis -> fpf fmt "Emphasis"
  | `Superscript -> fpf fmt "Superscript"
  | `Subscript -> fpf fmt "Subscript"

let rec odoc_reference_resolved fmt : Reference.Resolved.t -> unit =
  let open Names in
  let open Reference.Resolved in
  let dot r s = fpf fmt "%a.%s" odoc_reference_resolved r s in
  function
  | `Identifier id -> fpf fmt "%s" (Identifier.name id)
  | `SubstAlias (_, r) -> odoc_reference_resolved fmt (r :> t)
  | `Module (r, s) -> dot (r :> t) (ModuleName.to_string s)
  | `Canonical (_, `Resolved r) -> odoc_reference_resolved fmt (r :> t)
  | `Canonical (p, _) -> odoc_reference_resolved fmt (p :> t)
  | `ModuleType (r, s) -> dot (r :> t) (ModuleTypeName.to_string s)
  | `Type (r, s) -> dot (r :> t) (TypeName.to_string s)
  | `Constructor (r, s) -> dot (r :> t) (ConstructorName.to_string s)
  | `Field (r, s) -> dot (r :> t) (FieldName.to_string s)
  | `Extension (r, s) -> dot (r :> t) (ExtensionName.to_string s)
  | `Exception (r, s) -> dot (r :> t) (ExceptionName.to_string s)
  | `Value (r, s) -> dot (r :> t) (ValueName.to_string s)
  | `Class (r, s) -> dot (r :> t) (ClassName.to_string s)
  | `ClassType (r, s) -> dot (r :> t) (ClassTypeName.to_string s)
  | `Method (r, s) -> dot (r :> t) (MethodName.to_string s)
  | `InstanceVariable (r, s) ->
      dot (r :> t) (InstanceVariableName.to_string s)
  | `Label (r, s) ->
      fpf fmt "%a:%s" odoc_reference_resolved
        (r :> t)
        (LabelName.to_string s)

let rec odoc_reference fmt : Reference.t -> unit =
  let open Names in
  let open Reference in
  let dot p rhs = fpf fmt "%a.%s" odoc_reference p rhs in
  function
  | `Root (s, _) -> fpf fmt "%s" (UnitName.to_string s)
  | `Dot (p, s) -> dot (p :> t) s
  | `Module (p, s) -> dot (p :> t) (ModuleName.to_string s)
  | `ModuleType (p, s) -> dot (p :> t) (ModuleTypeName.to_string s)
  | `Type (p, s) -> dot (p :> t) (TypeName.to_string s)
  | `Constructor (p, s) -> dot (p :> t) (ConstructorName.to_string s)
  | `Field (p, s) -> dot (p :> t) (FieldName.to_string s)
  | `Extension (p, s) -> dot (p :> t) (ExtensionName.to_string s)
  | `Exception (p, s) -> dot (p :> t) (ExceptionName.to_string s)
  | `Value (p, s) -> dot (p :> t) (ValueName.to_string s)
  | `Class (p, s) -> dot (p :> t) (ClassName.to_string s)
  | `ClassType (p, s) -> dot (p :> t) (ClassTypeName.to_string s)
  | `Method (p, s) -> dot (p :> t) (MethodName.to_string s)
  | `InstanceVariable (p, s) ->
      dot (p :> t) (InstanceVariableName.to_string s)
  | `Label (p, s) -> dot (p :> t) (LabelName.to_string s)
  | `Resolved r -> odoc_reference_resolved fmt r

let rec odoc_inline_element fmt = function
  | `Space -> ()
  | `Word txt ->
      (* Ignore backspace changes *)
      let txt =
        String.filter txt ~f:(function '\\' -> false | _ -> true)
      in
      fpf fmt "Word,%a" str txt
  | `Code_span txt -> fpf fmt "Code_span,%a" str txt
  | `Raw_markup (`Html, txt) -> fpf fmt "Raw_html,%a" str txt
  | `Styled (style, elems) ->
      fpf fmt "Styled,%a,%a" odoc_style style odoc_inline_elements elems
  | `Reference (_kind, ref, content) ->
      fpf fmt "Reference,%a,%a" odoc_reference ref odoc_inline_elements
        content
  | `Link (txt, content) ->
      fpf fmt "Link,%a,%a" str txt odoc_inline_elements content

and odoc_inline_elements fmt elems =
  list (ign_loc odoc_inline_element) fmt elems

let rec odoc_nestable_block_element fmt : nestable_block_element -> unit =
  function
  | `Paragraph elms -> fpf fmt "Paragraph,%a" odoc_inline_elements elms
  | `Code_block txt -> fpf fmt "Code_block,%a" str txt
  | `Verbatim txt -> fpf fmt "Verbatim,%a" str txt
  | `Modules mods ->
      fpf fmt "Modules,%a" (list odoc_reference) (mods :> Reference.t list)
  | `List (ord, items) ->
      let ord = match ord with `Unordered -> "U" | `Ordered -> "O" in
      let list_item fmt elems =
        fpf fmt "Item(%a)" odoc_nestable_block_elements elems
      in
      fpf fmt "List,%s,%a" ord (list list_item) items

and odoc_nestable_block_elements fmt elems =
  list (ign_loc odoc_nestable_block_element) fmt elems

let odoc_tag fmt : tag -> unit = function
  | `Author txt -> fpf fmt "Author,%a" str txt
  | `Deprecated elems ->
      fpf fmt "Deprecated,%a" odoc_nestable_block_elements elems
  | `Param (p, elems) ->
      fpf fmt "Param,%a,%a" str p odoc_nestable_block_elements elems
  | `Raise (p, elems) ->
      fpf fmt "Raise,%a,%a" str p odoc_nestable_block_elements elems
  | `Return elems -> fpf fmt "Return,%a" odoc_nestable_block_elements elems
  | `See (kind, txt, elems) ->
      let kind =
        match kind with `Url -> "U" | `File -> "F" | `Document -> "D"
      in
      fpf fmt "See,%s,%a,%a" kind str txt odoc_nestable_block_elements elems
  | `Since txt -> fpf fmt "Since,%a" str txt
  | `Before (p, elems) ->
      fpf fmt "Before,%a,%a" str p odoc_nestable_block_elements elems
  | `Version txt -> fpf fmt "Version,%a" str txt
  | `Canonical (_, ref) ->
      fpf fmt "Canonical,%a" odoc_reference (ref :> Reference.t)
  | `Inline -> fpf fmt "Inline"
  | `Open -> fpf fmt "Open"
  | `Closed -> fpf fmt "Closed"

let odoc_block_element fmt : block_element -> unit = function
  | `Heading (lvl, lbl, content) ->
      let lvl = Int.to_string lvl in
      let lbl = match lbl with Some lbl -> lbl | None -> "" in
      fpf fmt "Heading,%s,%a,%a" lvl str lbl odoc_inline_elements content
  | `Tag tag -> fpf fmt "Tag,%a" odoc_tag tag
  | #nestable_block_element as elm -> odoc_nestable_block_element fmt elm

let odoc_docs fmt elems = list (ign_loc odoc_block_element) fmt elems

let docstring c text =
  if not c.Conf.parse_docstrings then comment text
  else
    let location = Lexing.dummy_pos in
    let parsed = Odoc__parser.Parser.parse_comment_raw ~location ~text in
    Format.asprintf "Docstring(%a)%!" odoc_docs
      parsed.Odoc__model.Error.value

let sort_attributes : attributes -> attributes =
  List.sort ~compare:Poly.compare

let make_mapper c ~ignore_doc_comment =
  (* remove locations *)
  let location _ _ = Location.none in
  let doc_attribute = function
    | {attr_name= {txt= "ocaml.doc" | "ocaml.text"; _}; _} -> true
    | _ -> false
  in
  let attribute (m : Ast_mapper.mapper) (attr : attribute) =
    match (attr.attr_name, attr.attr_payload) with
    | ( {txt= ("ocaml.doc" | "ocaml.text") as txt; loc}
      , PStr
          [ { pstr_desc=
                Pstr_eval
                  ( { pexp_desc= Pexp_constant (Pconst_string (doc, None))
                    ; pexp_loc
                    ; pexp_attributes
                    ; _ }
                  , [] )
            ; pstr_loc } ] ) ->
        let doc' =
          if ignore_doc_comment then "IGNORED" else docstring c doc
        in
        { attr_name= {txt; loc= m.location m loc}
        ; attr_payload=
            m.payload m
              (PStr
                 [ { pstr_desc=
                       Pstr_eval
                         ( { pexp_desc=
                               Pexp_constant (Pconst_string (doc', None))
                           ; pexp_loc= m.location m pexp_loc
                           ; pexp_attributes= m.attributes m pexp_attributes
                           ; pexp_loc_stack= [] }
                         , [] )
                   ; pstr_loc= m.location m pstr_loc } ])
        ; attr_loc= m.location m attr.attr_loc }
    | _ -> Ast_mapper.default_mapper.attribute m attr
  in
  (* sort attributes *)
  let attributes (m : Ast_mapper.mapper) (atrs : attribute list) =
    let atrs =
      if ignore_doc_comment then
        List.filter atrs ~f:(fun a -> not (doc_attribute a))
      else atrs
    in
    Ast_mapper.default_mapper.attributes m (sort_attributes atrs)
  in
  let expr (m : Ast_mapper.mapper) exp =
    let exp = {exp with pexp_loc_stack= []} in
    let {pexp_desc; pexp_attributes; _} = exp in
    match pexp_desc with
    (* convert [(c1; c2); c3] to [c1; (c2; c3)] *)
    | Pexp_sequence
        ({pexp_desc= Pexp_sequence (e1, e2); pexp_attributes= []; _}, e3) ->
        m.expr m
          (Exp.sequence e1 (Exp.sequence ~attrs:pexp_attributes e2 e3))
    | Pexp_poly ({pexp_desc= Pexp_constraint (e, t); _}, None) ->
        m.expr m {exp with pexp_desc= Pexp_poly (e, Some t)}
    | Pexp_constraint (e, {ptyp_desc= Ptyp_poly ([], _t); _}) -> m.expr m e
    | _ -> Ast_mapper.default_mapper.expr m exp
  in
  let pat (m : Ast_mapper.mapper) pat =
    let pat = {pat with ppat_loc_stack= []} in
    let {ppat_desc; ppat_loc= loc1; ppat_attributes= attrs1; _} = pat in
    (* normalize nested or patterns *)
    match ppat_desc with
    | Ppat_or
        ( pat1
        , { ppat_desc= Ppat_or (pat2, pat3)
          ; ppat_loc= loc2
          ; ppat_attributes= attrs2
          ; _ } ) ->
        m.pat m
          (Pat.or_ ~loc:loc1 ~attrs:attrs1
             (Pat.or_ ~loc:loc2 ~attrs:attrs2 pat1 pat2)
             pat3)
    | _ -> Ast_mapper.default_mapper.pat m pat
  in
  let typ (m : Ast_mapper.mapper) typ =
    let typ = {typ with ptyp_loc_stack= []} in
    Ast_mapper.default_mapper.typ m typ
  in
  let value_binding (m : Ast_mapper.mapper) vb =
    let { pvb_pat= {ppat_desc; ppat_loc; ppat_attributes; _}
        ; pvb_expr
        ; pvb_loc
        ; pvb_attributes } =
      vb
    in
    match (ppat_desc, pvb_expr.pexp_desc) with
    (* recognize and undo the pattern of code introduced by
       ocaml/ocaml@fd0dc6a0fbf73323c37a73ea7e8ffc150059d6ff to fix
       https://caml.inria.fr/mantis/view.php?id=7344 *)
    | ( Ppat_constraint
          ( ({ppat_desc= Ppat_var _; _} as p0)
          , {ptyp_desc= Ptyp_poly ([], t0); _} )
      , Pexp_constraint (e0, t1) )
      when Poly.(t0 = t1) ->
        m.value_binding m
          (Vb.mk ~loc:pvb_loc ~attrs:pvb_attributes p0
             (Exp.constraint_ ~loc:ppat_loc ~attrs:ppat_attributes e0 t0))
    (* convert [let (x : t) = e] to [let x = (e : t)] *)
    | Ppat_constraint (p0, t0), _ ->
        m.value_binding m
          (Vb.mk ~loc:pvb_loc ~attrs:pvb_attributes p0
             (Exp.constraint_ ~loc:ppat_loc ~attrs:ppat_attributes pvb_expr
                t0))
    | _ -> Ast_mapper.default_mapper.value_binding m vb
  in
  let structure_item (m : Ast_mapper.mapper) (si : structure_item) =
    match si.pstr_desc with
    | Pstr_eval ({pexp_desc= Pexp_extension e; _}, []) ->
        let e = m.extension m e in
        let pstr_loc = m.location m si.pstr_loc in
        {pstr_desc= Pstr_extension (e, []); pstr_loc}
    | _ -> Ast_mapper.default_mapper.structure_item m si
  in
  let structure (m : Ast_mapper.mapper) (si : structure) =
    let si =
      if ignore_doc_comment then
        List.filter si ~f:(fun si ->
            match si.pstr_desc with
            | Pstr_attribute a -> not (doc_attribute a)
            | _ -> true)
      else si
    in
    Ast_mapper.default_mapper.structure m si
  in
  let signature (m : Ast_mapper.mapper) (si : signature) =
    let si =
      if ignore_doc_comment then
        List.filter si ~f:(fun si ->
            match si.psig_desc with
            | Psig_attribute a -> not (doc_attribute a)
            | _ -> true)
      else si
    in
    Ast_mapper.default_mapper.signature m si
  in
  let class_signature (m : Ast_mapper.mapper) (si : class_signature) =
    let si =
      if ignore_doc_comment then
        let pcsig_fields =
          List.filter si.pcsig_fields ~f:(fun si ->
              match si.pctf_desc with
              | Pctf_attribute a -> not (doc_attribute a)
              | _ -> true)
        in
        {si with pcsig_fields}
      else si
    in
    Ast_mapper.default_mapper.class_signature m si
  in
  let class_structure (m : Ast_mapper.mapper) (si : class_structure) =
    let si =
      if ignore_doc_comment then
        let pcstr_fields =
          List.filter si.pcstr_fields ~f:(fun si ->
              match si.pcf_desc with
              | Pcf_attribute a -> not (doc_attribute a)
              | _ -> true)
        in
        {si with pcstr_fields}
      else si
    in
    Ast_mapper.default_mapper.class_structure m si
  in
  { Ast_mapper.default_mapper with
    location
  ; attribute
  ; attributes
  ; expr
  ; pat
  ; typ
  ; value_binding
  ; structure_item
  ; signature
  ; structure
  ; class_signature
  ; class_structure }

let mapper_ignore_doc_comment = make_mapper ~ignore_doc_comment:true

let mapper = make_mapper ~ignore_doc_comment:false

let impl c = Mapper.structure (mapper c)

let intf c = Mapper.signature (mapper c)

let use_file c = Mapper.use_file (mapper c)

let equal_impl ~ignore_doc_comments c ast1 ast2 =
  let map =
    if ignore_doc_comments then
      Mapper.structure (mapper_ignore_doc_comment c)
    else Mapper.structure (mapper c)
  in
  Poly.(map ast1 = map ast2)

let equal_intf ~ignore_doc_comments c ast1 ast2 =
  let map =
    if ignore_doc_comments then
      Mapper.signature (mapper_ignore_doc_comment c)
    else Mapper.signature (mapper c)
  in
  Poly.(map ast1 = map ast2)

let equal_use_file ~ignore_doc_comments c ast1 ast2 =
  let map =
    if ignore_doc_comments then
      Mapper.use_file (mapper_ignore_doc_comment c)
    else Mapper.use_file (mapper c)
  in
  Poly.(map ast1 = map ast2)

let make_docstring_mapper c docstrings =
  let doc_attribute = function
    | {attr_name= {txt= "ocaml.doc" | "ocaml.text"; _}; _} -> true
    | _ -> false
  in
  let attribute (m : Ast_mapper.mapper) attr =
    match (attr.attr_name, attr.attr_payload) with
    | ( {txt= ("ocaml.doc" | "ocaml.text") as txt; loc}
      , PStr
          [ { pstr_desc=
                Pstr_eval
                  ( { pexp_desc= Pexp_constant (Pconst_string (doc, None))
                    ; pexp_loc
                    ; pexp_attributes
                    ; _ }
                  , [] )
            ; pstr_loc } ] ) ->
        let doc' = docstring c doc in
        docstrings := (loc, doc) :: !docstrings ;
        { attr_name= {txt; loc}
        ; attr_payload=
            m.payload m
              (PStr
                 [ { pstr_desc=
                       Pstr_eval
                         ( { pexp_desc=
                               Pexp_constant (Pconst_string (doc', None))
                           ; pexp_loc
                           ; pexp_attributes
                           ; pexp_loc_stack= [] }
                         , [] )
                   ; pstr_loc } ])
        ; attr_loc= attr.attr_loc }
    | _ -> Ast_mapper.default_mapper.attribute m attr
  in
  (* sort attributes *)
  let attributes (m : Ast_mapper.mapper) atrs =
    let atrs = List.filter atrs ~f:doc_attribute in
    Ast_mapper.default_mapper.attributes m (sort_attributes atrs)
  in
  {Ast_mapper.default_mapper with attribute; attributes}

let docstrings_impl c s =
  let docstrings = ref [] in
  let (_ : structure) =
    Mapper.structure (make_docstring_mapper c docstrings) s
  in
  !docstrings

let docstrings_intf c s =
  let docstrings = ref [] in
  let (_ : signature) =
    Mapper.signature (make_docstring_mapper c docstrings) s
  in
  !docstrings

let docstrings_use_file c s =
  let docstrings = ref [] in
  let (_ : toplevel_phrase list) =
    Mapper.use_file (make_docstring_mapper c docstrings) s
  in
  !docstrings

type docstring_error =
  | Moved of Location.t * Location.t * string
  | Unstable of Location.t * string

let moved_docstrings c get_docstrings s1 s2 =
  let d1 = get_docstrings c s1 in
  let d2 = get_docstrings c s2 in
  let equal (_, x) (_, y) =
    let b = String.equal (docstring c x) (docstring c y) in
    Caml.Printf.printf "Docstring equal? %b,\n%s\n%s\n" b (docstring c x)
      (docstring c y) ;
    b
  in
  let unstable (x, y) = Unstable (x, y) in
  match List.zip_exn d1 d2 with
  | exception _ ->
      (* We only return the ones that are not in both lists. *)
      (* [l1] contains the ones that disappeared. *)
      let l1 = List.filter d1 ~f:(fun x -> not (List.mem ~equal d2 x)) in
      let l1 = List.map ~f:unstable l1 in
      (* [l2] contains the ones that appeared. *)
      let l2 = List.filter d2 ~f:(fun x -> not (List.mem ~equal d1 x)) in
      let l2 = List.map ~f:unstable l2 in
      List.rev_append l1 l2
  | l ->
      let l = List.filter l ~f:(fun (x, y) -> not (equal x y)) in
      let l1, l2 = List.unzip l in
      let both, l1 =
        List.partition_map l1 ~f:(fun x ->
            match List.find l2 ~f:(equal x) with
            | Some (l, s) -> `Fst (Moved (fst x, l, s))
            | None -> `Snd x)
      in
      let l2 = List.filter l2 ~f:(fun x -> not (List.mem ~equal l1 x)) in
      let l1 = List.map ~f:unstable l1 in
      let l2 = List.map ~f:unstable l2 in
      List.rev_append both (List.rev_append l1 l2)

let moved_docstrings_impl c s1 s2 = moved_docstrings c docstrings_impl s1 s2

let moved_docstrings_intf c s1 s2 = moved_docstrings c docstrings_intf s1 s2

let moved_docstrings_use_file c s1 s2 =
  moved_docstrings c docstrings_use_file s1 s2
