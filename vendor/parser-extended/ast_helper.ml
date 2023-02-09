(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         Alain Frisch, LexiFi                           *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Helpers to produce Parsetree fragments *)

open Asttypes
open Parsetree
open Docstrings

type 'a with_loc = 'a Location.loc
type loc = Location.t

type lid = Longident.t with_loc
type str = string with_loc
type str_opt = string option with_loc
type attrs = attribute list

let default_loc = ref Location.none

let with_default_loc l f =
  Misc.protect_refs [Misc.R (default_loc, l)] f

module Const = struct
  let mk ?(loc = !default_loc) d =
    {pconst_desc = d;
     pconst_loc = loc}

  let integer ?loc ?suffix i = mk ?loc (Pconst_integer (i, suffix))
  let int ?loc ?suffix i = integer ?loc ?suffix (Int.to_string i)
  let int32 ?loc ?(suffix='l') i = integer ?loc ~suffix (Int32.to_string i)
  let int64 ?loc ?(suffix='L') i = integer ?loc ~suffix (Int64.to_string i)
  let nativeint ?loc ?(suffix='n') i =
    integer ?loc ~suffix (Nativeint.to_string i)
  let float ?loc ?suffix f = mk ?loc (Pconst_float (f, suffix))
  let char ?loc c = mk ?loc (Pconst_char c)
  let string ?quotation_delimiter ?(loc= !default_loc) s =
    mk ~loc (Pconst_string (s, loc, quotation_delimiter))
end

module Attr = struct
  let mk ?(loc= !default_loc) name payload =
    { attr_name = name;
      attr_payload = payload;
      attr_loc = loc }
end

module Typ = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {ptyp_desc = d;
     ptyp_loc = loc;
     ptyp_loc_stack = [];
     ptyp_attributes = attrs}

  let attr d a = {d with ptyp_attributes = d.ptyp_attributes @ [a]}

  let any ?loc ?attrs () = mk ?loc ?attrs Ptyp_any
  let var ?loc ?attrs a = mk ?loc ?attrs (Ptyp_var a)
  let arrow ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_arrow (a, b))
  let tuple ?loc ?attrs a = mk ?loc ?attrs (Ptyp_tuple a)
  let constr ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_constr (a, b))
  let object_ ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_object (a, b))
  let class_ ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_class (a, b))
  let alias ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_alias (a, b))
  let variant ?loc ?attrs a b c = mk ?loc ?attrs (Ptyp_variant (a, b, c))
  let poly ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_poly (a, b))
  let package ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_package (a, b))
  let extension ?loc ?attrs a = mk ?loc ?attrs (Ptyp_extension a)
end

module Pat = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {ppat_desc = d;
     ppat_loc = loc;
     ppat_loc_stack = [];
     ppat_attributes = attrs}
  let attr d a = {d with ppat_attributes = d.ppat_attributes @ [a]}

  let any ?loc ?attrs () = mk ?loc ?attrs Ppat_any
  let var ?loc ?attrs a = mk ?loc ?attrs (Ppat_var a)
  let alias ?loc ?attrs a b = mk ?loc ?attrs (Ppat_alias (a, b))
  let constant ?loc ?attrs a = mk ?loc ?attrs (Ppat_constant a)
  let interval ?loc ?attrs a b = mk ?loc ?attrs (Ppat_interval (a, b))
  let tuple ?loc ?attrs a = mk ?loc ?attrs (Ppat_tuple a)
  let construct ?loc ?attrs a b = mk ?loc ?attrs (Ppat_construct (a, b))
  let variant ?loc ?attrs a b = mk ?loc ?attrs (Ppat_variant (a, b))
  let record ?loc ?attrs a b = mk ?loc ?attrs (Ppat_record (a, b))
  let array ?loc ?attrs a = mk ?loc ?attrs (Ppat_array a)
  let list ?loc ?attrs a = mk ?loc ?attrs (Ppat_list a)
  let or_ ?loc ?attrs a = mk ?loc ?attrs (Ppat_or a)
  let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Ppat_constraint (a, b))
  let type_ ?loc ?attrs a = mk ?loc ?attrs (Ppat_type a)
  let lazy_ ?loc ?attrs a = mk ?loc ?attrs (Ppat_lazy a)
  let unpack ?loc ?attrs a b = mk ?loc ?attrs (Ppat_unpack (a, b))
  let open_ ?loc ?attrs a b = mk ?loc ?attrs (Ppat_open (a, b))
  let exception_ ?loc ?attrs a = mk ?loc ?attrs (Ppat_exception a)
  let extension ?loc ?attrs a = mk ?loc ?attrs (Ppat_extension a)
  let cons ?loc ?attrs a = mk ?loc ?attrs (Ppat_cons a)
end

module Exp = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {pexp_desc = d;
     pexp_loc = loc;
     pexp_loc_stack = [];
     pexp_attributes = attrs}
  let attr d a = {d with pexp_attributes = d.pexp_attributes @ [a]}

  let ident ?loc ?attrs a = mk ?loc ?attrs (Pexp_ident a)
  let constant ?loc ?attrs a = mk ?loc ?attrs (Pexp_constant a)
  let let_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_let (a, b))
  let fun_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pexp_fun (a, b, c, d))
  let function_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_function a)
  let apply ?loc ?attrs a b = mk ?loc ?attrs (Pexp_apply (a, b))
  let match_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_match (a, b))
  let try_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_try (a, b))
  let tuple ?loc ?attrs a = mk ?loc ?attrs (Pexp_tuple a)
  let construct ?loc ?attrs a b = mk ?loc ?attrs (Pexp_construct (a, b))
  let variant ?loc ?attrs a b = mk ?loc ?attrs (Pexp_variant (a, b))
  let record ?loc ?attrs a b = mk ?loc ?attrs (Pexp_record (a, b))
  let field ?loc ?attrs a b = mk ?loc ?attrs (Pexp_field (a, b))
  let setfield ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_setfield (a, b, c))
  let array ?loc ?attrs a = mk ?loc ?attrs (Pexp_array a)
  let list ?loc ?attrs a = mk ?loc ?attrs (Pexp_list a)
  let ifthenelse ?loc ?attrs a b = mk ?loc ?attrs (Pexp_ifthenelse (a, b))
  let sequence ?loc ?attrs a b = mk ?loc ?attrs (Pexp_sequence (a, b))
  let while_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_while (a, b))
  let for_ ?loc ?attrs a b c d e = mk ?loc ?attrs (Pexp_for (a, b, c, d, e))
  let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_constraint (a, b))
  let coerce ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_coerce (a, b, c))
  let send ?loc ?attrs a b = mk ?loc ?attrs (Pexp_send (a, b))
  let new_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_new a)
  let setinstvar ?loc ?attrs a b = mk ?loc ?attrs (Pexp_setinstvar (a, b))
  let indexop_access ?loc ?attrs pia_lhs pia_kind pia_paren pia_rhs =
    mk ?loc ?attrs (Pexp_indexop_access {pia_lhs; pia_kind; pia_paren; pia_rhs})
  let override ?loc ?attrs a = mk ?loc ?attrs (Pexp_override a)
  let letmodule ?loc ?attrs a b c= mk ?loc ?attrs (Pexp_letmodule (a, b, c))
  let letexception ?loc ?attrs a b = mk ?loc ?attrs (Pexp_letexception (a, b))
  let assert_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_assert a)
  let lazy_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_lazy a)
  let poly ?loc ?attrs a b = mk ?loc ?attrs (Pexp_poly (a, b))
  let object_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_object a)
  let newtype ?loc ?attrs a b = mk ?loc ?attrs (Pexp_newtype (a, b))
  let pack ?loc ?attrs a = mk ?loc ?attrs (Pexp_pack a)
  let open_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_open (a, b))
  let letopen ?loc ?attrs a b = mk ?loc ?attrs (Pexp_letopen (a, b))
  let letop ?loc ?attrs let_ ands body =
    mk ?loc ?attrs (Pexp_letop {let_; ands; body})
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pexp_extension a)
  let unreachable ?loc ?attrs () = mk ?loc ?attrs Pexp_unreachable
  let hole  ?loc ?attrs () = mk ?loc ?attrs Pexp_hole
  let beginend ?loc ?attrs a = mk ?loc ?attrs (Pexp_beginend a)
  let cons ?loc ?attrs a = mk ?loc ?attrs (Pexp_cons a)
  let prefix ?loc ?attrs a b = mk ?loc ?attrs (Pexp_prefix (a, b))
  let infix ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_infix (a, b, c))

  let case lhs ?guard rhs =
    {
     pc_lhs = lhs;
     pc_guard = guard;
     pc_rhs = rhs;
    }

  let binding_op op pat exp loc =
    {
      pbop_op = op;
      pbop_pat = pat;
      pbop_exp = exp;
      pbop_loc = loc;
    }
end

module Mty = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {pmty_desc = d; pmty_loc = loc; pmty_attributes = attrs}
  let attr d a = {d with pmty_attributes = d.pmty_attributes @ [a]}

  let ident ?loc ?attrs a = mk ?loc ?attrs (Pmty_ident a)
  let alias ?loc ?attrs a = mk ?loc ?attrs (Pmty_alias a)
  let signature ?loc ?attrs a = mk ?loc ?attrs (Pmty_signature a)
  let functor_ ?loc ?attrs a b = mk ?loc ?attrs (Pmty_functor (a, b))
  let with_ ?loc ?attrs a b = mk ?loc ?attrs (Pmty_with (a, b))
  let typeof_ ?loc ?attrs a = mk ?loc ?attrs (Pmty_typeof a)
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pmty_extension a)
end

module Mod = struct
let mk ?(loc = !default_loc) ?(attrs = []) d =
  {pmod_desc = d; pmod_loc = loc; pmod_attributes = attrs}
  let attr d a = {d with pmod_attributes = d.pmod_attributes @ [a]}

  let ident ?loc ?attrs x = mk ?loc ?attrs (Pmod_ident x)
  let structure ?loc ?attrs x = mk ?loc ?attrs (Pmod_structure x)
  let functor_ ?loc ?attrs arg body =
    mk ?loc ?attrs (Pmod_functor (arg, body))
  let apply ?loc ?attrs m1 m2 = mk ?loc ?attrs (Pmod_apply (m1, m2))
  let constraint_ ?loc ?attrs m mty = mk ?loc ?attrs (Pmod_constraint (m, mty))
  let unpack ?loc ?attrs a b c = mk ?loc ?attrs (Pmod_unpack (a, b, c))
  let gen_apply ?loc ?attrs a b = mk ?loc ?attrs (Pmod_gen_apply (a, b))
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pmod_extension a)
  let hole ?loc ?attrs () = mk ?loc ?attrs Pmod_hole
end

module Sig = struct
  let mk ?(loc = !default_loc) d = {psig_desc = d; psig_loc = loc}

  let value ?loc a = mk ?loc (Psig_value a)
  let type_ ?loc rec_flag a = mk ?loc (Psig_type (rec_flag, a))
  let type_subst ?loc a = mk ?loc (Psig_typesubst a)
  let type_extension ?loc a = mk ?loc (Psig_typext a)
  let exception_ ?loc a = mk ?loc (Psig_exception a)
  let module_ ?loc a = mk ?loc (Psig_module a)
  let mod_subst ?loc a = mk ?loc (Psig_modsubst a)
  let rec_module ?loc a = mk ?loc (Psig_recmodule a)
  let modtype ?loc a = mk ?loc (Psig_modtype a)
  let modtype_subst ?loc a = mk ?loc (Psig_modtypesubst a)
  let open_ ?loc a = mk ?loc (Psig_open a)
  let include_ ?loc a = mk ?loc (Psig_include a)
  let class_ ?loc a = mk ?loc (Psig_class a)
  let class_type ?loc a = mk ?loc (Psig_class_type a)
  let extension ?loc ?(attrs = []) a = mk ?loc (Psig_extension (a, attrs))
  let attribute ?loc a = mk ?loc (Psig_attribute a)
  let text txt =
    let f_txt = List.filter (fun ds -> docstring_body ds <> "") txt in
    List.map
      (fun ds -> attribute ~loc:(docstring_loc ds) (text_attr ds))
      f_txt
end

module Str = struct
  let mk ?(loc = !default_loc) d = {pstr_desc = d; pstr_loc = loc}

  let eval ?loc ?(attrs = []) a = mk ?loc (Pstr_eval (a, attrs))
  let value ?loc a = mk ?loc (Pstr_value a)
  let primitive ?loc a = mk ?loc (Pstr_primitive a)
  let type_ ?loc rec_flag a = mk ?loc (Pstr_type (rec_flag, a))
  let type_extension ?loc a = mk ?loc (Pstr_typext a)
  let exception_ ?loc a = mk ?loc (Pstr_exception a)
  let module_ ?loc a = mk ?loc (Pstr_module a)
  let rec_module ?loc a = mk ?loc (Pstr_recmodule a)
  let modtype ?loc a = mk ?loc (Pstr_modtype a)
  let open_ ?loc a = mk ?loc (Pstr_open a)
  let class_ ?loc a = mk ?loc (Pstr_class a)
  let class_type ?loc a = mk ?loc (Pstr_class_type a)
  let include_ ?loc a = mk ?loc (Pstr_include a)
  let extension ?loc ?(attrs = []) a = mk ?loc (Pstr_extension (a, attrs))
  let attribute ?loc a = mk ?loc (Pstr_attribute a)
  let text txt =
    let f_txt = List.filter (fun ds -> docstring_body ds <> "") txt in
    List.map
      (fun ds -> attribute ~loc:(docstring_loc ds) (text_attr ds))
      f_txt
end

module Cl = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {
     pcl_desc = d;
     pcl_loc = loc;
     pcl_attributes = attrs;
    }
  let attr d a = {d with pcl_attributes = d.pcl_attributes @ [a]}

  let constr ?loc ?attrs a b = mk ?loc ?attrs (Pcl_constr (a, b))
  let structure ?loc ?attrs a = mk ?loc ?attrs (Pcl_structure a)
  let fun_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pcl_fun (a, b, c, d))
  let apply ?loc ?attrs a b = mk ?loc ?attrs (Pcl_apply (a, b))
  let let_ ?loc ?attrs a b = mk ?loc ?attrs (Pcl_let (a, b))
  let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pcl_constraint (a, b))
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pcl_extension a)
  let open_ ?loc ?attrs a b = mk ?loc ?attrs (Pcl_open (a, b))
end

module Cty = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {
     pcty_desc = d;
     pcty_loc = loc;
     pcty_attributes = attrs;
    }
  let attr d a = {d with pcty_attributes = d.pcty_attributes @ [a]}

  let constr ?loc ?attrs a b = mk ?loc ?attrs (Pcty_constr (a, b))
  let signature ?loc ?attrs a = mk ?loc ?attrs (Pcty_signature a)
  let arrow ?loc ?attrs a b = mk ?loc ?attrs (Pcty_arrow (a, b))
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pcty_extension a)
  let open_ ?loc ?attrs a b = mk ?loc ?attrs (Pcty_open (a, b))
end

module Ctf = struct
  let mk ?(loc = !default_loc) ?(attrs = [])
           ?(docs = empty_docs) d =
    {
     pctf_desc = d;
     pctf_loc = loc;
     pctf_attributes = add_docs_attrs docs attrs;
    }

  let inherit_ ?loc ?attrs a = mk ?loc ?attrs (Pctf_inherit a)
  let val_ ?loc ?attrs a b c = mk ?loc ?attrs (Pctf_val (a, b, c))
  let method_ ?loc ?attrs a b c = mk ?loc ?attrs (Pctf_method (a, b, c))
  let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pctf_constraint (a, b))
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pctf_extension a)
  let attribute ?loc a = mk ?loc (Pctf_attribute a)
  let text txt =
   let f_txt = List.filter (fun ds -> docstring_body ds <> "") txt in
     List.map
      (fun ds -> attribute ~loc:(docstring_loc ds) (text_attr ds))
      f_txt

  let attr d a = {d with pctf_attributes = d.pctf_attributes @ [a]}

end

module Cf = struct
  let mk ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) d =
    {
     pcf_desc = d;
     pcf_loc = loc;
     pcf_attributes = add_docs_attrs docs attrs;
    }

  let inherit_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcf_inherit (a, b, c))
  let val_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcf_val (a, b, c))
  let method_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcf_method (a, b, c))
  let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pcf_constraint (a, b))
  let initializer_ ?loc ?attrs a = mk ?loc ?attrs (Pcf_initializer a)
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pcf_extension a)
  let attribute ?loc a = mk ?loc (Pcf_attribute a)
  let text txt =
    let f_txt = List.filter (fun ds -> docstring_body ds <> "") txt in
    List.map
      (fun ds -> attribute ~loc:(docstring_loc ds) (text_attr ds))
      f_txt

  let virtual_ ct = Cfk_virtual ct
  let concrete o e = Cfk_concrete (o, e)

  let attr d a = {d with pcf_attributes = d.pcf_attributes @ [a]}

end

module Val = struct
  let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
        ?(prim = []) name typ =
    {
     pval_name = name;
     pval_type = typ;
     pval_attributes = add_docs_attrs docs attrs;
     pval_loc = loc;
     pval_prim = prim;
    }
end

module Md = struct
  let mk ?(loc = !default_loc) ?ext ?(attrs_ext = []) ?(attrs_end = [])
        ?(docs = empty_docs) ?(text = []) name typ =
    {
     pmd_name = name;
     pmd_type = typ;
     pmd_ext_attributes = (ext, attrs_ext);
     pmd_attributes_end =
       add_text_attrs text (add_docs_attrs docs attrs_end);
     pmd_loc = loc;
    }
end

module Ms = struct
  let mk ?(loc = !default_loc) ?ext ?(attrs_ext = []) ?(attrs_end = [])
        ?(docs = empty_docs) ?(text = []) name syn =
    {
     pms_name = name;
     pms_manifest = syn;
     pms_ext_attributes= (ext, attrs_ext);
     pms_attributes_end =
       add_text_attrs text (add_docs_attrs docs attrs_end);
     pms_loc = loc;
    }
end

module Mtd = struct
  let mk ?(loc = !default_loc) ?ext ?(attrs_ext = [])  ?(attrs_end = [])
        ?(docs = empty_docs) ?(text = []) ?typ name =
    {
     pmtd_name = name;
     pmtd_type = typ;
     pmtd_ext_attributes= (ext, attrs_ext);
     pmtd_attributes_end =
       add_text_attrs text (add_docs_attrs docs attrs_end);
     pmtd_loc = loc;
    }
end

module Mb = struct
  let mk ?(loc = !default_loc) ?ext ?(attrs_ext = [])  ?(attrs_end = [])
        ?(docs = empty_docs) ?(text = []) name expr =
    {
     pmb_name = name;
     pmb_expr = expr;
     pmb_ext_attributes = ext, attrs_ext;
     pmb_attributes_end =
       add_text_attrs text (add_docs_attrs docs attrs_end);
     pmb_loc = loc;
    }
end

module Opn = struct
  let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
        ?(override = Fresh) expr =
    {
     popen_expr = expr;
     popen_override = override;
     popen_loc = loc;
     popen_attributes = add_docs_attrs docs attrs;
    }
end

module Incl = struct
  let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs) mexpr =
    {
     pincl_mod = mexpr;
     pincl_loc = loc;
     pincl_attributes = add_docs_attrs docs attrs;
    }

end

module Ci = struct
  let mk ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(text = [])
        ?(virt = Concrete) ?(params = []) name expr =
    {
     pci_virt = virt;
     pci_params = params;
     pci_name = name;
     pci_expr = expr;
     pci_attributes =
       add_text_attrs text (add_docs_attrs docs attrs);
     pci_loc = loc;
    }
end

module Type = struct
  let mk ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(text = [])
      ?(params = [])
      ?(cstrs = [])
      ?(kind = Ptype_abstract)
      ?(priv = Public)
      ?manifest
      name =
    {
     ptype_name = name;
     ptype_params = params;
     ptype_cstrs = cstrs;
     ptype_kind = kind;
     ptype_private = priv;
     ptype_manifest = manifest;
     ptype_attributes =
       add_text_attrs text (add_docs_attrs docs attrs);
     ptype_loc = loc;
    }

  let constructor ?(loc = !default_loc) ?(attrs = []) ?(info = empty_info)
        ?(vars = []) ?(args = Pcstr_tuple []) ?res name =
    {
     pcd_name = name;
     pcd_vars = vars;
     pcd_args = args;
     pcd_res = res;
     pcd_loc = loc;
     pcd_attributes = add_info_attrs info attrs;
    }

  let field ?(loc = !default_loc) ?(attrs = []) ?(info = empty_info)
        ?(mut = Immutable) name typ =
    {
     pld_name = name;
     pld_mutable = mut;
     pld_type = typ;
     pld_loc = loc;
     pld_attributes = add_info_attrs info attrs;
    }

end

(** Type extensions *)
module Te = struct
  let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
        ?(params = []) ?(priv = Public) path constructors =
    {
     ptyext_path = path;
     ptyext_params = params;
     ptyext_constructors = constructors;
     ptyext_private = priv;
     ptyext_loc = loc;
     ptyext_attributes = add_docs_attrs docs attrs;
    }

  let mk_exception ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
      constructor =
    {
     ptyexn_constructor = constructor;
     ptyexn_loc = loc;
     ptyexn_attributes = add_docs_attrs docs attrs;
    }

  let constructor ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(info = empty_info) name kind =
    {
     pext_name = name;
     pext_kind = kind;
     pext_loc = loc;
     pext_attributes = add_docs_attrs docs (add_info_attrs info attrs);
    }

  let decl ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
         ?(info = empty_info) ?(vars = []) ?(args = Pcstr_tuple []) ?res name =
    {
     pext_name = name;
     pext_kind = Pext_decl(vars, args, res);
     pext_loc = loc;
     pext_attributes = add_docs_attrs docs (add_info_attrs info attrs);
    }

  let rebind ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(info = empty_info) name lid =
    {
     pext_name = name;
     pext_kind = Pext_rebind lid;
     pext_loc = loc;
     pext_attributes = add_docs_attrs docs (add_info_attrs info attrs);
    }

end

module Csig = struct
  let mk self fields =
    {
     pcsig_self = self;
     pcsig_fields = fields;
    }
end

module Cstr = struct
  let mk self fields =
    {
     pcstr_self = self;
     pcstr_fields = fields;
    }
end

(** Row fields *)
module Rf = struct
  let mk ?(loc = !default_loc) ?(attrs = []) desc = {
    prf_desc = desc;
    prf_loc = loc;
    prf_attributes = attrs;
  }
  let tag ?loc ?attrs label const tys =
    mk ?loc ?attrs (Rtag (label, const, tys))
  let inherit_?loc ty =
    mk ?loc (Rinherit ty)
end

(** Object fields *)
module Of = struct
  let mk ?(loc = !default_loc) ?(attrs=[]) desc = {
    pof_desc = desc;
    pof_loc = loc;
    pof_attributes = attrs;
  }
  let tag ?loc ?attrs label ty =
    mk ?loc ?attrs (Otag (label, ty))
  let inherit_ ?loc ty =
    mk ?loc (Oinherit ty)
end
