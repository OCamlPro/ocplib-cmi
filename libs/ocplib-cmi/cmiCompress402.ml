
  let rec signature sg = List.map signature_item sg

  and signature_item s =
    match s with
        Sig_value (id, v) -> Sig_value (ident id, value_description v)
      | Sig_type (id, t, rec_status) ->
        Sig_type (ident id,  type_declaration t, rec_status)
      | Sig_typext (id, ec, status) ->
        Sig_typext (ident id, extension_constructor ec, status)
      | Sig_module (id, m, rec_status) ->
        Sig_module (ident id, module_declaration m, rec_status)
      | Sig_modtype (id, m) ->
        Sig_modtype (ident id, modtype_declaration m)
      | Sig_class (id, c, rec_status) ->
        Sig_class (ident id, class_declaration c, rec_status)
      | Sig_class_type (id, c, rec_status) ->
        Sig_class_type (ident id, cltype_declaration c, rec_status)

  and value_description v =
    { val_loc = location v.val_loc;
      val_type = type_expr v.val_type;
      val_kind = value_kind v.val_kind;
      val_attributes = v.val_attributes;
    }

  and value_kind v =
    match v with
        Val_reg
      | Val_unbound -> v
      | Val_prim prim -> Val_prim (primitive prim)
      | Val_ivar (mutable_flag, s) -> Val_ivar (mutable_flag, string s)
      | Val_self (meths, vars, name, self) ->
        let meths = ref (Meths.map (fun (id, ty) -> (ident id, type_expr ty)) !meths)
        in
        let vars = ref (Vars.map (fun (id, mutable_flag, virtual_flag, ty) ->
          (ident id, mutable_flag, virtual_flag, type_expr ty)) !vars) in

        let name = string name in
        let self = type_expr self in
        Val_self (meths, vars, name, self)
      | Val_anc (list, s) -> Val_anc (share list, string s)

  and primitive prim = share { prim with
      Primitive.prim_name = string prim.Primitive.prim_name;
    }

  and constructor_declaration t =
      {
        cd_id = ident t.cd_id;
        cd_args = List.map type_expr t.cd_args;
        cd_res = map_option type_expr t.cd_res;
        cd_loc = location t.cd_loc;
        cd_attributes = t.cd_attributes;
  }

  and label_declaration t =
    {
      ld_id = ident t.ld_id;
      ld_mutable = t.ld_mutable;
      ld_type = type_expr t.ld_type;
      ld_loc = location t.ld_loc;
      ld_attributes = t.ld_attributes;
    }


  and cltype_declaration t =
    {
      clty_params = List.map type_expr t.clty_params;
      clty_type = class_type t.clty_type;
      clty_path = path t.clty_path;
      clty_variance = share t.clty_variance;
      clty_loc = location t.clty_loc;
      clty_attributes = t.clty_attributes;
    }

  and class_type c=
    match c with
        Cty_constr (p, tys, cty) ->
          Cty_constr (path p, List.map type_expr tys, cty)
      | Cty_signature cl -> Cty_signature (class_signature cl)
      | Cty_arrow (label, ty, cl) ->
        Cty_arrow (string label, type_expr ty, class_type cl)

  and class_signature c =
    let vars = Vars.map (fun (mutable_flag, virtual_flag, ty) ->
      (mutable_flag, virtual_flag, type_expr ty)) c.csig_vars in
    { csig_self = type_expr c.csig_self;
      csig_vars = vars;
      csig_concr = share c.csig_concr;
      csig_inher = List.map (fun (p, tys) ->
        (path p, List.map type_expr tys)) c.csig_inher;
    }

  and type_expr_option top =
    match top with
        None -> None | Some ty -> Some (type_expr ty)

  and class_declaration t =
    {
      cty_params = List.map type_expr t.cty_params;
      cty_type = class_type t.cty_type;
      cty_path = path t.cty_path;
      cty_new = type_expr_option t.cty_new;
      cty_variance = share t.cty_variance;
      cty_loc = location t.cty_loc;
      cty_attributes = t.cty_attributes;
    }

  and module_declaration t =
    { md_type = module_type t.md_type;
      md_attributes = t.md_attributes;
      md_loc = location t.md_loc;
    }

  and modtype_declaration t =
    {
      mtd_type = map_option module_type t.mtd_type;
      mtd_attributes = t.mtd_attributes;
      mtd_loc = location t.mtd_loc;
    }

  and module_type t =
    match t with
        Mty_ident p -> share t
      | Mty_signature s -> Mty_signature (signature s)
      | Mty_functor (id, m1, m2) ->
        Mty_functor (ident id, map_option module_type m1, module_type m2)
      | Mty_alias p -> Mty_alias (path p)

  and extension_constructor  ({
    ext_type_path;
    ext_type_params;
    ext_args;
    ext_ret_type;
    ext_private;
    ext_loc;
    ext_attributes } as ec) =
    { ec with
      ext_loc = location ext_loc;
      ext_type_path = path ext_type_path;
      ext_type_params = List.map type_expr ext_type_params;
      ext_args = List.map type_expr ext_args;
      ext_ret_type = type_expr_option ext_ret_type }

  and type_declaration t =
    {
      type_loc = location t.type_loc;
        type_newtype_level = t.type_newtype_level;
        type_params = List.map type_expr t.type_params;
        type_arity = t.type_arity;
        type_kind = type_kind t.type_kind;
        type_private = t.type_private;
        type_manifest = type_expr_option t.type_manifest;
        type_variance = share t.type_variance;
        type_attributes = t.type_attributes;
    }

  and type_kind t =
    match t with
        Type_abstract -> Type_abstract
      | Type_variant vs ->
        Type_variant (List.map constructor_declaration vs)
      | Type_record (fs, re) ->
        Type_record (List.map label_declaration fs, re)
      | Type_open -> t

  (* TODO *)
  and type_expr ty =
    try
      Hashtbl.find type_of_id ty.id
    with Not_found ->
      match ty.desc with
          Tlink ty ->
            let ty' = type_expr ty in
            Hashtbl.add type_of_id ty.id ty';
            ty'
        | Tvar _ | Tunivar _ ->
           let ty' = newty ty in
          Hashtbl.add type_of_id ty.id ty';
          ty'
      | _ ->
          let ty' = newty ty in
          Hashtbl.add type_of_id ty.id ty';
          let desc = match ty.desc with
              Tvar _ | Tunivar _ -> assert false
            | Tarrow ( label, ty1, ty2, commutable) ->
              Tarrow (string label, type_expr ty1, type_expr ty2, commutable)
            | Ttuple tys ->
              Ttuple (List.map type_expr tys)
            | Tconstr (p, tys, abbrev) ->
              Tconstr (path p, List.map type_expr tys, abbrev_memo_ref abbrev)
            | Tobject (ty, refo) ->
(*
              let (tys, ty) = Ctype.flatten_fields ty in
              let build_fields level tys ty =
                List.fold_right
                  (fun (s, k, ty1) ty2 ->
                    incr counter;
                    {
                      desc = Tfield(s, k, ty1, ty2);
                      id = !counter;
                      level = level;
                    }) tys ty
              in
              let ty = build_fields ty.level tys ty in
*)
              let refo =
                match !refo with
                    None -> refo
                  | Some (p, tys) ->
                    ref (Some (path p, List.map type_expr tys))
              in
              let rec iter ty meths =
                match ty.desc with
                    Tfield (name, kind, ty, next) ->
                      let ty = type_expr ty in
                      let meths = (name, kind, ty) :: meths in
(*                      Printf.printf "   %s : %d (%s)\n" name ty.id
                        (match kind with
                            Fpresent -> "present"
                          | Fabsent -> "absent"
                          | Fvar _ -> "var"); *)
                      iter next meths
                  | Tlink _ -> assert false
                  | Tpackage (_, _, _)  -> assert false
                  | Tpoly (_, _) -> assert false
                  | Tvariant _ -> assert false
                  | Tsubst _ -> assert false
                  | Tobject (_, _)  -> assert false
                  | Tconstr (_, _, _) -> assert false
                  | Ttuple _ -> assert false
                  | Tarrow (_, _, _, _) -> assert false
                  | Tunivar _ -> (* print_string "   univar\n" *) ()
                  | Tnil -> remembered := (ty', meths, refo) :: !remembered
                  | Tvar _ -> (* print_string "   var\n"  *) ()

              in
(*              Printf.printf "obj\n"; *)
              iter ty [];
(*              Printf.printf "end\n%!"; *)
              Tobject (type_expr ty, refo)
            | Tfield (s, field_kind, ty, next) ->
              Tfield (string s, share field_kind, type_expr ty, type_expr next)
            | Tnil -> Tnil
            | Tlink _ -> assert false
            | Tsubst ty -> Tsubst (type_expr ty)
            | Tvariant row -> Tvariant (row_desc row)
            | Tpoly (ty, tys) -> Tpoly (type_expr ty, List.map type_expr tys)
            | Tpackage (p, ss, tys) ->
              Tpackage (path p, share ss, List.map type_expr tys)
          in
          ty'.desc <- desc;
          let m = Marshal.to_string { ty' with id = 100 } [] in
          try
            let ty'' = Hashtbl.find hcons m in
            ty'.desc <- Tnil;
            Hashtbl.add type_of_id ty.id ty'';
            ty''
          with Not_found ->
            Hashtbl.add hcons m ty';
            ty'

  and row_desc r =
    { row_fields = List.map (fun (label, rf) ->
      (string label, row_field rf)) r.row_fields;
      row_more = type_expr r.row_more;
      row_bound = r.row_bound;
      row_closed = r.row_closed;
      row_fixed = r.row_fixed;
      row_name = (match r.row_name with
          None -> None
        | Some (p, tys) -> Some (path p, List.map type_expr tys));
      }

  and row_field r =
    match r with
        Rpresent None -> r
      | Rpresent (Some ty) -> Rpresent (Some (type_expr ty))
      | Rabsent -> r
      | Reither (bool1, tys, bool2, refo) ->
        Reither (bool1, List.map type_expr tys, bool2,
                 match !refo with
                     None -> refo
                   | Some rf -> ref (Some (row_field rf)))

  and abbrev_memo_ref a =
    match !a with
        Mnil -> a
      | Mlink a -> abbrev_memo_ref a
      | Mcons (private_flag, p, ty1, ty2, ab) ->
        ref (Mcons (private_flag, path p, type_expr ty1, type_expr ty2, abbrev_memo ab))

  and abbrev_memo a =
    match a with
        Mnil -> a
      | Mlink a -> abbrev_memo !a
      | Mcons (private_flag, p, ty1, ty2, ab) ->
        Mcons (private_flag, path p, type_expr ty1, type_expr ty2, abbrev_memo ab)
