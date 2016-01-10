(*i camlp4use: "pa_extend.cmo" i*)
(*i camlp4deps: "parsing/grammar.cma" i*)

open Names
open Pp
open Pcoq
open Genarg
open Term
open Topconstr
open Libnames
open Tactics
open Tacticals
open Termops
open Namegen
open Recordops
open Tacmach
open Coqlib
open Glob_term
open Util
open Evd
open Extend
open Goptions
open Tacexpr
open Tacinterp
open Constr
open Tactic
open Extraargs
open Ppconstr
open Printer
open Pp
open Errors
open Util
open Tacexpr
open Genredexpr
open Constrexpr
open Libnames
open Tok
open Compat
open Misctypes
open Locus
open Decl_kinds

open Pcoq

DECLARE PLUGIN "mod_reduce"

(*
module type MOD_REDUCE =
sig
  val declare_delta_collection : Names.Id.t -> Names.Constant.t list -> unit
  val lookup : Names.Id.t -> Names.Constant.t list
end
*)
module MOD_REDUCE =
struct

  type collection =
    | List of Libnames.reference list

  let data : collection Names.Idmap.t ref = ref Names.Idmap.empty

  let new_delta_collection : Names.Id.t * collection -> Libobject.obj
    = Libobject.(declare_object
                 { (default_object "MOD_REDUCE_DELTA_COLLECTION") with
                   cache_function = (fun (_,_) ->
                     (** TODO: I don't know what to do here. **)
                     ())
                 ; load_function = (fun i (obj_name,(name,value)) ->
                     data := Names.Idmap.add name value !data)
                 ; classify_function = (fun i -> Substitute i)
                 ; subst_function = (fun (s,(id,col)) ->
                     (id,col))
                 })

  let declare_delta_collection (id : Names.Id.t) (lst : Libnames.reference list) : unit =
    let lst = List lst (* (List.fold_left (fun acc x ->
        Pp.msg_debug (Libnames.pr_reference x) ;
        match x with
        | Qualid _ -> x :: acc
        | _ -> assert false) [] lst) *) in
    data := Names.Idmap.add id lst !data ;
    ignore (Lib.add_leaf id (new_delta_collection (id, lst)))

  let lookup id =
    match Names.Idmap.find id !data with
      List x -> x
end

open Genredexpr
open Prim

GEXTEND Gram
  GLOBAL: red_expr ;
  red_flag:
    [ [ IDENT "beta" -> FBeta
      | IDENT "iota" -> FIota
      | IDENT "zeta" -> FZeta
      | IDENT "delta"; d = delta_flag -> d
    ] ]
  ;
  red_global:
    [ [ s = smart_global -> [s]
      | IDENT "?" ; s = ident -> List.map (fun x -> Misctypes.AN x)
          (MOD_REDUCE.lookup s)
      ] ]
  ;
  delta_flag:
    [ [ "["; idl = LIST1 red_global; "]" -> FConst (List.flatten idl)
      | -> FDeltaBut []
    ] ]
  ;
  strategy_flag:
    [ [ s = LIST1 red_flag -> Redops.make_red_flag s
      | d = delta_flag -> Redops.make_red_flag [FBeta;FIota;FZeta;d]
    ] ]
  ;

  red_expr:
  [ [ IDENT "mod_reduce" ; strat = strategy_flag -> Cbv strat
    ] ] ;
END;;

VERNAC COMMAND EXTEND Declare_red_collection
  | [ "Declare" "Delta" "Collection" ident(s) ":=" "[" global_list(gls) "]" ] ->
    [ MOD_REDUCE.declare_delta_collection s gls ]
END;;
