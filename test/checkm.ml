(*---------------------------------------------------------------------------
   Copyright (c) 2013 The gg programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

type 'a printer = Format.formatter -> 'a -> unit
type 'a gen = (int -> Random.State.t -> 'a) * 'a printer
let (>>) x f = f x

module Test = struct

  module Log = struct
    type arg = unit printer
    type id = string option
    type backtrace = string list
    type cmp = [ `Gt | `Geq | `Lt | `Leq | `Eq | `Neq | `Peq | `Pneq]
    type labels = (string * int) list
    type failure = [
      | `Exn_uncaught of id * exn * arg option
      | `Exn_none of id * arg option * arg option
      | `Comparison of id * cmp * (arg * arg) option
      | `Holds of id * arg option
      | `For_all of id * int * (arg option * failure * backtrace) list
	    * labels option
      | `Other of string ]

    type murder = [
      | `For_all of id * int * labels option
      | `Other of string ]

    type run_info =
	{ run_test_count : int;
	  run_fail_stop : bool;
	  run_verbose : int;
	  run_gen_seed : int;
	  run_gen_size : int;
	  run_gen_kill : int;
	  run_gen_success : int;
	  run_gen_falsifiers : int; }

    type run_result =
	{ run_time : float;
	  run_okay : int;
	  run_fail : int;
	  run_kill : int;
	  run_skip : int;
	  run_todo : int }

    type test_info =
	{ test_name : string;
	  test_num : int;
	  (* test_seed : int *) }

    type check_info =
	{ check_id : string;
	  check_num : int }

    type test_result = [
      | `Okay of float
      | `Fail of failure * backtrace
      | `Kill of murder * backtrace
      | `Skip of string * backtrace
      | `Todo of string * backtrace ]

    type event = [
      | `No_exec of string list
      | `Run_start
      | `Run_end of run_result
      | `Test_start of test_info
      | `Test_end of test_info * test_result
      | `Print of arg ]

    (* Pretty print paragraphs. *)

    let r_paragraphs = Str.regexp "[ \t]*\n[ \t]*\n[ \n\t]*"
    let r_words = Str.regexp "[ \t]+\\|[ \t]*\n[ \t]*"
    let paragraphs s =                            (* paragraphs from string. *)
      let paragraphs = Str.split r_paragraphs s in
      List.map (Str.split r_words) paragraphs

    let rec pp_list pp_sep pp_v ppf = function
      | v :: l ->
	  pp_v ppf v;
	  if (l <> []) then (pp_sep ppf (); pp_list pp_sep pp_v ppf l)
      | [] -> ()

    let pp_paragraphs ppf s =
      let words ppf wl =
	Format.fprintf ppf "@[";
	pp_list Format.pp_print_space Format.pp_print_string ppf wl;
	Format.fprintf ppf "@]";
      in
      let paras = pp_list (fun pp () -> Format.fprintf ppf "@ @ ") words in
      Format.fprintf ppf "@[<v>%a@]" paras (paragraphs s)

    (* Misc pretty printers. *)

    let prf ppf fmt = Format.fprintf ppf fmt                   (* short cut. *)

    let cmp_to_string = function
      | `Lt -> "<" | `Leq -> "<="  | `Eq -> "=" | `Neq -> "<>"
      | `Gt -> ">" | `Geq -> ">=" | `Peq -> "==" | `Pneq -> "!="

    let pp_id ppf = function None -> () | Some v -> prf ppf "%s: " v
    let pp_backtrace ppf = function
      | [] ->
	  prf ppf "%a" pp_paragraphs
	    "No stack trace, did you compile and link with -g ? Is the \
	     last check of the sequence C.success ? are you using bytecode ?"
      | bt ->
	  prf ppf "@[<v>%a@]"
	    (pp_list Format.pp_print_space Format.pp_print_string) bt

    let pp_arg ppf v = match v with
    | None -> () | Some v -> prf ppf "@[@ on@ value@ %a@]" v ()

    (* Pretty printing failures. *)

    let rec pp_falsifiers ri ppf falsifiers =
      let pp_arg' ppf v = match v with
      | None -> () | Some v -> prf ppf "@[On@ value@ %a@]@ " v ()
      in
      let pp_case ppf (arg, f, bt) = prf ppf "@[<v>%a%a@ %a@]@ "
	  pp_arg' arg (pp_failure ri) f pp_backtrace bt
      in
      prf ppf "@[<v>%a@]" (pp_list Format.pp_print_space pp_case) falsifiers

    and pp_failure ri ppf f =
      let pr fmt = prf ppf fmt in
      match f with
      | `Exn_uncaught (id, e, v) -> pr "@[%aUncaught@ exception@ %s%a.@]"
	    pp_id id (Printexc.to_string e) pp_arg v
      | `Exn_none (id, a, r) ->
	  let pr_ret ppf = function None -> prf ppf "@ a@ value"
	    | Some v -> prf ppf " %a" v ()
	  in
	  pr "@[%aExpected@ an@ exception%a@ but%a@ was@ returned.@]" pp_id id
	    pp_arg a pr_ret r
      | `Comparison (id, cmp, v) ->
	  begin match v with
	  | None -> pr "@[%aComparison@ predicate %s@ is@ false.@]"
		pp_id id (cmp_to_string cmp)
	  | Some (v, v') -> pr "@[%a%a %s@ %a@ is@ false.@]"
		pp_id id v () (cmp_to_string cmp) v' ()
	  end
      | `Holds (id, v) ->
	  pr "@[%aPredicate@ is@ false%a.@]" pp_id id pp_arg v
      | `For_all (id, okay, falsifiers, labels) ->
	  pr "@[<v1>@[%aFor@ all@ fails@ after@ %d/%d@ successes.@]@ %a@]"
	    pp_id id okay ri.run_gen_success (pp_falsifiers ri) falsifiers
      | `Other s -> pr "@[%a@]" pp_paragraphs s

    (* Pretty print murders *)

    let pp_murder ri ppf m =
      let pr fmt = prf ppf fmt in
      match m with
      |  `For_all (id, okay, labels) ->
	  pr "@[For all@ was@ killed, %d/%d@ cases@ passed@ after %d@ \
	      generations.@]"
            okay ri.run_gen_success ri.run_gen_kill
      | `Other s -> pr "@[%a@]" pp_paragraphs s

    (* Pretty print test ends *)

    let min_time = 0.001             (* timings < min_time are not reported. *)

    let pp_test_end ri i ppf res =
      let pr fmt = prf ppf fmt in
      let pr_time ppf t = if t >= min_time then prf ppf "@ (%.4gs)" t in
      match res with
      | `Okay t ->
	  pr "@[[okay] @[%s%a@]@]" i.test_name pr_time t
      | `Skip (s, bt) ->
	  pr "@[[skip] @[<v>%s@ %a@ %a@]@]"
	    i.test_name pp_paragraphs s pp_backtrace bt
      | `Todo (s, bt) ->
	  pr "@[[todo] @[<v>%s@ %a@ %a@]@]"
	    i.test_name pp_paragraphs s pp_backtrace bt
      | `Kill (m, bt) ->
	  pr "@[[kill] @[<v>%s@ %a@ %a@]@]"
	    i.test_name (pp_murder ri) m pp_backtrace bt
      | `Fail (f, bt) ->
	  pr "@[[fail] @[<v>%s@ %a@ %a@]@]"
	    i.test_name (pp_failure ri) f pp_backtrace bt

    (* Loggers. *)

    let batch_log ppf ri e =
      let pr fmt = prf ppf fmt in
      match e with
      | `No_exec l ->
	  let pr_l = pp_list Format.pp_print_space Format.pp_print_string in
	  if l <> [] then pr "@[<v>%a@ @]" pr_l l
      | `Run_start ->
	  if ri.run_verbose < 1 then () else
	  pr "@[Running %d tests@ with@ random seed %d@ and@ size hint %d.@]@."
	    ri.run_test_count ri.run_gen_seed ri.run_gen_size;
      | `Run_end r ->
	  if ri.run_verbose < 1 then () else
	  let result r = if r.run_fail = 0 then "Success" else "FAILURE" in
	  pr "@.@[%d succeded,@ %d failed,@ %d killed,@ %d skipped,\
	      @ %d todo@]@."
	     r.run_okay r.run_fail r.run_kill r.run_skip r.run_todo;
	  pr "@[%s@ " (result r);
	  if (r.run_time >= min_time) then pr "in@ %.4gs@ " r.run_time;
	  pr "with@ random@ seed %d@ and@ size hint %d.@]@."
	    ri.run_gen_seed ri.run_gen_size
      | `Test_start i -> ()
      | `Test_end (i, res) -> pr "%a@." (pp_test_end ri i) res
      | `Print v -> pr "%a" v ()

    let term_log ppf ri e =
      let pr fmt = prf ppf fmt in
      match e with
      | `Test_start i ->
	  pr "@[@<0>%s[@<0>%sexec@<0>%s] @[(%d/%d)@ %s@]@?"
	    "\x1B7" "\x1B[5m" "\x1B[0m" i.test_num ri.run_test_count i.test_name
      | `Test_end r ->
	  pr "@<0>%s" "\x1B8\x1B[K"; (* kill the line. *)
	  begin match r with
	  | (i, `Fail (f, bt)) ->
	      pr "@<0>%s[fail]@<0>%s @[<v>%s@ %a@ %a@]@\n@."
		"\x1B[7m" "\x1B[0m" i.test_name (pp_failure ri) f pp_backtrace
		bt
	  | _ -> batch_log ppf ri e
	  end
      | e -> batch_log ppf ri e
  end

  (* Tests *)

  type log = Log.run_info -> Log.event -> unit
  type run =
      { info : Log.run_info;
	log : log;
	rstate : Random.State.t;
	mutable tnum : int;   (* current test number in the list of tests. *)
	mutable okay : int;
	mutable fail : int;
	mutable kill : int;
	mutable skip : int;
	mutable todo : int; }

  type t = string * (run -> unit)

  let create n f = n, (fun t -> ignore (f t))
  let name (n, _) = n
  let func (_, f) = f

  (* Global test list *)

  let list : t list ref = ref []
  let add_test n f = list := (create n f) :: !list

  (* Stopping tests *)

  exception Fail of Log.failure
  exception Kill of Log.murder
  exception Skip of string
  exception Todo of string

  let fail s = raise (Fail (`Other s))
  let kill s = raise (Kill (`Other s))
  let skip s = raise (Skip s)
  let todo s = raise (Todo s)

  (* Loggers *)
  let term_log = Log.term_log
  let batch_log = Log.batch_log

  (* Runs *)

  type run_conf =
      { selectors : string list;
	sort : (string -> string -> int) option;
	no_exec : bool;
	fail_stop : bool;
	verbose : int;
	gen_seed : int option;
	gen_size : int;
	gen_kill : int option;
	gen_success : int;
	gen_falsifiers : int }

  let run_conf =
    { selectors = []; sort = Some Stdlib.compare; no_exec = false;
      fail_stop = false; verbose = 1; gen_seed = None; gen_size = 100;
      gen_kill = None; gen_success = 1000; gen_falsifiers = 3; }

  let select_tests prefixes tests = match prefixes with
  | [] -> tests, List.length tests
  | l ->
      let any l = String.concat "\\|" l in
      let prefix p = (Str.quote p) ^ ".*" in
      let r = Str.regexp_case_fold (any (List.rev_map prefix l)) in
      let matches (n, _) =
	Str.string_match r n 0 && Str.matched_string n = n
      in
      let add ((l, n) as acc) t = if matches t then (t :: l, n+1) else acc in
      List.fold_left add ([], 0) tests

  let sort_tests ?(decr = false) sort tests = match sort with
  | None -> tests
  | Some cmp ->
      let test_cmp =
	if decr then fun (n, _) (n', _) -> -1 * cmp n n' else
	fun (n, _) (n', _) -> cmp n n'
      in
      List.sort test_cmp tests

  let create_run log conf test_count =
    let rseed = match conf.gen_seed with
    | Some s -> s
    | None ->                                     (* generate a random seed. *)
	let s = Random.State.make_self_init () in
	Random.State.bits s
    in
    let kill = match conf.gen_kill with
    | None -> 4 * conf.gen_success | Some v -> v
    in
    let info = { Log.run_test_count = test_count;
		 run_fail_stop = conf.fail_stop;
		 run_verbose = conf.verbose;
		 run_gen_seed = rseed;
		 run_gen_size = conf.gen_size;
		 run_gen_kill = kill;
		 run_gen_success = conf.gen_success;
		 run_gen_falsifiers = conf.gen_falsifiers }
    in
    let rstate = Random.State.make [| rseed |] in
    { info = info; log = log; rstate = rstate; tnum = 0; okay = 0;
      fail = 0; kill = 0; skip = 0; todo = 0; }

  (* Note, it would have been nice to be able to get and possibly
     report the seed for a single test but Random doesn't provide
     that. *)

  let r_newline = Str.regexp "\n"
  let r_checkm = Str.regexp ".*checkm.ml"
  let split_trace ?(full = false) backtrace = (* Split the backtraces (Grrr). *)
    let stack = Str.split r_newline backtrace in
    let no_ptest s = not (Str.string_match r_checkm s 0) in
    if full then stack else List.filter no_ptest stack

  exception Fail_stop (* to stop after first failure. *)

  let run_test r (n, f) =
    let start = Sys.time () in
    r.tnum <- r.tnum + 1;
    let info = { Log.test_name = n; test_num = r.tnum; } in
    r.log r.info (`Test_start info);
    let result =
      try ignore (f r); r.okay <- r.okay + 1; `Okay (Sys.time () -. start) with
      | e ->
	  let b = Printexc.get_backtrace () in
	  match e with
	  | Skip s -> r.skip <- r.skip + 1; `Skip (s, split_trace b)
	  | Todo s -> r.todo <- r.todo + 1; `Todo (s, split_trace b)
	  | Kill s -> r.kill <- r.kill + 1; `Kill (s, split_trace b)
	  | Fail f -> r.fail <- r.fail + 1; `Fail (f, split_trace b)
	  | e ->
	      r.fail <- r.fail + 1; `Fail (`Exn_uncaught (None, e, None),
					      split_trace b)
    in
    r.log r.info (`Test_end (info, result));
    if r.info.Log.run_fail_stop && r.fail > 0 then raise Fail_stop

  let no_exec log conf tests =
    let tests, test_count = select_tests conf.selectors tests in
    let names = List.rev_map fst (sort_tests ~decr:true conf.sort tests) in
    let r = create_run log conf test_count in
    r.log r.info (`No_exec names);
    `Ok

  let run log conf tests =
    if conf.no_exec then no_exec log conf tests else
    let start = Sys.time () in
    let tests, test_count =
      let tests, count = select_tests conf.selectors tests in
      sort_tests conf.sort tests, count
    in
    let r = create_run log conf test_count in
    r.log r.info `Run_start;
    Printexc.record_backtrace true;
    (try List.iter (run_test r) tests; with Fail_stop -> ());
    r.log r.info (`Run_end { Log.run_time = Sys.time () -. start;
			     run_okay = r.okay;
			     run_fail = r.fail;
			     run_kill = r.kill;
			     run_skip = r.skip;
			     run_todo = r.todo; });
    if r.fail = 0 then `Ok else `Fail

  let runf ?log conf f = failwith "TODO"

  (* Command line options *)

  let log_args logger = [
    "-batch", Arg.Unit (fun () -> logger := batch_log) ,
    "batch mode, does not output terminal control sequences" ]

  let run_conf_args a =
    let u a' = a := a' in [
    "-select", Arg.String (fun s -> u { !a with selectors = s::!a.selectors }),
    "<prefix>, execute only tests whose name matches <prefix>";
    "-no-exec", Arg.Unit (fun () -> u { !a with no_exec = true }),
    "don't execute the tests, only print their names";
    "-fail-stop", Arg.Unit (fun () -> u { !a with fail_stop = true }),
    "stop on first failed test";
    "-verbose", Arg.Int (fun v -> u { !a with verbose = v }),
    "<int> make the output more verbose (defaults to 1).";
    "-seed", Arg.Int (fun s -> u { !a with gen_seed = Some s }),
    "<int>, random seed (auto-generated if unspecified).";
    "-size", Arg.Int (fun s -> u { !a with gen_size = s }),
    "<int>, hint size for generated cases (defaults to 100)";
    "-kill", Arg.Int (fun k -> u { !a with gen_kill = Some k }),
    "<int>, generated cases before killing (defaults to 4 * success).";
    "-success", Arg.Int (fun s -> u { !a with gen_success = s }),
    "<int>, succeeding cases before success (defaults to 1000)";
    "-falsifiers", Arg.Int (fun f -> u { !a with gen_falsifiers = f }),
    "number of failing cases reported (defaults to 3)"; ]
end

type test = Test.t

module C = struct

  type check = Test.run -> Test.run

  let fail f = raise (Test.Fail f)
  let arg pr v = fun ppf () -> pr ppf v
  let arg_of_pr pr v = match pr with None -> None | Some pr -> Some (arg pr v)
  let neg p = fun x -> not (p x)
  let success r = r
  let holds ?id ?pr p v r =
    if p v then r else fail (`Holds (id, arg_of_pr pr v))

  let for_all ?id ?pr ?classify ?(cond = fun _ -> true) (g, gpr) check r =
    let total = ref 0 in
    let okay = ref 0 in
    let failc = ref 0 in
    let falsifiers = ref [] in
    while (!okay < r.Test.info.Test.Log.run_gen_success &&
           !total < r.Test.info.Test.Log.run_gen_kill &&
           !failc < r.Test.info.Test.Log.run_gen_falsifiers)
    do
      let v = g r.Test.info.Test.Log.run_gen_size r.Test.rstate in
      if (cond v) then begin
	try check v r; incr okay; with
	| Test.Fail f ->
	    let b = Printexc.get_backtrace () in
	    let f = (arg_of_pr pr v), f, (Test.split_trace b) in
	    falsifiers := f :: !falsifiers;
	    incr failc
      end;
      incr total
    done;
    let labels = None in
    if !failc <> 0 then fail (`For_all (id, !okay, !falsifiers, labels)) else
    if !total = r.Test.info.Test.Log.run_gen_kill then
      raise (Test.Kill (`For_all (id, !okay, labels)))
    else
    r

  let no_raise ?id ?pr f v r =
    try ignore (f v); r with
    | e -> fail (`Exn_uncaught (id, e, arg_of_pr pr v))

  let raises ?id ?pr ?prr ?(exn = fun _ -> true) f v r =
    let res = ref None in
    begin try res := Some (f v)
    with e -> if not (exn e) then fail (`Exn_uncaught (id, e, arg_of_pr pr v))
    end;
    match !res with
    | None -> r
    | Some res -> fail (`Exn_none (id, arg_of_pr pr v, arg_of_pr prr res))

  let raises_failure ?id ?pr ?prr f v t =
    let exn = function Failure _ -> true | _ -> false in
    raises ~exn ?id ?pr ?prr f v t

  let raises_invalid_arg ?id ?pr ?prr f v t =
    let exn = function Invalid_argument _ -> true | _ -> false in
    raises ~exn ?id ?pr ?prr f v t

  let catch c f v r = try c r with Test.Fail _ -> ignore (f v); r

  let log pr v r = r.Test.log r.Test.info (`Print (arg pr v)); r

  module Order = struct
    type 'a cmp = ?cmp:('a -> 'a -> int) -> ?id:string -> ?pr:'a printer -> 'a
      -> 'a -> check

    let cmp_failure ?id ?pr x c y = match pr with
    | None -> `Comparison (id, c, None)
    | Some pr -> `Comparison (id, c, Some (arg pr x, arg pr y))


    let compare ?(cmp = Stdlib.compare) ?id ?pr x c y r =
      let success = match cmp x y with
      | 0 -> (match c with `Eq | `Geq | `Leq -> true | _ -> false)
      | 1 -> (match c with `Gt | `Geq | `Neq-> true | _ -> false)
      | -1 -> (match c with `Lt | `Leq | `Neq -> true | _ -> false)
      | _ -> assert false
      in
      if success then r else fail (cmp_failure ?id ?pr x c y)

    let (=) ?cmp ?id ?pr x y r = compare ?id ?pr ?cmp x `Eq y r
    let (<>) ?cmp ?id ?pr x y r = compare ?id ?pr ?cmp x `Neq y r
    let (<) ?cmp ?id ?pr x y r = compare ?id ?pr ?cmp x `Lt y r
    let (<=) ?cmp ?id ?pr x y r = compare ?id ?pr ?cmp x `Leq y r
    let (>) ?cmp ?id ?pr x y r = compare ?id ?pr ?cmp x `Gt y r
    let (>=) ?cmp ?id ?pr x y r = compare ?id ?pr ?cmp x `Geq y r

    let pequal ?id ?pr x c y r = match (x == y), c with
    | true, `Peq | false, `Pneq -> r
    | false, `Peq | true, `Pneq -> fail (cmp_failure ?id ?pr x c y)
    | _ -> assert false

    let (==) ?id ?pr x y r = pequal ?id ?pr x `Peq y r
    let (!=) ?id ?pr x y r = pequal ?id ?pr x `Pneq y r
  end

  module type Testable = sig
    type t
    val pp : Format.formatter -> t -> unit
    val compare : t -> t -> int
  end

  module type S = sig
    type t

    val holds : ?id:string -> (t -> bool) -> t -> check
    val for_all : ?id:string -> ?classify:(t -> string) -> ?cond:(t -> bool) ->
      t gen -> (t -> Test.run -> 'b) -> check

    val no_raise : ?id:string -> (t -> 'b) -> t -> check

    val raises : ?id:string -> ?prr:'b printer ->
      ?exn:(exn -> bool) -> (t -> 'b) -> t -> check

    val raises_failure : ?id:string -> ?prr:'b printer ->
      (t -> 'b) -> t -> check

    val raises_invalid_arg : ?id:string -> ?prr:'b printer ->
      (t -> 'b) -> t -> check

    val log : t -> check

    module Order : sig
      val (=) : ?id:string -> t -> t -> check
      val (<>) : ?id:string -> t -> t -> check
      val (<) : ?id:string -> t -> t -> check
      val (<=) : ?id:string -> t -> t -> check
      val (>) : ?id:string -> t -> t -> check
      val (>=) : ?id:string -> t -> t -> check
      val (==) : ?id:string -> t -> t -> check
      val (!=) : ?id:string -> t -> t -> check
    end
  end

  module Make (T : Testable) = struct
    type t = T.t
    let holds = holds ~pr:T.pp
    let for_all = for_all ~pr:T.pp
    let no_raise = no_raise ~pr:T.pp
    let raises = raises ~pr:T.pp
    let raises_failure = raises_failure ~pr:T.pp
    let raises_invalid_arg = raises_invalid_arg ~pr:T.pp
    let log = log T.pp
    module Order = struct
      let (=) ?id x y = Order.(=) ~cmp:T.compare ?id ~pr:T.pp x y
      let (<>) ?id x y = Order.(<>) ~cmp:T.compare ?id ~pr:T.pp x y
      let (<) ?id x y = Order.(<) ~cmp:T.compare ?id ~pr:T.pp x y
      let (<=) ?id x y = Order.(<=) ~cmp:T.compare ?id ~pr:T.pp x y
      let (>) ?id x y = Order.(>) ~cmp:T.compare ?id ~pr:T.pp x y
      let (>=) ?id x y = Order.(>=) ~cmp:T.compare ?id ~pr:T.pp x y
      let (==) ?id x y = Order.(==) ?id ~pr:T.pp x y
      let (!=) ?id x y = Order.(!=) ?id ~pr:T.pp x y
    end
  end

  module Special = struct

    module Bool = struct
      type t = bool
      let pp = Format.pp_print_bool
      let compare = Stdlib.compare
    end

    module Char = struct
      type t = char
      let pp ppf c = Format.fprintf ppf "%C" c
      let compare = Stdlib.compare
    end

    module Int = struct
      type t = int
      let pp = Format.pp_print_int
      let compare = Stdlib.compare
    end

    module Float = struct
      type t = float
      let pp = Format.pp_print_float
      let compare = Stdlib.compare
    end

    module String = struct
      type t = string
      let pp ppf s = Format.fprintf ppf "%S" s
      let compare = Stdlib.compare
    end

    module Cb = Make (Bool)
    module Cc = Make (Char)
    module Ci = Make (Int)
    module Cf = Make (Float)
    module Cs = Make (String)
  end
end

module Gen = struct
  let prf ppf fmt = Format.fprintf ppf fmt

  let pr (_, pr) = pr
  let gen (g, _) = g

  (* Base type *)

  let unit = (fun s rs -> ()), fun ppf () -> prf ppf "()"
  let const v pr = (fun s rs -> v), pr
  let bool = (fun s rs -> Random.State.bool rs), fun ppf v -> prf ppf "%B" v
  let uint ?(max = max_int) () =
    let g =
      if max < 0 then invalid_arg "negative max" else
      if max = max_int then fun s rs -> Random.State.bits rs else
      let bound = max + 1 in
      fun s rs -> Random.State.int rs bound
    in
    let pr ppf v = prf ppf "%d" v in
    g, pr


  (* Higher-order *)

  let option (gv, prv) =
    let g s rs = if Random.State.bool rs then None else (Some (gv s rs)) in
    let pr ppf = function
      | None -> prf ppf "None"
      | Some v -> prf ppf "@[<6>Some(%a)@]" prv v
    in
    g, pr

  (* Note, tuple generators do not enforce an order of evaluation. *)

  let t2 (g0, pr0) (g1, pr1) =
    let g s rs = g0 s rs, g1 s rs in
    let pr ppf (v0, v1) = prf ppf "@[<1>(%a,@ %a)@]" pr0 v0 pr1 v1 in
    g, pr

  let t3 (g0, pr0) (g1, pr1) (g2, pr2) =
    let g s rs = g0 s rs, g1 s rs, g2 s rs in
    let pr ppf (v0, v1, v2) =
      prf ppf "@[<1>(%a,@ %a,@ %a)@]" pr0 v0 pr1 v1 pr2 v2 in
    g, pr

  let t4 (g0, pr0) (g1, pr1) (g2, pr2) (g3, pr3) =
    let g s rs = g0 s rs, g1 s rs, g2 s rs, g3 s rs in
    let pr ppf (v0, v1, v2, v3) =
      prf ppf "@[<1>(%a,@ %a,@ %a,@ %a)@]" pr0 v0 pr1 v1 pr2 v2 pr3 v3
    in
    g, pr

  let t5 (g0, pr0) (g1, pr1) (g2, pr2) (g3, pr3) (g4, pr4) =
    let g s rs = g0 s rs, g1 s rs, g2 s rs, g3 s rs, g4 s rs in
    let pr ppf (v0, v1, v2, v3, v4) =
      prf ppf "@[<1>(%a,@ %a,@ %a,@ %a,@ %a)@]"
	pr0 v0 pr1 v1 pr2 v2 pr3 v3 pr4 v4
    in
    g, pr

  let t6 (g0, pr0) (g1, pr1) (g2, pr2) (g3, pr3) (g4, pr4) (g5, pr5) =
    let g s rs = g0 s rs, g1 s rs, g2 s rs, g3 s rs, g4 s rs, g5 s rs in
    let pr ppf (v0, v1, v2, v3, v4, v5) =
      prf ppf "@[<1>(%a,@ %a,@ %a,@ %a,@ %a,@ %a)@]"
	pr0 v0 pr1 v1 pr2 v2 pr3 v3 pr4 v4 pr5 v5
    in
    g, pr



(*
  let const v = fun () -> v
  let bool = fun () -> Random.State.bool rstate


  let int ?(amax = max_int) =
    if amax < 0 then invalid_arg "negative max" else
    if amax = max_int then
      let rec aux () =
	let v = rint rstate () in
	if v = min_int then aux () else v
      in
      aux
    else
      let bound = (2 * amax) + 1 in
      if 0 < bound && bound < max_int then
	fun () -> -amax + Random.State.int rstate bound
      else
	let bound = Int32.add (Int32.mul 2l (Int32.of_int amax)) 1l in
	let min = Int32.of_int (-amax) in
	fun () -> Int32.to_int (Int32.add min (Random.State.int32 rstate bound))

  let after_one = 1. +. epsilon_float
  let ufloat ?(max = max_float) =
    if max < 0. then invalid_arg "negative max" else
    fun () -> (Random.State.float rstate after_one) *. max

  let float ?(amax = max_float) =
    if amax < 0. then invalid_arg "negative max" else
    fun () -> (-1. +. (Random.State.float rstate after_one) *. 2.) *. amax

  let char = fun () -> Char.chr (Random.State.int rstate 256)

  let string ?(len = uint ~max:1000) ?(c = char) =
    fun () ->
      let l = len () in
      let s = String.create l in
      for i = 0 to l - 1 do s.[i] <- c () done;
      s


  let list ?(len = uint ~max:1000) g = fun () ->
    let rec aux i len l = if i = len then l else aux (i + 1) len (g () :: l) in
    aux 0 (len ()) []

  let map f g = fun () -> f (g ())

  let filter p g =
    let rec aux () = let v = g () in if p v then v else aux () in
    aux

  let choose gl = List.nth gl (Random.State.int rstate (List.length gl))
*)

end

(*---------------------------------------------------------------------------
   Copyright (c) 2013 The gg programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
