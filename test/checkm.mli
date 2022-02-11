(*---------------------------------------------------------------------------
   Copyright (c) 2013 The gg programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Unit testing with automatic case generators (or not).

    [Checkm] is a module to define tests and run them. It supports
    precise failure reports and automatic (e.g. randomized) test case
    generation.

    Consult the {{:#basics}basics}.

    Open the module to use it.

    {b Important.} The module and the sources of your tests must be
    compiled and linked with [-g] (tag sources and executables with
    [debug] for [ocamlbuild]) to get good backtraces. Bytecode
    backtraces are usually more precise than native ones.

    {e Version %%VERSION%% - %%EMAIL%% }

    {1:top  } *)

type test
(** The type for tests. *)

type 'a printer = Format.formatter -> 'a -> unit
(** The type for value printers. *)

type 'a gen = (int -> Random.State.t -> 'a) * 'a printer
(** The type for value generators. *)

val (>>) : 'a -> ('a -> 'b) -> 'b
(** Sequencing operator. *)

(** Unit tests.

    {1:top  Tests} *)
module Test : sig

  type run
  (** The type for tests runs. Holds
      information about a test run. *)

  type t = test
  (** The type for tests. *)

  val create : string -> (run -> 'b) -> test
  (** [create n f] creates a test with name [n] and testing function [f]. *)

  val name : test -> string
  (** [name t] is the name of [t]. *)

  val func : test -> (run -> unit)
  (** [func t] is the test function of [t]. *)

  (** {3:defaultlist Global test list}

      Many test programs just define a list of tests in a global variable.
      [Checkm] defines one for you. *)

  val list : test list ref
  (** [list] an initially empty global list of tests. *)

  val add_test : string -> (run -> 'b) -> unit
  (** [add_test n f] add the test [create n f] in front of {!list}. *)

  (** {1:fail Stopping tests}

      These functions can be called by test functions to stop a test
      in different ways. *)

  val fail : string -> 'a
  (** [fail reason] stops the current test and marks it as failed
      for the given [reason]. *)

  val kill : string -> 'a
  (** [kill reason] stops the current test and marks it as killed
      for the given [reason]. *)

  val skip : string -> 'a
  (** [skip reason] stops the current test and marks it as skipped
      for the given [reason]. *)

  val todo : string -> 'a
  (** [todo reason] stops the current test and marks it as to be done
      for the given [reason]. *)

  (** {1:run Runs} *)

  (** Types for implementing logs. *)
  module Log : sig

    type backtrace = string list
    type arg = unit printer
    (** The type for function arguments in failures. Invoking
        the printer with unit will print the argument on the
        formatter. *)

    type id = string option
    type cmp = [ `Gt | `Geq | `Lt | `Leq | `Eq | `Neq | `Peq | `Pneq]
    (** The type for comparisons. *)
    type labels = (string * int) list
    type murder = [
      | `For_all of id * int * labels option
      | `Other of string ]


    type failure = [
      | `Exn_uncaught of id * exn * arg option
      | `Exn_none of id * arg option * arg option
      | `Comparison of id * cmp * (arg * arg) option
      | `Holds of id * arg option
      | `For_all of id * int * (arg option * failure * backtrace) list *
	    labels option
      | `Other of string ]

    type run_info =
	{ run_test_count : int; (** Total number of tests in the run. *)
	  run_fail_stop : bool;
	  run_verbose : int; (** Hinted output verboseness. *)
	  run_gen_seed : int; (** Random seed for the run. *)
	  run_gen_size : int; (** Size hint for generated values. *)
	  run_gen_kill : int; (** Maximal number of cases generated. *)
	  run_gen_success : int; (** Number of cases to pass *)
	  run_gen_falsifiers : int; (** Approximate number of falsifiers. *) }

    type run_result =
	{ run_time : float; (** Time taken to run all the tests. *)
	  run_okay : int; (** Number of successful tests. *)
	  run_fail : int; (** Number of failed tests. *)
	  run_kill : int; (** Number of killed tests. *)
	  run_skip : int; (** Number of skipped tests. *)
	  run_todo : int (** Number of tests to do. *) }

    type test_info =
	{ test_name : string;  (** Test name. *)
	  test_num : int;   (** Test number in the run. *)
	  (* test_seed : int;  (** Random seed for the test. *) *) }

    type test_result = [
      | `Okay of float (** Time taken *)
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

    val pp_paragraphs : Format.formatter -> string -> unit
    (** [pp_paragraphs fmt s] pretty prints the paragraphs in [s] on
	[fmt].  Paragraphs are words separated by two or more
	newlines ('\n'). Words are sequences of printable characters
	separated by a sequence of white space that do not contain
	two consecutive newlines ('\n'). *)
  end
  type log = Log.run_info -> Log.event -> unit
  (** The type for logs. *)

  val term_log : Format.formatter -> log
  (** [term_log fmt] logs events on [fmt], uses
      {{:http://www.ecma-international.org/publications/standards/Ecma-048.htm}
	ANSI escape sequences} for interactive feedback. *)

  val batch_log : Format.formatter -> log
    (** Same as {!term_log} but doesn't use ANSI escape sequences. *)

  type run_conf =
      { selectors : string list; (** Test name prefixes (see below). *)
	sort : (string -> string -> int) option; (** Test sorter. *)
	no_exec : bool; (** Don't execute tests only log their names. *)
	fail_stop : bool; (** Stop the run on the first test failure. *)
	verbose : int; (** Test monitoring verboseness. *)
        gen_seed : int option; (** Random seed for random case generation. *)
	gen_size : int; (** Hinted generated case size. *)
	gen_kill : int option;
          (** Number of generated cases before killing 4 * success if
	      unspecified. *)
	gen_success : int; (** Number of successful cases before succeeding. *)
	gen_falsifiers : int; (** Number of failing cases reported. *) }
  (** The type for run configuration.

      [selectors] is a list of prefixes, only tests whose names
      match a prefix in [selectors] are performed. If [selector]
      is the empty list all tests are selected.

      Properties starting with [gen] pertain to automatic test
      case generation. TODO link to how it works and meaning of args.*)

  val run_conf : run_conf
  (** [run_conf] is the default run configuration. *)

  val run : log -> run_conf -> test list -> [ `Ok | `Fail ]
  (** [run logger conf tl] runs the test list [tl] with configuration
      [conf] and feedbacks the result with [log]. [`Ok] is returned
      iff all tests succeed. *)

  val runf : ?log:log -> run_conf -> (run -> 'a) -> [ `Ok | `Fail ]
  (** [runf conf f] runs the test function [f] with the configuration [conf]
      and feedbacks the result on stdout. *)

  (** {3:commandline Command line options} *)

  val log_args : (Format.formatter -> log) ref ->
      (Arg.key * Arg.spec * Arg.doc) list
  (** [log_args lr] returns a list of command line argument specifications
      to use with the [Arg] module to define the log in [lr]. *)

  val run_conf_args : run_conf ref -> (Arg.key * Arg.spec * Arg.doc) list
  (** [run_conf_args cr] returns a list of command line argument specifications
      to use with the [Arg] module to define the run configuration in [cr]. *)

end


(** Checks (assertions)

    Checks perform assertions with precise error reports.

    Most combinators have two optional arguments.
    [id] an optional string that may help to identify a check.
    [pr] an optional value printer. Used to print the arguments
    given to checks, use {!Make} to apply the argument for
    a given type.
*)
module C : sig

   type check = Test.run -> Test.run
  (** The type for checks. A check

    A check is just a function that threads the
    tester. check combinators generate checks. Just a way to
    pass implicit arguments by. *)

  val neg : ('a -> bool ) -> ('a -> bool)
  (** [neg p x] is [true] iff [p x] is [false]. *)

  val success : check
  (** [success] is a check that always succeeds. *)

  val holds : ?id:string -> ?pr:'a printer -> ('a -> bool) -> 'a -> check
  (** [holds p v] succeeds [iff] [p v] is [true]. *)

  val for_all : ?id:string -> ?pr:'a printer -> ?classify:('a -> string) ->
    ?cond:('a -> bool) -> 'a gen -> ('a -> Test.run -> 'b) -> check
  (** [for_all classify cond g c] will repeateadly generate values [v]
      from [g], if [cond v] is [true] will check [c v] (number of
      tested values depends on tester and [cond] defaults to [fun _ ->
      true]) If given [classify] sorts value into equivalence classes
      to compute a distribution of the generated values. *)

  val no_raise : ?id:string -> ?pr:'a printer -> ('a -> 'b) -> 'a -> check
  (** [no_raise f v] succeeds iff [f v] raises no exception. *)

  val raises : ?id:string -> ?pr:'a printer -> ?prr:'b printer ->
    ?exn:(exn -> bool) ->
  ('a -> 'b) -> 'a -> check
  (** [raises ~exn f v] succeeds iff [f v] raises an exception [e] such
      that [exn e] is [true] ([exn] defaults to [fun _ -> true]). *)

  val raises_failure : ?id:string -> ?pr:'a printer -> ?prr:'b printer ->
    ('a -> 'b) -> 'a -> check
  (** [raises_failure f v] succeeds iff [f v] raises [Failure]. *)

  val raises_invalid_arg : ?id:string -> ?pr:'a printer -> ?prr:'b printer ->
    ('a -> 'b) -> 'a -> check
  (** [raises_invalid_arg f v] succeeds iff [f v] raises
      [Invalid_argument]. *)

  val catch : check -> ('a -> 'b) -> 'a -> check
  (** [catch c f v] executes [c] if it fails the failure
      is catched and [f v] is executed. The [catch] check
      always succeeds. *)

  val log : 'a printer -> 'a -> check
  (** Not a check, [log pr v] logs the value [v] in the log using
      printer [pr]. *)

  (** Comparison predicates.

      These are the most important checks. This module
      must be opened in your scope.

      Being polymorphic they cannot print their arguments. Use
      them infinx with an appropriate [pr] parameter or better
      use the functor to specialize the predicates on a type.

      All these combinators except the physical ones use
      [Stdlib.compare]. Use the functor if you need another
      comparison function. *)
  module Order : sig

    type 'a cmp =
	?cmp:('a -> 'a -> int) -> ?id:string -> ?pr:'a printer -> 'a -> 'a ->
	  check
    (** The type for comparisons. Just a shortcut. *)

    val (=) : 'a cmp
    val (<>) :'a cmp
    val (<) : 'a cmp
    val (<=) : 'a cmp
    val (>) : 'a cmp
    val (>=) : 'a cmp
    val (==) : ?id:string -> ?pr:'a printer -> 'a -> 'a -> check
    val (!=) : ?id:string -> ?pr:'a printer -> 'a -> 'a -> check
  end


  (** {1 Combinator specialization}

      Given a comparison function [compare] a printer [print] and a
      type [t] the functor {Checkm.C.Make} automatically applies the
      [pr] parameter and generates suitable {!Order} predicates.

      Basic types are already specialized in the module C.Special
      open this module to use them.

      TODO Testable.t should maybe be type 'a Testable.t *)


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

  module Make (T : Testable) : S with type t = T.t

  module Special : sig
    module Cb : S with type t = bool
    module Cc : S with type t = char
    module Ci : S with type t = int
    module Cf : S with type t = float
    module Cs : S with type t = string
  end
end

(** Test case generators.

    {b Note.} Not all generators are random.
*)
module Gen : sig

  val pr : 'a gen -> 'a printer
  val gen : 'a gen -> (int -> Random.State.t -> 'a)

  (** {1:base Base type random generators} *)

  val unit : unit gen
  (** [unit] generates always [()]. *)

  val const : 'a -> 'a printer -> 'a gen
  (** [const v pr] generates always [v] and prints it with [pr]. *)

  val bool : bool gen
  (** [bool] generates [bool] values. *)

  val uint : ?max:int -> unit -> int gen
  (** [uint max ()] is a generator for [int] values in \[[0;max]\]
      ([max] defaults to [max_int]). *)

  (** {1:higher Higher order generators} *)

  val option : 'a gen -> 'a option gen
  (** [option g] randomly generates option cases. The [Some] case values
      are generated by [g]. *)


  val t2 : 'a gen -> 'b gen -> ('a * 'b) gen
  (** [t2 g0 g1] generates a couple with [g0] and [g1]. *)

  val t3 : 'a gen -> 'b gen -> 'c gen -> ('a * 'b * 'c) gen
  (** [t3 g0 g1 g2] generates a triplet with [g0], [g1] and [g2]. *)

  val t4 : 'a gen -> 'b gen -> 'c gen -> 'd gen -> ('a * 'b * 'c * 'd) gen
  (** [t4 g0 g1 g2 g3] generates a quadruplet with [g0], [g1], [g2] and [g3]. *)

  val t5 : 'a gen -> 'b gen -> 'c gen -> 'd gen -> 'e gen ->
    ('a * 'b * 'c * 'd * 'e) gen
  (** [t5 g0 g1 g2 g3 g4] generates a quintuplet with [g0], [g1], [g2], [g3]
      and [g4]. *)

  val t6 : 'a gen -> 'b gen -> 'c gen -> 'd gen -> 'e gen -> 'f gen ->
    ('a * 'b * 'c * 'd * 'e * 'f) gen
  (** [t6 g0 g1 g2 g3 g4 g5] generates a sextuplet with [g0], [g1], [g2], [g3],
      [g4] and [g5] *)

(*


  val int : ?amax:int -> int gen
  (** [int amax] is a generator for [int] values in \[[-amax;amax]\]
      ([amax] defaults to [max_int]). Note that [min_int] is never generated. *)

  val ufloat : ?max:float -> float gen
  (** [ufloat max] is a generator for [float] values in \[[0;max]\]
      ([max] defaults to [max_float]). *)

  val float : ?amax:float -> float gen
  (** [float amax] is a generator for [float] values in \[[-amax;amax]\]
      ([amax] defaults to [max_float]) *)

  val char : char gen
  (** [char] is a generator for latin1 [char]s. *)

  (** {1:higher Higher order generators} *)

  val string : ?len:int gen -> ?c:char gen -> string gen
  (** [string len] is a generator for latin1 [string]s whose
      length is generated by [len] (defaults to [uint ~max:1000])
      and characters by [c] (defaults to [char]). *)

  val option : 'a gen -> 'a option gen
  (** [option g] is a generator for option values using [g] if there's some. *)

  val pair : 'a gen -> 'b gen -> ('a * 'b) gen
  (** [pair g g'] is generetor for pairs using [g] and [g']. *)

  val list : ?len:int gen -> 'a gen -> 'a list gen
  (** [list len g] is a generator for [list] values whose
      length is generated by [len] (defaults to [uint ~max:1000])
      and elements by [g].*)

  val map : ('a -> 'b) -> 'a gen -> 'b gen
  (** [map f g] is the generator for values in ['b] by
      mapping the result of [g] with [f]. *)

  val filter : ('a -> bool) -> 'a gen -> 'a gen
  (** [filter p g] is the generator with values [v] from
      [g] such that [p v] is true. *)

  val choose : 'a gen list -> 'a gen
  (** [choose gl] is the generator using the generators in [gl]
      equiprobably. *)

*)

end

(** {1:basics Basics}

    Three steps.

    1. Introduce the test module.

    A test is just a function taking a tester (ignore it for now).
    If the function returns a value the tests succeeds, if it
    raises any exception it fails.

    * defining test
    * running & selecting test.
    * global list of tests.

    2. Introduce checks.
    3. Introduce checks with case generation.


    TODO polish and distribute separately.
    TODO remove dependency on Str.
    TODO add a module for generators.

    Rationale.
    * avoid names as much as possible.
    * on failure, backtrace (for emacs).
    * on failure, print as much as possible, arguments, result
      expected result. Toplevel feel.


    Test unit {Test.unit} is a named function taking a tester.
    Unit of failure.


    TODO. A {e check} is a property you want to verify. Conjuctions of
    checks are combined into named {e units}. A unit is a check itself
    so it can be combined with other checks. A check either succeeds
    or fails. If it succeeds the next check in the conjuction is
    executed. If it fails it aborts immediatly the rest of the checks
    in the unit. A unit itself always succeeds regardless of what
    happened to its checks this allows to run sequences of
    units. There are however different ways to abort a unit

    {e outcome} of a unit depends on its checks.
    TIPS heavy weight predicate vs fine grained seqs with compare.

*)

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
