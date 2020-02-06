open OUnit
open Aws_sdb

let from_opt = function
  | None -> assert false
  | Some(x) -> x

module TestSuite(Runtime : sig
    type 'a m
    val run_request :
        (module Aws.Call with type input = 'input
                           and type output = 'output
                           and type error = 'error)
      -> 'input
      -> [`Ok of 'output | `Error of 'error Aws.Error.t] m
    val un_m : 'a m -> 'a
  end) = struct

  let arb_domain =
    QCheck.Gen.oneofl Aws_test.Corpus.boats

  let create_delete_domain =
    QCheck.Test.make ~count:1
      ~name:"SDB create / delete domain"
        QCheck.(QCheck.make arb_domain)
        (fun domain_name ->

          let create_domain = Runtime.(un_m (run_request
                                               (module CreateDomain)
                                               (Types.CreateDomainRequest.make ~domain_name ()))) in
          match create_domain with
          | `Ok resp ->
             true
          | `Error error ->
             Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string error);
             false
          (*
            1. Build request to create domain
            2. Make request
            3. Check response is OK
           *)
        )
  let test_cases =
    [ create_delete_domain ]

  let rec was_successful =
    function
    | [] -> true
    | RSuccess _::t
    | RSkip _::t ->
      was_successful t
    | RFailure _::_
    | RError _::_
    | RTodo _::_ ->
      false
  let _ =
    let verbose = ref false in
    let set_verbose _ = verbose := true in
    Arg.parse
      [("-verbose", Arg.Unit set_verbose, "Run the test in verbose mode.");]
      (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
      ("Usage: " ^ Sys.argv.(0) ^ " [-verbose]");
    if not (was_successful (QCheck_runner.run_tests_main test_cases)) then
      exit 1
end
