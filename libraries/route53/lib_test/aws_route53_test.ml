open OUnit
open Aws_route53

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

  let create_domain ~name ~caller_reference =
    Runtime.(un_m (run_request (module CreateHostedZone)
                     (Types.CreateHostedZoneRequest.make ~name ~caller_reference ())))

  let arb_domain_name =
    QCheck.Gen.oneofl Aws_test.Corpus.viruses

  let create_delete_domain_test =
    QCheck.Test.make ~count:1
      ~name:"Route53 create / delete domain"
      QCheck.(QCheck.make arb_domain_name
                ~print:(fun x -> x))
      (fun domain ->
        let  create_res = create_domain ~name:(domain ^ ".com") ~caller_reference:"ocaml-aws-test" in
        match create_res with
        | `Ok resp ->
           Printf.printf "%s\n" (Yojson.Basic.to_string (Types.CreateHostedZoneResponse.(to_json (of_json (to_json resp)))));
           true == true
        | `Error err ->
           Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err);
           assert false
      )

  let test_cases =
    [ create_delete_domain_test
    ]

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
