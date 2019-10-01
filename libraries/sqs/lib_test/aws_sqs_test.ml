open OUnit
open Aws_sqs

(* TODO Maybe load this from ENV variable *)
let test_region = "us-east-1"

let from_opt = function
  | None -> assert false
  | Some(x) -> x

module TestSuite(Runtime : sig
    type 'a m
    val run_request :
      region:string
      -> (module Aws.Call with type input = 'input
                           and type output = 'output
                           and type error = 'error)
      -> 'input
      -> [`Ok of 'output | `Error of 'error Aws.Error.t] m
    val un_m : 'a m -> 'a
  end) = struct

  let delete_queue queue_url region =
    Runtime.(un_m (run_request
                     ~region
                     (module DeleteQueue)
                     (Types.DeleteQueueRequest.make ~queue_url ())))

  let create_queue queue_name region =
    Runtime.(un_m (run_request
                     ~region
                     (module CreateQueue)
                     (Types.CreateQueueRequest.make ~queue_name ())))

  let arb_cooking =
    let c = List.map (QCheck.Gen.pure) Aws_test.Corpus.cooking in
    QCheck.make @@ QCheck.Gen.oneof c

  let create_delete_queue_test =
    QCheck.Test.make ~count:1
    ~name:"SQS create / delete queue"
    QCheck.(arb_cooking)
    (fun queue_name ->
      let create_res = create_queue queue_name test_region in

      begin match create_res with
         | `Ok resp ->
            Printf.printf "%s\n" (Yojson.Basic.to_string (Types.CreateQueueResult.(to_json (of_json (to_json resp)))));
          true
       | `Error err -> begin Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err); false end
      end

      (* let queue_url = match create_res with
       *   | `Ok resp -> from_opt resp.queue_url
       *   | `Error err -> assert false
       * in
       *
       * let delete_res = delete_queue queue_url "us-east-1" in
       * "Delete Queue"
       * @? begin match delete_res with
       *    | `Ok resp -> true
       *    | `Error err -> begin Printf.printf "Error: %s\n" (Aws.Error.format Errors_internal.to_string err); false end
       *    end *)
    )
  let test_cases =
    [ create_delete_queue_test ]

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
