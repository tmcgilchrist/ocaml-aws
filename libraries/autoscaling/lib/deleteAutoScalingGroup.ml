open Types
open Aws

type input = DeleteAutoScalingGroupType.t

type output = unit

type error = Errors_internal.t

let service = "autoscaling"

let to_http service region (req : input) =
  let uri =
    Uri.add_query_params
      (Uri.of_string @@ Aws.Util.of_option_exn (Endpoints.url_of service region))
      (List.append
         [ "Version", [ "2011-01-01" ]; "Action", [ "DeleteAutoScalingGroup" ] ]
         (Util.drop_empty
            (Uri.query_of_encoded
               (Query.render (DeleteAutoScalingGroupType.to_query req)))))
  in
  `POST, uri, []

let of_http body = `Ok ()

let parse_error code err =
  let errors =
    [ Errors_internal.ResourceContention
    ; Errors_internal.ResourceInUse
    ; Errors_internal.ScalingActivityInProgress
    ]
    @ Errors_internal.common
  in
  match Errors_internal.of_string err with
  | Some var ->
      if List.mem var errors
         &&
         match Errors_internal.to_http_code var with
         | Some var -> var = code
         | None -> true
      then Some var
      else None
  | None -> None
