open Types
open Aws

type input = DeleteDBInstanceMessage.t

type output = DeleteDBInstanceResult.t

type error = Errors_internal.t

let service = "rds"

let to_http service region (req : input) =
  let uri =
    Uri.add_query_params
      (Uri.of_string @@ Aws.Util.of_option_exn (Endpoints.url_of service region))
      (List.append
         [ "Version", [ "2014-10-31" ]; "Action", [ "DeleteDBInstance" ] ]
         (Util.drop_empty
            (Uri.query_of_encoded (Query.render (DeleteDBInstanceMessage.to_query req)))))
  in
  `POST, uri, []

let of_http body =
  try
    let xml = Ezxmlm.from_string body in
    let resp =
      Util.option_bind
        (Xml.member "DeleteDBInstanceResponse" (snd xml))
        (Xml.member "DeleteDBInstanceResult")
    in
    try
      Util.or_error
        (Util.option_bind resp DeleteDBInstanceResult.parse)
        (let open Error in
        BadResponse
          { body; message = "Could not find well formed DeleteDBInstanceResult." })
    with Xml.RequiredFieldMissing msg ->
      let open Error in
      `Error
        (BadResponse
           { body
           ; message =
               "Error parsing DeleteDBInstanceResult - missing field in body or \
                children: "
               ^ msg
           })
  with Failure msg ->
    `Error
      (let open Error in
      BadResponse { body; message = "Error parsing xml: " ^ msg })

let parse_error code err =
  let errors =
    [ Errors_internal.SnapshotQuotaExceeded
    ; Errors_internal.DBSnapshotAlreadyExists
    ; Errors_internal.InvalidDBInstanceState
    ; Errors_internal.DBInstanceNotFound
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
