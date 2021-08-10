open Types

type input = DeleteUserMessage.t

type output = User.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
