namespace SuperDeduper

  module ResultOperators = 
    let (>>=) a b = Result.bind b a

  [<AutoOpenAttribute>]
  module ResultExtensions =
    open ResultOperators

    let allOkOrElse (aggregateError: 'b): Result<'a, 'b> list -> Result<'a list, 'b> =
      let folder (list: Result<'a list, 'b>) (next: Result<'a, 'b>) = 
        match list, next with
        | Ok l, Ok n -> n::l |> Result.Ok
        | _, _ -> Result.Error aggregateError
      List.fold folder (Result.Ok [])

    type ResultBuilder() =

      member this.Bind(option, mapping) = option >>= mapping

      member this.Return(thing) = Result.Ok thing

      member this.ReturnFrom(thing) = thing

    let result = ResultBuilder()
