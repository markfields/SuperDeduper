namespace SuperDeduper

  module ResultOperators = 
    let (>>=) a b = Result.bind b a

  [<AutoOpenAttribute>]
  module ResultExtensions =
    open ResultOperators

    type ResultBuilder() =

      member this.Bind(option, mapping) = option >>= mapping

      member this.Return(thing) = Result.Ok thing

      member this.ReturnFrom(thing) = thing

    let result = ResultBuilder()
