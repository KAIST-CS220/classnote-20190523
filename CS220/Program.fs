open System

/// Option<'a>
type MaybeBuilder () =
  member __.Bind (m, f) =
    match m with
    | None -> None
    | Some v -> f v

  member __.Return (x) = Some x

let maybe = MaybeBuilder ()

let v: Option<int * int> =
  maybe {
    let! x = Some 1
    let! y = Some 2
    return x, y
  }
  // automatically convert the above to...
let v' =
  maybe.Bind (Some 1, fun x ->
    maybe.Bind (Some 2, fun y ->
      maybe.Return (x, y)))

    ////
let v'' =
  1 |> (fun x -> 2 |> fun y -> Some (x, y))


type Parser<'a> = {
  Parse: string -> Result<'a * string, string>
}

let runOnInput p input =
  p.Parse input

type ParserBuilder () =
  member __.Bind (p, f) =
    { Parse = fun s ->
        match runOnInput p s with
        | Ok (v, remainder) ->
          runOnInput (f v) remainder
        | Error msg -> Error msg }

  member __.Return (x) =
    { Parse = fun s -> Ok (x, s) }

let parser = ParserBuilder ()

module Parser =
  let char ch =
    { Parse = fun s ->
        if String.IsNullOrEmpty s then
          Error "No input"
        else
          if s.[0] = ch then Ok (s.[0], s.[1..])
          else Error "Invalid char" }

  // let twoChars =
  //   parser {
  //     let! a = char
  //     let! b = char
  //     return a, b
  //   }

  let andThen p1 p2 =
    parser {
      let! a = p1
      let! b = p2
      return (a, b)
    }

  let (.>>.) = andThen

  // Parser<'a> list -> Parser<'a list>
  let rec sequence parsers =
    match parsers with
    | [] -> parser { return [] }
    | hd :: tl ->
    parser {
      let! h = hd
      let! t = sequence tl
      return (h :: t)
    }

  // let strABC = char 'A' .>>. char 'B' .>>. char 'C'
  let strABC =
    [ char 'A'; char 'B'; char 'C' ] |> sequence


  // map: ('a -> 'b) -> Parser<'a> -> Parser<'b>
  let map f parser =
    { Parse = fun s ->
        match runOnInput parser s with
        | Ok (v, rest) -> Ok (f v, rest)
        | Error e -> Error e }

  let (|>>) p f = map f p

  // Parser<string>
  let strABC =
    [ char 'A'; char 'B'; char 'C' ]
    |> sequence
    // Parser<char list>
    |>> (fun chars -> List.toArray chars
                      |> System.String)

  let orElse p1 p2 =
    { Parse = fun s ->
        match runOnInput p1 s with
        | Ok (v, rest) -> Ok (v, rest)
        | Error _ -> runOnInput p2 s }
  let (<|>) = orElse

  // Given string "123abc"

//   let twoChars = char .>>. char

  // decompose
  // let twoChars' =
  //   parser.Bind (char, fun a ->
  //     parser.Bind (char, fun b ->
  //       parser.Return (a, b)))
    ///
  //   fun s ->
  //     let v, remainder = runOnInput char s
  //     let a = v // assign name
  //     let b = v' // assign parsed value from the
  //                // second parser with the remainder
  //     { Parse = fun s -> Ok ((a, b), s) }



[<EntryPoint>]
let main argv =
  // runOnInput Parser.twoChars "xa"
  // |> printfn "%A"
  0 // return an integer exit code
