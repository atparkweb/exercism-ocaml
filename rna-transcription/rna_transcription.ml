type dna = [ `A | `C | `G | `T ]
type rna = [ `A | `C | `G | `U ]

let to_rna x =
  let translate = function
    `A -> `U
    | `C -> `G
    | `G -> `C
    | `T -> `A
  in List.map translate x;;
