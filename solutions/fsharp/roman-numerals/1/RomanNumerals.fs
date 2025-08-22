module RomanNumerals
let dict = Map.ofList [(1, "I");(4, "IV");(5, "V");(9, "IX");(10, "X");(40, "XL");(50, "L");(90, "XC");(100, "C");(400, "CD");(500, "D");(900, "CM");(1000, "M")]

let numbers = dict |> Map.toList |> List.map (fun number -> fst number) |> fun list -> 0::list |> List.rev |> List.toArray

let fromArab arabicNumber =
    let rec translate (arabicNumber: int) (result: string) (index: int) =
        match numbers.[index] = 0, arabicNumber >= numbers.[index] with
        | true, _ -> result
        | false, false -> translate arabicNumber result (index + 1)
        | false, true  -> translate (arabicNumber - numbers.[index]) ($"{result}{dict.Item numbers.[index]}") index
    translate arabicNumber "" 0

let roman arabicNumeral = fromArab arabicNumeral
