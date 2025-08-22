module TwoFer

let twoFer (input: string option): string =
    if input = None then
        "One for you, one for me."
    else
        $"One for {input}, one for me."