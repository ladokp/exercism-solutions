module RunLengthEncoding exposing (encode, decode)

import String exposing (fromInt)
import Char exposing (isDigit, toCode)

{-| Encode a string using Run-Length Encoding.
   Consecutive characters are compressed into the character and its count.
-}
encode : String -> String
encode input =
    let
        -- Helper function to accumulate characters and counts as we go
        encodeHelper chars currentChar count acc =
            case chars of
                [] ->
                    -- If no more characters, finalize by adding the last count + character
                    if count > 1 then
                        acc ++ fromInt count ++ String.fromChar currentChar
                    else
                        acc ++ String.fromChar currentChar

                c :: rest ->
                    if c == currentChar then
                        -- If the character matches the current one, increase the count
                        encodeHelper rest currentChar (count + 1) acc
                    else
                        -- If a different character, add current count+char to acc, reset count
                        let
                            newAcc =
                                if count > 1 then
                                    acc ++ fromInt count ++ String.fromChar currentChar
                                else
                                    acc ++ String.fromChar currentChar
                        in
                        encodeHelper rest c 1 newAcc
    in
    case String.toList input of
        [] ->
            ""

        c :: rest ->
            encodeHelper rest c 1 ""


{-| Decode a Run-Length Encoded string back to its original form.
   Expects counts to be followed by characters.
-}
decode : String -> String
decode input =
    let
        -- Helper function to accumulate decoded output
        decodeHelper chars count acc =
            case chars of
                [] ->
                    acc

                c :: rest ->
                    if isDigit c then
                        -- Convert character digit to integer by using ASCII code
                        decodeHelper rest (count * 10 + (toCode c - toCode '0')) acc
                    else
                        -- Non-digit, repeat character by count and reset count
                        let
                            newAcc =
                                acc ++ String.repeat (if count == 0 then 1 else count) (String.fromChar c)
                        in
                        decodeHelper rest 0 newAcc
    in
    decodeHelper (String.toList input) 0 ""
