BEGIN {
    dictionary["M"] = 1000; dictionary["CM"] = 900; dictionary["D"] = 500; dictionary["CD"] = 400
    dictionary["C"] = 100 ; dictionary["XC"] = 90 ; dictionary["L"] = 50 ; dictionary["XL"] = 40
    dictionary["X"] = 10  ; dictionary["IX"] = 9  ; dictionary["V"] = 5  ; dictionary["IV"] = 4
    dictionary["I"] = 1
    PROCINFO["sorted_in"] = "@val_num_desc"
}

{
    roman = ""
    decimal = $1
    for (numeral in dictionary) {
        while (decimal >= dictionary[numeral]) {
            decimal -= dictionary[numeral]
            roman = roman numeral
        }
    }
    print roman
}