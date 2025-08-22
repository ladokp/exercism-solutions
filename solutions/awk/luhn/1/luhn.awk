{
    gsub(" ", "", $0)
    if ( $0 ~ /[^0-9 ]+/ || length($0) < 2 ) {
        print "false"
        exit
    }
    
    sum = 0
    for (i = length($0) - 1; i > 0; i-=2) {
        new_digit = substr($0, i, 1) * 2
        if (new_digit > 9) {
            new_digit -= 9
        }
        sum += new_digit
    }
    
    for (i = length($0); i > 0; i-=2) {
        sum += substr($0, i, 1)
    }
    
    if (sum % 10 == 0) {
        print "true"
    } else {
        print "false"
    }
}