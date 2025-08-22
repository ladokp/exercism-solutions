#!/usr/bin/env gawk -f

BEGIN {
    RS = "^$"
}
END {
    gsub(/[[:space:]]/, "")
    if ($0 == "")
        print "Fine. Be that way!"
    else {
        yelling = /[A-Z]/ && !/[a-z]/
        asking  = /\?$/
        switch (asking "" yelling ) {
            case "11": print "Calm down, I know what I'm doing!"; break
            case "01": print "Whoa, chill out!"; break
            case "10": print "Sure."; break
            case "00": print "Whatever."; break
        }
    }
}