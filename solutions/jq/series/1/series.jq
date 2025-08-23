if .sliceLength == 0 then "slice length cannot be zero" | halt_error 
elif .sliceLength < 0 then "slice length cannot be negative" | halt_error 
elif (.series | length) == 0 then "series cannot be empty" | halt_error 
elif (.series | length) < .sliceLength then "slice length cannot be greater than series length" | halt_error
else [. as {sliceLength:$l, series:$s} | $s | match("(?=\\d{\($l)}+)";"g") | $s[.offset:.offset+$l] ]
end
