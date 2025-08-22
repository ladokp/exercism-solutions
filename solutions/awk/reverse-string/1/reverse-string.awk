BEGIN { FS = "" }
{ for (c = NF; c > 0; c--) reversed = reversed $c }
END { print reversed }