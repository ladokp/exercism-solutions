# These variables are initialized on the command line (using '-v'):
# - len

@include "assert"
{
    d = length
    assert(d > 0, "series cannot be empty")
    assert(len > 0 && len <= d, "invalid length")
    p = d - len + 1
    for ( i = 1; i <= p; i++ ) {
        printf "%s%s", substr($0, i, len), (i < p ? " " : "\n" )
    }
}
