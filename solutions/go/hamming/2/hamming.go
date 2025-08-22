package hamming

import "errors"

func Distance(a, b string) (distance int, err error) {
    if len(a) != len(b) {
        err = errors.New("length is different")
        return
    }

    for i := 0; i < len(a); i += 1 {
        if a[i] != b[i] {
            distance += 1
        }
    }
    return
}
