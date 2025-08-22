package hamming

import "errors"

func Distance(a, b string) (distance int, err error) {
    if len(a) != len(b) {
        err = errors.New("length is different")
        return
    }
    for index, a_value := range []byte(a) {
        if a_value != b[index] {
            distance += 1
        }
    }
    return
}
