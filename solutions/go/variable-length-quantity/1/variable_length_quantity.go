package variablelengthquantity

import "errors"

const limit = 0x80

func EncodeVarint(input []uint32) (encoded []byte) {
	for _, value := range input {
		e := []byte{byte(value % limit)}
		for value >>= 7; value != 0; value >>= 7 {
			e = append([]byte{limit + byte(value%limit)}, e...)
		}
		encoded = append(encoded, e...)
	}
	return
}

func DecodeVarint(input []byte) (decodedIntegers []uint32, err error) {
	var d uint32
	var complete bool
	for _, value := range input {
		d += uint32(value & (limit - 1))
		complete = value&limit == 0
		if complete {
			decodedIntegers = append(decodedIntegers, d)
			d = 0
			continue
		}
		d <<= 7
	}

	if !complete {
		decodedIntegers, err = nil, errors.New("incomplete sequence")
	}
	return
}
