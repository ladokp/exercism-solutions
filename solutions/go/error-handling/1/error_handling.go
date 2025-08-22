package erratum

import (
	"errors"
)

var transientError TransientError
var frobError FrobError

func Use(opener ResourceOpener, input string) (err error) {
	var resource Resource
	for resource, err = opener(); err != nil; resource, err = opener() {
		if errors.As(err, &transientError) {
			continue
		}
		return err
	}

	defer func(resource Resource) {
		err := resource.Close()
		if err != nil {
			return
		}
	}(resource)

	defer func() {
		if e := recover(); e != nil {
			if err = e.(error); errors.As(err, &frobError) {
				resource.Defrob(frobError.defrobTag)
			}
		}
	}()

	resource.Frob(input)
	return err
}
