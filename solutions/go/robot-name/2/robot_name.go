package robotname

import (
	"math/rand"
	"strings"
	"time"
)

var seededRand = rand.New(
	rand.NewSource(time.Now().UnixNano()))

var usedIds = ""

func StringWithCharset(length int, charset string) string {
	b := make([]byte, length)
	for i := range b {
		b[i] = charset[seededRand.Intn(len(charset))]
	}
	return string(b)
}

// The Robot type describes a Robot and its ID
type Robot struct {
	ID string
}

func (r *Robot) Name() (string, error) {
	if r.ID == "" {
		for {
			var newId = StringWithCharset(2, "ABCDEFGHIJKLMNOPQRSTUVWXYZ") + StringWithCharset(3, "01234567890")
			if !strings.Contains(usedIds, newId) {
				usedIds += "," + newId
				r.ID = newId
				break
			}
		}
	}
	return r.ID, nil
}

func (r *Robot) Reset() {
	r.ID = ""
	r.Name()
}
