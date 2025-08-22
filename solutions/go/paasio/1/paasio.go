package paasio

import (
	"io"
	"sync/atomic"
)

type readCounter struct {
	reader  io.Reader
	counter atomic.Uint64
}

type writeCounter struct {
	writer  io.Writer
	counter atomic.Uint64
}

type readWriteCounter struct {
	ReadCounter
	WriteCounter
}

func NewWriteCounter(writer io.Writer) WriteCounter {
	return &writeCounter{
		writer: writer,
	}
}

func NewReadCounter(reader io.Reader) ReadCounter {
	return &readCounter{
		reader: reader,
	}
}

func NewReadWriteCounter(readWriter io.ReadWriter) ReadWriteCounter {
	return &readWriteCounter{
		NewReadCounter(readWriter),
		NewWriteCounter(readWriter),
	}
}

func (rc *readCounter) Read(p []byte) (int, error) {
	bytes, err := rc.reader.Read(p)
	if err == nil {
		inc := uint64(bytes<<32 + 1)
		rc.counter.Add(inc)
	}
	return bytes, err
}

func (rc *readCounter) ReadCount() (int64, int) {
	counts := rc.counter.Load()
	return int64(counts >> 32), int(counts % (1 << 32))
}

func (wc *writeCounter) Write(p []byte) (int, error) {
	bytes, err := wc.writer.Write(p)
	if err == nil {
		inc := uint64(bytes<<32 + 1)
		wc.counter.Add(inc)
	}
	return bytes, err
}

func (wc *writeCounter) WriteCount() (int64, int) {
	counts := wc.counter.Load()
	return int64(counts >> 32), int(counts % (1 << 32))
}
