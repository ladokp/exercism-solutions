package circular

import "fmt"

type Buffer struct {
	buffer chan byte
}

func NewBuffer(size int) *Buffer {
	return &Buffer{buffer: make(chan byte, size)}
}

func (b *Buffer) ReadByte() (byte, error) {
	select {
	case out := <-b.buffer:
		return out, nil
	default:
		return '0', fmt.Errorf("no element in the buffer")
	}
}

func (b *Buffer) WriteByte(c byte) error {
	select {
	case b.buffer <- c:
		return nil
	default:
		return fmt.Errorf("buffer is full")
	}
}

func (b *Buffer) Reset() {
	for {
		if _, err := b.ReadByte(); err != nil {
			return
		}
	}
}

func (b *Buffer) Overwrite(c byte) {
	if err := b.WriteByte(c); err != nil {
		b.ReadByte()
		b.WriteByte(c)
	}
}
