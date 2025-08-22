"""
This module provides classes for metered file and socket operations,
tracking the number of bytes and operations performed.

Classes:
- MeteredFile: A file-like object that tracks read and write operations.
- MeteredSocket: A socket wrapper that tracks send and receive operations.
"""

from io import BufferedRandom


class MeteredFile(BufferedRandom):
    """A file-like object that tracks read and write operations,
    including the number of bytes read or written and the number of operations performed.
    """

    def __init__(self, *args, **kwargs):
        """Initializes a MeteredFile object with counters for read/write operations."""
        super().__init__(*args, **kwargs)
        self._read_bytes = self._read_ops = self._write_bytes = (
            self._write_ops
        ) = 0

    def __enter__(self):
        """Enters a runtime context related to this object."""
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        """Exits the runtime context related to this object."""
        return super().__exit__(exc_type, exc_val, exc_tb)

    def __iter__(self):
        """Returns the iterator object itself."""
        return self

    def __next__(self):
        """Reads the next line from the file. Tracks the read operation and count of bytes."""
        data = super().readline()
        self._read_bytes += len(data)
        self._read_ops += 1
        if data:
            return data
        raise StopIteration

    def read(self, size=-1):
        """Read 'size' bytes from the file. Tracks the read operation and count of bytes."""
        data = super().read(size)
        self._read_bytes += len(data)
        self._read_ops += 1
        return data

    @property
    def read_bytes(self):
        """Returns the total number of bytes read from the file."""
        return self._read_bytes

    @property
    def read_ops(self):
        """Returns the total number of read operations performed."""
        return self._read_ops

    def write(self, b):
        """Write data to the file. Tracks the write operation and count of bytes."""
        write_length = super().write(b)
        self._write_bytes += write_length
        self._write_ops += 1
        return write_length

    @property
    def write_bytes(self):
        """Returns the total number of bytes written to the file."""
        return self._write_bytes

    @property
    def write_ops(self):
        """Returns the total number of write operations performed."""
        return self._write_ops


class MeteredSocket:
    """A socket wrapper that tracks send and receive operations,
    including the number of bytes sent or received and the number of operations performed.
    """

    def __init__(self, socket):
        """Initializes a MeteredSocket object with counters for send/receive operations."""
        self.socket = socket
        self._recv_bytes = self._recv_ops = self._send_bytes = (
            self._send_ops
        ) = 0

    def __enter__(self):
        """Enters a runtime context related to this object."""
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        """Exits the runtime context related to this object."""
        return self.socket.__exit__(exc_type, exc_val, exc_tb)

    def recv(self, buffer_size, flags=0):
        """Receive data from the socket. Tracks the receive operation and count of bytes."""
        data = self.socket.recv(buffer_size, flags)
        self._recv_bytes += len(data)
        self._recv_ops += 1
        return data

    @property
    def recv_bytes(self):
        """Returns the total number of bytes received from the socket."""
        return self._recv_bytes

    @property
    def recv_ops(self):
        """Returns the total number of receive operations performed."""
        return self._recv_ops

    def send(self, data, flags=0):
        """Send data to the socket. Tracks the send operation and count of bytes."""
        send_len = self.socket.send(data, flags)
        self._send_bytes += send_len
        self._send_ops += 1
        return send_len

    @property
    def send_bytes(self):
        """Returns the total number of bytes sent to the socket."""
        return self._send_bytes

    @property
    def send_ops(self):
        """Returns the total number of send operations performed."""
        return self._send_ops
