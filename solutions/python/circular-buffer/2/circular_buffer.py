class BufferFullException(BufferError):
    """Exception raised when the CircularBuffer is full.

    Args:
        message (str): A description of the error.
    """
    def __init__(self, message):
        self.message = message


class BufferEmptyException(BufferError):
    """Exception raised when the CircularBuffer is empty.

    Args:
        message (str): A description of the error.
    """
    def __init__(self, message):
        self.message = message


class CircularBuffer:
    def __init__(self, capacity):
        """Initialize a CircularBuffer with a given capacity.

        Args:
            capacity (int): The maximum number of items the buffer can hold.
        """
        self.buffer = []
        self.capacity = capacity

    def read(self):
        """Read and remove the oldest item from the buffer.

        Raises:
            BufferEmptyException: If the buffer is empty.

        Returns:
            The oldest item in the buffer.
        """
        if not self.buffer:
            raise BufferEmptyException("Circular buffer is empty")
        return self.buffer.pop(0)

    def write(self, data):
        """Write a new item to the buffer.

        Raises:
            BufferFullException: If the buffer is full.

        Args:
            data: The item to be added to the buffer.
        """
        if len(self.buffer) == self.capacity:
            raise BufferFullException("Circular buffer is full")
        self.buffer.append(data)

    def overwrite(self, data):
        """Overwrite the oldest item in the buffer if it is full.

        Args:
            data: The item to be added to the buffer.
        """
        if len(self.buffer) == self.capacity:
            self.read()
        self.write(data)

    def clear(self):
        """Clear all items from the buffer."""
        self.buffer = []
