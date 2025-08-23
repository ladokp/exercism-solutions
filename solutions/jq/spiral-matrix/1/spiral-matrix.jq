# Function to create an n x n matrix filled with zeroes
def make_matrix($n):
  def fill($n; $elem): reduce range($n) as $i ([]; . + [$elem]);
  fill($n; fill($n; 0))
;

# Initial parameters setup
.size as $n
| {matrix: make_matrix($n), x: 0, y: 0, dx: 0, dy: 1, i: 1}
| until(.i > $n * $n;
    # Fill the current position with the current value of .i
    .matrix[.x][.y] = .i

    # Check if the direction needs to change
    | if .x + .dx < 0 or .x + .dx == $n or
         .y + .dy < 0 or .y + .dy == $n or
         .matrix[.x + .dx][.y + .dy] != 0
      then
        # Change direction
        . + {dx: .dy, dy: -.dx}
      else
        .
      end

    # Move in the current direction
    | .x += .dx
    | .y += .dy

    # Increment the value of .i
    | .i += 1
  )
| .matrix
