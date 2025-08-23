# Helper function to check if a number is within a given range
def between($lo; $hi): $lo <= . and . < $hi;

# Function to count neighbouring mines around a given cell (i, j)
def count_neighbouring_mines($i; $j):
  . as $m
  | [[-1,-1], [-1,0], [-1,1],
     [ 0,-1],        [ 0,1],
     [ 1,-1], [ 1,0], [ 1,1]] as $neighbours
  | reduce $neighbours[] as [$di, $dj] (
      0;
      . + if  ($i + $di | between(0; $m | length)) and
              ($j + $dj | between(0; $m[$i] | length)) and
              $m[$i + $di][$j + $dj] == 99
          then 1
          else 0
          end
    )
;

# Function to annotate the matrix with mine counts
def annotate:
  . as $m
  | reduce range(length) as $i ([]; . + [
      reduce range($m[$i] | length) as $j ([]; . + [
        if $m[$i][$j] == 99
          then 99
          else ($m | count_neighbouring_mines($i; $j))
        end
      ])
    ])
;

# Convert input string to numeric array
# "*" indicates a mine (99); "." indicates no mine (0)
def to_numeric_array:
  if . == "\n" then [[]] else
    rtrimstr("\n")
    | split("\n")
    | map([ split("")[] | if . == "." then 0 else 99 end ])
  end
;

# Convert numeric array back to string array
# 99 (mine) is represented by "*", other numbers by their value
def to_string_array:
  map(
    map(if . == 0 then "." elif . == 99 then "*" else tostring end)
    | join("")
  )
;

# Main pipeline: convert to numeric array, annotate, and convert back to string array
to_numeric_array
| annotate
| to_string_array
