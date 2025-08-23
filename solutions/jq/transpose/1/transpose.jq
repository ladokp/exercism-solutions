.lines
| map(split(""))
| transpose
| map(
  until(length == 0 or last != null; del(last))
  | .[] //= " "
  | join("")
)