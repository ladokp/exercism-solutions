# Define a function to perform a global replacement until no more replacements are available.
def gsub_recursively(pattern; replacement):
  def recurse: gsub(pattern; replacement) as $replaced | if $replaced != . then $replaced | recurse else $replaced end;
  recurse
;

# Clean and process the input value
.value
| gsub("[^[\\](){}]"; "")                # Remove all characters except brackets
| gsub_recursively("(\\[\\])|(\\(\\))|(\\{\\})"; "")  # Recursively remove matching pairs of brackets
| . == ""                                # Check if the resulting string is empty
