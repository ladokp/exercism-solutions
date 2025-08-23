# You might want to look at:
#
# - the alternative operator:
#   https://stedolan.github.io/jq/manual/v1.6/#Alternativeoperator://
#
# - string interpolation:
#   https://stedolan.github.io/jq/manual/v1.6/#Stringinterpolation-%5C(foo)

def check_name: if .name == null then "you" else .name end;

"One for \(check_name), one for me."
