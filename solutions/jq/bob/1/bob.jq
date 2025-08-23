def is_yelling:
  test("[[:upper:]]") and (test("[[:lower:]]") | not);

def is_questioning:
  test("\\?\\s*$");

def is_silent:
  test("^\\s*$");

.heyBob |
if is_yelling and is_questioning then
  "Calm down, I know what I'm doing!"
elif is_yelling then
  "Whoa, chill out!"
elif is_questioning then
  "Sure."
elif is_silent then
  "Fine. Be that way!"
else
  "Whatever."
end
