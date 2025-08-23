def one_or_it:
  if . == 0 then "it" 
  else "one"
  end
;

def bottle_message:
  if . == 0 
  then "no more bottles"
  else "\(.) bottle\(if . != 1 then "s" else "" end)"
  end
;

def capitalize:
  split("")
  | [first | ascii_upcase] + .[1:]
  | join("")
;

def verse:
  [
    "\(bottle_message | capitalize) of beer on the wall, \(bottle_message) of beer.",
    if . == 0
    then "Go to the store and buy some more, 99 bottles of beer on the wall."
    else . - 1 | "Take \(one_or_it) down and pass it around, \(bottle_message) of beer on the wall."
    end
  ]
;

def lyrics:
  if .takeDown == 1 
  then .startBottles | verse
  else 
    (.startBottles | verse)
    + [""]
    + (
        {
          startBottles: (.startBottles - 1),
          takeDown: (.takeDown - 1)
        } | lyrics
      )
  end
;

lyrics