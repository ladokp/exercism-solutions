def transcoder:
  "abcdefghijklmnopqrstuvwxyz"
  | split("")
  | [., reverse]
  | transpose
  | map({key: first, value: last})
  | from_entries
;

def transcode:
  transcoder as $tr
  | ascii_downcase
  | [scan("[[:alnum:]]")]
  | map($tr[.] // .)
  | join("")
;

def chunk:
  if length <= 5 then
    .
  else
    "\(.[:5]) \(.[5:] | chunk)"
  end
;

(.property == "encode") as $willChunk
| .input.phrase
| transcode
| if $willChunk then chunk else . end
