.phrase
| ascii_downcase
| [scan("[a-z]")]
| length == (unique | length)
