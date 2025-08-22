{
  print isIsogram($0)
}

function isIsogram(string) {
  string = tolower(string)
  gsub(/[^a-z]+/, "", string)
  split(string, characters, "")
  for (i in characters) {
    current_character = characters[i]
    if (unique_characters[current_character] == 1) {
      return "false"
    }
    unique_characters[current_character] = 1
  }
  return "true"
}