# Define the alphabet as a string
"ABCDEFGHIJKLMNOPQRSTUVWXYZ" as $alphabet

# For each letter in the alphabet
| .letter as $current_letter
# Determine the position (1-based index) of the current letter in the alphabet
| ($alphabet | index($current_letter) + 1) as $letter_position
# Generate a substring of the alphabet up to the determined length
| ($alphabet / "")[0:$letter_position] as $letters_substring
# Create a blank row of dots with the same length as the letter's position
| ("." * $letter_position) as $blank_row

# Initialize an array to hold the top half of the diamond
| [
    # Loop through each position up to the letter's position
    foreach range($letter_position) as $index (null; null;
      # Create the right half of the row by replacing the dot at position $index with the letter
      ($blank_row[0:$index] + $letters_substring[$index] + $blank_row[$index+1:$letter_position]) as $right_half
      # Create the left half by reversing the right half (excluding the first character)
      | ($right_half[1:$letter_position] | explode | reverse | implode) as $left_half
      # Combine the left and right halves to form the complete row
      | $left_half + $right_half
    )
  ]

# Add the bottom half of the diamond by reversing the top half (excluding the middle row)
| . + (.[0:length - 1] | reverse)
# Join the rows into a single string with newline characters
| join("\n")
