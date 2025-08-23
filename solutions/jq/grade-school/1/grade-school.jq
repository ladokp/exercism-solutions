# Function to transform the input data into a desired format
def transform:
  # Reduce the list of students into an object with 'seen' and 'school' keys
  reduce .input.students[] as [$name, $grade] (
    {seen: {}, school: {}}; 
    # Check if the student's name has not been seen before
    if .seen[$name] == null then
      # Mark the student as seen and add them to the appropriate grade
      .seen[$name] = $grade |
      .school[$grade | tostring] += [$name] // [$name]
    else
      .
    end
  )
  | .school;  # Return the 'school' object

# Function to get students of a specific grade
def get_grade($wanted):
  # Transform the input and return the sorted list of students for the desired grade
  transform | .[$wanted | tostring] // [] | sort;

# Function to transform the input into an array
def transform_to_array:
  transform
  | to_entries              # Convert the object to an array of key-value pairs
  | sort_by(.key | tonumber) # Sort by grade (numeric keys)
  | map(.value | sort)      # Sort names within each grade
  | add // [];              # Flatten the array and return an empty array if null

# Main logic based on the 'property' value
if .property == "roster" then
  (transform_to_array) // []  # Return the sorted roster or an empty array
elif .property == "grade" then
  get_grade(.input.desiredGrade)  # Return the sorted list of students for the desired grade
else
  []
end
