class GradeSchool
  constructor: () ->
    @students = {}  # Initialize an empty object to store students and their grade levels

  # Method to add a student to a specific grade level
  add: (student, level) ->
    if @students[student]  # Check if the student is already in the list
      false  # Return false if the student already exists
    else
      @students[student] = level  # Add the student with the specified grade level
      true  # Return true indicating the student was successfully added

  # Method to retrieve all students in a specific grade level
  grade: (level) ->
    result = []  # Initialize an empty array to store students in the specified grade
    for student, grade of @students  # Iterate over each student and their grade level
      if grade is level  # Check if the student's grade matches the specified level
        result.push student  # Add the student to the result array
    result.sort()  # Sort the list of students alphabetically
    result  # Return the sorted list of students

  # Method to get a roster of all students, sorted by grade level
  roster: () ->
    result = []  # Initialize an empty array to store the final roster
    grades = (g for _, g of @students).sort()  # Get a sorted list of all grade levels
    for g in grades.unique()  # Iterate over each unique grade level
      result.push (@grade g)...  # Append all students in that grade to the result
    result  # Return the final roster

# Extend the Array prototype to include a unique method
Array::unique = ->
  output = {}  # Initialize an empty object to track unique elements
  output[@[key]] = @[key] for key in [0...@length]  # Populate the object with unique array elements
  value for key, value of output  # Return an array of the unique values

# Export the GradeSchool class as a module
module.exports = GradeSchool
