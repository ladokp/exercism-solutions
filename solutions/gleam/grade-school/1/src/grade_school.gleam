import gleam/dict
import gleam/list
import gleam/int
import gleam/string

// Type alias for School: a dictionary mapping a grade (Int) to a list of students (List(String))
pub type School = dict.Dict(Int, List(String))

// Create a new, empty school (an empty dictionary)
pub fn create() -> School {
  dict.new() // Initialize an empty dictionary for the school
}

// Add a student to a specified grade in the school.
// Returns an error (Nil) if the student is already present in the school.
pub fn add(
  to school: School,              // The school to which the student will be added
  student student: String,        // The name of the student
  grade grade_num: Int            // The grade to which the student should be added
) -> Result(School, Nil) {        // Result type: Ok(School) if successful, Error(Nil) if the student is already present
  let current = grade(school, grade_num) // Get the current list of students for the grade
  
  // Check if the student already exists in the school
  case list.contains(roster(school), student) {
    False -> 
      // If the student is not in the school, insert them into the appropriate grade,
      // Sort the students alphabetically and update the school dictionary
      Ok(dict.insert(school, grade_num, list.sort([student, ..current], string.compare)))
    _ -> 
      // If the student is already in the school, return an error
      Error(Nil)
  }
}

// Retrieve the list of students for a specific grade.
// If the grade does not exist, return an empty list.
pub fn grade(school: School, desired_grade: Int) -> List(String) {
  // Look up the grade in the school dictionary
  case dict.get(school, desired_grade) {
    Ok(students) -> students  // If found, return the list of students
    _ -> []                   // If not found, return an empty list
  }
}

// Return a complete roster of all students in the school, sorted by grade and then alphabetically within each grade.
pub fn roster(school: School) -> List(String) {
  let grades = list.sort(dict.keys(school), int.compare) // Get a sorted list of all grade levels
  // For each grade, retrieve and combine the sorted list of students into one full roster
  list.flat_map(over: grades, with: fn(grade_num) {
    grade(school, grade_num) // Retrieve the students for each grade
  })
}
