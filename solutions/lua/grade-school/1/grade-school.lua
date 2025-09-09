-- Define a table to represent the School "class"
local School = {}

-- Create a new instance of the School.
-- This is the constructor-like function.
-- It initializes an empty database (db) for storing students by grade.
function School:new()
  -- Set the __index metamethod to allow method lookups on the class
  self.__index = self
  -- Create a new School object with an empty db (a dictionary where the keys are grades and the values are lists of student names)
  return setmetatable({ db = {} }, self)
end

-- Retrieve the full roster of the school.
-- This returns the entire database (all grades and their students).
function School:roster()
  return self.db
end

-- Add a student to a specific grade in the school.
-- If the grade already exists, the student is added to the list for that grade,
-- and the list is sorted alphabetically after insertion.
-- If the grade doesn't exist, a new list is created with the student's name.
-- @param name: The student's name (string)
-- @param grade: The grade level (integer)
function School:add(name, grade)
  -- Check if the grade already exists in the database
  if self.db[grade] then
    -- If it does, insert the student's name and sort the list alphabetically
    table.insert(self.db[grade], name)
    table.sort(self.db[grade])
  else
    -- If the grade doesn't exist, create a new entry with the student's name
    self.db[grade] = { name }
  end
end

-- Retrieve the list of students for a specific grade.
-- If the grade exists, return the list of students.
-- If the grade doesn't exist, return an empty table.
-- @param grade: The grade level (integer)
-- @return: A list of student names for the given grade, or an empty list if the grade has no students.
function School:grade(grade)
  -- Check if the grade exists in the database
  if self.db[grade] then
    -- If it exists, return the list of students for the grade
    return self.db[grade]
  end
  -- If the grade doesn't exist, return an empty table
  return {}
end

-- Return the School module (the class-like table)
return School
