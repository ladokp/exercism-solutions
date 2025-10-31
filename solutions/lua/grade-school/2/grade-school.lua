-- school.lua

-- Define the 'School' table. This will serve as the class/prototype for School objects.
local School = {}

--- Constructor Method ---
-- Uses the colon ':' syntax, which implicitly passes the 'School' table itself as the first argument, 'self'.
function School:new()
  -- Sets the '__index' metamethod of the 'School' prototype to itself.
  -- This is the core of Lua's OOP inheritance: if a method/field is not found
  -- in a School instance, Lua will look it up in the 'School' table (the prototype).
  self.__index = self

  -- Create the new instance (object). It is an empty table containing only the 'db'.
  -- 'db' (database) will store the school's data: { [grade_number] = {student_names...} }
  -- setmetatable attaches the 'School' prototype (self) as the metatable for the new instance.
  return setmetatable({ db = {} }, self)
end

--- Instance Method: Get Roster ---
-- Returns a flat list of all students in the school.
function School:roster()
  local roster = {} -- Initialize a new table to hold the flat list of student names.

  -- Iterate through all grade tables in the database (e.g., db[1], db[2], etc.)
  for _, grade in pairs(self.db) do
    -- Iterate through the list of students in the current 'grade' table (which is an array).
    for _, student in ipairs(grade) do
      -- Add the student's name to the flat roster table.
      table.insert(roster, student)
    end
  end
  return roster
end

--- Instance Method: Add Student ---
-- Adds a student to a specific grade and maintains an alphabetical list of students per grade.
function School:add(name, grade)
  -- 1. Check for Duplicate Student Name Across All Grades
  -- This loop ensures a student is only enrolled once in the school.
  for _, grade_roster in pairs(self.db) do
    for _, student in ipairs(grade_roster) do
      if student == name then
        return false -- Student already exists, return failure.
      end
    end
  end

  -- 2. Add Student to the Specified Grade
  if self.db[grade] then
    -- The grade exists: insert the new student's name.
    table.insert(self.db[grade], name)
    -- Sort the grade's roster alphabetically after insertion.
    table.sort(self.db[grade])
  else
    -- The grade does not exist: create a new table for the grade with the student's name.
    self.db[grade] = { name }
  end

  return true -- Student was successfully added.
end

--- Instance Method: Get Grade Roster ---
-- Returns the list of students for a single, specified grade.
function School:grade(grade)
  -- Return the table associated with the grade number.
  -- Use 'or {}' to return an empty table if the grade does not exist (instead of 'nil').
  return self.db[grade] or {}
end

-- Return the 'School' table (the class/prototype) so it can be used when the file is required.
return School
