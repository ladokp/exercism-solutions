# We will get more in-depth about jq functions in a later lesson.
# For now, your job is to implement the logic of the tasks in the function
# bodies, so that they output the correct values.

# We haven't covered regular expressions yet.
# This function will remove leading and trailing whitespace
# from the input string.
def trim: sub("^\\s+"; "") | sub("\\s+$"; "");

def split_log_line: split(": ");

# Task 1. Get message from a log line
def message: . | split_log_line | .[1] | trim;

# Task 2. Get log level from a log line
def log_level:
  . | split_log_line | .[0] | 
    if . == "[ERROR]" then "error"
    elif . == "[WARNING]" then "warning"
    elif . == "[INFO]" then "info"
    else ""
    end
;

# Task 3. Reformat a log line
def reformat: message + " (\(log_level))";
