def new_remote_control_car:
  # Populate the object with the required attributes
  {
      "battery_percentage": 100,
      "distance_driven_in_meters": 0,
      "nickname": null
  }
;

def new_remote_control_car($nickname):
  # Populate the object with the required attributes
  new_remote_control_car + {$nickname}
;

def display_distance:
  # Implement the required output string
  "\(.distance_driven_in_meters // 0) meters"
;

def display_battery:
  # Implement the required output string
  "Battery " + 
  (if .battery_percentage > 0
   then "at \(.battery_percentage)%"
   else "empty" end)
;

def drive:
  # Update the input's attributes as required
  if .battery_percentage == 0
  then .
  else
    .battery_percentage -= 1 |
    .distance_driven_in_meters += 20
  end
;
