local function ability(scores)
  table.sort(scores, function(a, b) return a > b end)
  return scores[3] + scores[2] + scores[1]
end

local function roll_dice()
  return {
    math.random(1, 6),
    math.random(1, 6),
    math.random(1, 6),
    math.random(1, 6)
  }
end

local function modifier(input)
  return math.floor((input - 10) / 2)
end

local function new_character(self, name)
  local constitution = ability(roll_dice())

  return {
    name = name,
    strength = ability(roll_dice()),
    dexterity = ability(roll_dice()),
    constitution = constitution,
    intelligence = ability(roll_dice()),
    wisdom = ability(roll_dice()),
    charisma = ability(roll_dice()),
    hitpoints = 10 + modifier(constitution)
  }
end

return {
  Character = { new = new_character },
  ability = ability,
  roll_dice = roll_dice,
  modifier = modifier
}