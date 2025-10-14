local function roll_dice()
  return {
    math.random(1, 6),
    math.random(1, 6),
    math.random(1, 6),
    math.random(1, 6)
  }
end

local function ability()
  local scores = roll_dice()
  table.sort(scores, function(a, b) return a > b end)
  return scores[3] + scores[2] + scores[1]
end

local function modifier(input)
  return math.floor((input - 10) / 2)
end

local function new_character(self, name)
  local constitution = ability()

  return {
    name = name,
    strength = ability(),
    dexterity = ability(),
    constitution = constitution,
    intelligence = ability(),
    wisdom = ability(),
    charisma = ability(),
    hitpoints = 10 + modifier(constitution)
  }
end

return {
  Character = { new = new_character },
  ability = ability,
  roll_dice = roll_dice,
  modifier = modifier
}