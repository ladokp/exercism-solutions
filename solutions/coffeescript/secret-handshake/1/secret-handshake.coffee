class SecretHandshake
  @allowed_actions = ["wink", "double blink", 
                      "close your eyes", "jump"]
  
  @commands: (number) ->
    actions = SecretHandshake.allowed_actions
      .filter (_, index) -> number & Math.pow 2, index
    if number & Math.pow 2, 4
      actions.reverse()
    else
      actions

module.exports = SecretHandshake
