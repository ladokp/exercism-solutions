local colors = {
        "black", "brown", "red", "orange", "yellow", 
        "green", "blue", "violet", "grey", "white"
}

return {
  color_code = function(color)
    for index, value in ipairs(colors) do
      if value == color then
        return index - 1
      end
    end
  end
}
