return {
  encode = function (data)
    for letter in ("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"):gmatch('.') do
      data = data:gsub(letter..'*', function (run)
        return (#run > 1 and tostring(#run) or '') .. run:sub(1,1)
      end)
    end
    return data
  end,

  decode = function (compressed)
    return compressed:gsub('(%d+)(%a)', function (num, letter)
      return string.rep(letter, num)
    end)
  end
}