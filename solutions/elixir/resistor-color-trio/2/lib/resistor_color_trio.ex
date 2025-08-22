defmodule ResistorColorTrio do
  @color_values %{
    black: 0,
    brown: 1,
    red: 2,
    orange: 3,
    yellow: 4,
    green: 5,
    blue: 6,
    violet: 7,
    grey: 8,
    white: 9
  }

  @doc """
  Calculate the resistance value in ohm or kiloohm from resistor colors
  """
  @spec label(colors :: [atom]) :: {number, :ohms | :kiloohms | :megaohms | :gigaohms}
  def label([first | [second | [third | _]]]) do
    value = (@color_values[first] * 10 + @color_values[second]) * Integer.pow(10, @color_values[third])
    unit = if value < 1000, do: :ohms, else: if value < 1000000, do: :kiloohms, else: if value < 1000000000, do: :megaohms, else: :gigaohms
    case unit do
      :ohms -> {value, unit}
      :kiloohms -> {div(value, 1000), unit}
      :megaohms -> {div(value, 1000000), unit}
      :gigaohms -> {div(value, 1000000000), unit}
    end
  end
end
