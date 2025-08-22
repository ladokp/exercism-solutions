defmodule KitchenCalculator do
  def get_volume(volume_pair) do
    elem(volume_pair, 1)
  end

  def to_milliliter({:cup, _} = volume_pair) do
    {:milliliter, elem(volume_pair, 1)*240}
  end

  def to_milliliter({:fluid_ounce, _} = volume_pair) do
    {:milliliter, elem(volume_pair, 1)*30}
  end

  def to_milliliter({:teaspoon, _} = volume_pair) do
    {:milliliter, elem(volume_pair, 1)*5}
  end

  def to_milliliter({:tablespoon, _} = volume_pair) do
    {:milliliter, elem(volume_pair, 1)*15}
  end

  def to_milliliter({:milliliter, _} = volume_pair) do
    volume_pair
  end

  def from_milliliter(volume_pair, :cup = unit) do
    {unit, elem(volume_pair, 1)/240}
  end

  def from_milliliter(volume_pair, :fluid_ounce = unit) do
    {unit, elem(volume_pair, 1)/30}
  end

  def from_milliliter(volume_pair, :teaspoon = unit) do
    {unit, elem(volume_pair, 1)/5}
  end

  def from_milliliter(volume_pair, :tablespoon = unit) do
    {unit, elem(volume_pair, 1)/15}
  end

  def from_milliliter(volume_pair, :milliliter = _unit) do
    volume_pair
  end

  def convert(volume_pair, unit) do
    {unit, elem(from_milliliter(to_milliliter(volume_pair), unit), 1)}
  end
end
