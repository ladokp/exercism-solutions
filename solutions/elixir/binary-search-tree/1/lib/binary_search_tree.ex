defmodule BinarySearchTree do
  def new(data), do: %{data: data, left: nil, right: nil}

  def insert(nil, node_value), do: new(node_value)
  def insert(%{data: data, left: left, right: right}, node_value) do
    if node_value <= data do
      %{data: data, left: insert(left, node_value), right: right}
    else
      %{data: data, left: left, right: insert(right, node_value)}
    end
  end

  def  in_order(tree), do: in_order(tree, []) |> Enum.reverse()
  defp in_order(nil, accum), do: accum
  defp in_order(tree, accum) do
    in_order(tree.right, [tree.data | in_order(tree.left, accum)])
  end
end