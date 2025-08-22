defmodule Strain do
  @doc """
  Given a `list` of items and a function `fun`, return the list of items where
  `fun` returns true.

  Do not use `Enum.filter`.
  """
  @spec keep(list :: list(any), fun :: (any -> boolean)) :: list(any)
  def keep([], _), do: []

  def keep([ head | tail ], fun) do
    if fun.(head) do
      [ head | keep(tail, fun) ]
    else
      keep(tail, fun)
    end
  end

  @doc """
  Given a `list` of items and a function `fun`, return the list of items where
  `fun` returns false.

  Do not use `Enum.reject`.
  """
  @spec discard(list :: list(any), fun :: (any -> boolean)) :: list(any)
  def discard([], _), do: []

  def discard([ head | tail ], fun) do
    do_not_keep = fn x -> !fun.(x) end
    if fun.(head) do
      keep(tail, do_not_keep)
    else
      [ head | keep(tail, do_not_keep) ]
    end
  end
end
