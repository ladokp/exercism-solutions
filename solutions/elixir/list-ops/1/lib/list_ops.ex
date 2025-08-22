defmodule ListOps do
  # Please don't use any external modules (especially List or Enum) in your
  # implementation. The point of this exercise is to create these basic
  # functions yourself. You may use basic Kernel functions (like `Kernel.+/2`
  # for adding numbers), but please do not use Kernel functions for Lists like
  # `++`, `--`, `hd`, `tl`, `in`, and `length`.

  @spec count(list) :: non_neg_integer
  def count(l), do: foldl(l, 0, fn _, len -> len + 1 end)

  @spec reverse(list) :: list
  def reverse(l), do: foldl(l, [], &[&1 | &2])

  @spec map(list, (any -> any)) :: list
  def map(list, f), do: foldr(list, [], &[f.(&1) | &2])

  @spec filter(list, (any -> as_boolean(term))) :: list
  def filter(list, f), do: foldr(list, [], &(if f.(&1), do: [&1 | &2], else: &2))

  @type acc :: any
  @spec foldl(list, acc, (any, acc -> acc)) :: acc
  def foldl([], acc, _), do: acc
  def foldl([h | t], acc, f), do: foldl(t, f.(h, acc), f)

  @spec foldr(list, acc, (any, acc -> acc)) :: acc
  def foldr([], acc, _), do: acc
  def foldr([h | t], acc, f), do: f.(h, foldr(t, acc, f))

  @spec append(list, list) :: list
  def append(a, b), do: foldr(a, b, &[&1 | &2])

  @spec concat([[any]]) :: [any]
  def concat(ll), do: foldr(ll, [], &append/2)
end