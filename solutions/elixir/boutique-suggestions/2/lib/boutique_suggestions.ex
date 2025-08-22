defmodule BoutiqueSuggestions do
  def get_combinations(tops, bottoms, options \\ %{}) do
    for top <- tops,
        bottom <- bottoms,
          top.base_color != bottom.base_color
          && top.price + bottom.price <= (options[:maximum_price] || 100.00) do
      {top, bottom}
    end
  end
end
