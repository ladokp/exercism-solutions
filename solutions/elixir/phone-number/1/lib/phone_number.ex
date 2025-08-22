defmodule PhoneNumber do
  @doc """
  Cleans the formatting from a phone number if the given number is valid. 
  It removes separators and validates the number according to specific rules:
    - The phone number must contain digits only.
    - The phone number must have a valid length (10 or 11 digits).
    - If the number has 11 digits, it must start with '1'.
    - The area code and exchange code must not start with '0' or '1'.

  Returns:
    - `{:ok, cleaned_number}` if the phone number is valid.
    - `{:error, reason}` if the phone number is invalid.
  """
  @spec clean(String.t()) :: {:ok, String.t()} | {:error, String.t()}
  def clean(raw) do
    validators = [
      &validate_digits/1,
      &validate_length/1,
      &validate_country_code/1,
      &validate_area_code/1,
      &validate_exchange_code/1
    ]

    Enum.reduce_while(validators, {:ok, remove_separators(raw)}, fn validator, acc ->
      {:ok, number} = acc

      case validator.(number) do
        {:ok, number} -> {:cont, {:ok, number}}
        {:error, error} -> {:halt, {:error, error}}
      end
    end)
  end

  @doc """
  Removes any separator characters from the phone number.
  Separator characters include: spaces, periods, parentheses, hyphens.
  """
  @spec remove_separators(String.t()) :: String.t()
  defp remove_separators(number) do
    String.replace(number, ~r/[\s+.()-]/, "")
  end

  @doc """
  Ensures the phone number contains only digits.
  Returns:
    - `{:ok, number}` if the number contains digits only.
    - `{:error, "must contain digits only"}` if invalid characters are found.
  """
  @spec validate_digits(String.t()) :: {:ok, String.t()} | {:error, String.t()}
  defp validate_digits(number) do
    valid_digits? =
      number
      |> String.codepoints()
      |> Enum.all?(fn <<x>> -> x in ?0..?9 end)

    if valid_digits?, do: {:ok, number}, else: {:error, "must contain digits only"}
  end

  @doc """
  Validates the length of the phone number.
  Returns:
    - `{:ok, number}` if the length is valid (10 or 11 digits).
    - `{:error, "must not be fewer than 10 digits"}` if the number is too short.
    - `{:error, "must not be greater than 11 digits"}` if the number is too long.
  """
  @spec validate_length(String.t()) :: {:ok, String.t()} | {:error, String.t()}
  defp validate_length(number) do
    length = String.length(number)

    cond do
      length < 10 -> {:error, "must not be fewer than 10 digits"}
      length > 11 -> {:error, "must not be greater than 11 digits"}
      true -> {:ok, number}
    end
  end

  @doc """
  Validates the country code if the number has 11 digits. The country code must be '1'.
  If valid, the country code is removed from the number.
  Returns:
    - `{:ok, number_without_country_code}` if the country code is valid.
    - `{:error, "11 digits must start with 1"}` if the country code is invalid.
  """
  @spec validate_country_code(String.t()) :: {:ok, String.t()} | {:error, String.t()}
  defp validate_country_code(number) do
    case String.length(number) do
      10 ->
        {:ok, number}

      11 ->
        if String.starts_with?(number, "1") do
          {:ok, number |> String.split_at(1) |> elem(1)}
        else
          {:error, "11 digits must start with 1"}
        end
    end
  end

  @doc """
  Validates the area code of the phone number.
  The area code must not start with '0' or '1'.
  Returns:
    - `{:ok, number}` if the area code is valid.
    - `{:error, "area code cannot start with zero"}` if it starts with '0'.
    - `{:error, "area code cannot start with one"}` if it starts with '1'.
  """
  @spec validate_area_code(String.t()) :: {:ok, String.t()} | {:error, String.t()}
  defp validate_area_code(number) do
    {area_code, _} = String.split_at(number, 3)

    cond do
      String.starts_with?(area_code, "0") -> {:error, "area code cannot start with zero"}
      String.starts_with?(area_code, "1") -> {:error, "area code cannot start with one"}
      true -> {:ok, number}
    end
  end

  @doc """
  Validates the exchange code of the phone number.
  The exchange code must not start with '0' or '1'.
  Returns:
    - `{:ok, number}` if the exchange code is valid.
    - `{:error, "exchange code cannot start with zero"}` if it starts with '0'.
    - `{:error, "exchange code cannot start with one"}` if it starts with '1'.
  """
  @spec validate_exchange_code(String.t()) :: {:ok, String.t()} | {:error, String.t()}
  defp validate_exchange_code(number) do
    {_area_code, rest} = String.split_at(number, 3)
    {exchange_code, _} = String.split_at(rest, 3)

    cond do
      String.starts_with?(exchange_code, "0") -> {:error, "exchange code cannot start with zero"}
      String.starts_with?(exchange_code, "1") -> {:error, "exchange code cannot start with one"}
      true -> {:ok, number}
    end
  end
end
