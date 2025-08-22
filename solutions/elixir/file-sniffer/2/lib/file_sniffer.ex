defmodule FileSniffer do
 @types [
    # {extension, type, binary}
    {"exe", "application/octet-stream", <<0x7F, 0x45, 0x4C, 0x46>>},
    {"bmp", "image/bmp", <<0x42, 0x4D>>},
    {"png", "image/png", <<0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A>>},
    {"jpg", "image/jpg", <<0xFF, 0xD8, 0xFF>>},
    {"gif", "image/gif", <<0x47, 0x49, 0x46>>}
  ]

  def type_from_extension(extension) do
    Enum.find_value(@types, fn {ext, type, _} ->
      if ext == extension, do: type
    end)
  end

  def type_from_binary(file_binary) do
    Enum.find_value(@types, fn {_, type, binary} ->
      if String.starts_with?(file_binary, binary), do: type
    end)
  end

  def verify(file_binary, extension) do
    type1 = type_from_binary(file_binary)
    type2 = type_from_extension(extension)
    if type1 != nil && type1 == type2 do
      {:ok, type1}
    else
      {:error, "Warning, file format and file extension do not match."}
    end
  end
end
