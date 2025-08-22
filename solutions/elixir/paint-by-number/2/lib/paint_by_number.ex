defmodule PaintByNumber do
  def palette_bit_size(color_count) when color_count == 0, do: 0
  def palette_bit_size(color_count) when color_count <= 2, do: 1
  def palette_bit_size(color_count), do: 1 + palette_bit_size(color_count/2)

  def empty_picture(), do: <<>>

  def test_picture() do
    <<0::2, 1::2, 2::2, 3::2>>
  end

  def prepend_pixel(picture, color_count, pixel_color_index) do
    palette_bits = palette_bit_size(color_count)
    <<pixel_color_index::size(palette_bits), picture::bitstring>>
  end

  def get_first_pixel(<<>>, _color_count), do: nil
  def get_first_pixel(picture, color_count) do
    palette_bits = palette_bit_size(color_count)
    <<first_pixel::size(palette_bits), _rest_pixels::bitstring>> = picture
    first_pixel
  end

  def drop_first_pixel(<<>>, _color_count), do: <<>>
  def drop_first_pixel(picture, color_count) do    
    palette_bits = palette_bit_size(color_count)
    <<_first_pixel::size(palette_bits), rest_pixels::bitstring>> = picture
    rest_pixels
  end

  def concat_pictures(picture1, picture2) do
    <<picture1::bitstring, picture2::bitstring>>
  end
end
