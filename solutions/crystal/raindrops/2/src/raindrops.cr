class Raindrops
  def self.convert(num : Int32) : String
    sound = ""
    sound += "Pling" if num % 3 == 0
    sound += "Plang" if num % 5 == 0
    sound += "Plong" if num % 7 == 0
    return "#{num}" if sound.blank?
    sound
  end
end
