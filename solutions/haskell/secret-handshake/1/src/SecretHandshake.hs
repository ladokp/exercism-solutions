module SecretHandshake (handshake) where

import Data.Bits (testBit)

data Action = Wink
            | DoubleBlink
            | CloseYourEyes
            | Jump
            | Reverse
            deriving (Show, Eq, Enum, Bounded)

handshake :: Int -> [String]
handshake n = applyReverseIfNeeded (map actionToString actions)
  where
    actions = [action | action <- [Wink .. Jump], n `testBit` fromEnum action]

    applyReverseIfNeeded :: [String] -> [String]
    applyReverseIfNeeded actionsList
      | n `testBit` fromEnum Reverse = reverse actionsList
      | otherwise                    = actionsList

    actionToString :: Action -> String
    actionToString Wink          = "wink"
    actionToString DoubleBlink   = "double blink"
    actionToString CloseYourEyes = "close your eyes"
    actionToString Jump          = "jump"
    actionToString Reverse       = undefined  -- Reverse should never be converted to a string
