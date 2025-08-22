module FoodChain (song) where

-- | Data type representing different types of verses in the song.
--   AnimalVerse: A verse for animals with no additional lines (e.g., "fly").
--   AnimalVerseWithFinal: A verse for animals with a unique line and the final line.
--   AnimalsVerse: A verse for animals that include the "swallow" chain (e.g., "cow", "goat").
data Verse = AnimalVerse String 
           | AnimalVerseWithFinal String 
           | AnimalsVerse [String]

-- | Show instance to define how each verse should be displayed in the song.
instance Show Verse where
  show verse = case verse of
      -- Simple verse with no additional lines, just "I don't know why she swallowed..."
      AnimalVerse animal -> intro animal <> animalPhrase animal
      -- Verse with a unique line and the final repeating lines.
      AnimalVerseWithFinal animal -> intro animal <> animalPhrase animal <> finalLines
      -- Verse with a series of "swallowed" animals, building up the chain.
      AnimalsVerse animals -> show (AnimalVerse (head animals))
                              <> concatMap swallowToCatch (zip animals (tail animals))
                              <> birdToSpiderLine
                              <> finalLines
    where
      -- Line introducing the animal the old lady swallowed.
      intro animal = "I know an old lady who swallowed a " <> animal <> ".\n"
      
      -- Chain of lines for swallowing each animal to catch the previous.
      swallowToCatch (hunter, prey) = "She swallowed the " <> hunter <> " to catch the " <> prey <> ".\n"
      
      -- Special line for the "bird" to "spider" interaction.
      birdToSpiderLine = "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.\n"
      
      -- Final repeating lines for most verses.
      finalLines = "She swallowed the spider to catch the fly.\nI don't know why she swallowed the fly. Perhaps she'll die.\n\n"

-- | Generates the unique phrase for each animal.
animalPhrase :: String -> String
animalPhrase animal = case animal of 
  "fly"    -> "I don't know why she swallowed the fly. Perhaps she'll die.\n\n"
  "spider" -> "It wriggled and jiggled and tickled inside her.\n"
  "cow"    -> "I don't know how she swallowed a cow!\n"
  "goat"   -> "Just opened her throat and swallowed a goat!\n"
  "dog"    -> "What a hog, to swallow a dog!\n"
  "cat"    -> "Imagine that, to swallow a cat!\n"
  "bird"   -> "How absurd to swallow a bird!\n"
  "horse"  -> "She's dead, of course!\n"
  _        -> ""

-- | Full song composed by combining each verse.
song :: String
song = concatMap show $ [AnimalVerse "fly", AnimalVerseWithFinal "spider"] 
       <> map AnimalsVerse animalSequence 
       <> [AnimalVerse "horse"]
  where 
    -- List of animal sequences for verses that build the swallowing chain.
    animalSequence = tail $ reverse $ scanr (:) [] ["cow", "goat", "dog", "cat", "bird"]
