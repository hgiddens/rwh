-- file: ch01/WC.hs
-- lines beginning with "--" are comments.

main = interact wordCount''
    where
      -- lines
      wordCount input = show (length (lines input)) ++ "\n"
      -- words
      wordCount' input = ((show . length . words) input) ++ "\n"
      -- characters
      wordCount'' input = ((show . length) input) ++ "\n"
