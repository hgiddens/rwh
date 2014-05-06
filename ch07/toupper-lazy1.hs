import System.IO
import Data.Char (toUpper)

main = do inh <- openFile "input.txt" ReadMode
          outh <- openFile "output.txt" WriteMode
          inpStr <- hGetContents inh
          let result = processData inpStr
          hPutStr outh result
          hClose inh
          hClose outh
    where
      processData = map toUpper
