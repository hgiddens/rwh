import System.IO
import Data.Char (toUpper)

main = do inh <- openFile "input.txt" ReadMode
          outh <- openFile "output.txt" WriteMode
          mainloop inh outh
          hClose outh
          hClose inh

mainloop :: Handle -> Handle -> IO ()
mainloop inh outh = do ineof <- hIsEOF inh
                       if ineof
                       then return ()
                       else do inpStr <- hGetLine inh
                               hPutStrLn outh (map toUpper inpStr)
                               mainloop inh outh
