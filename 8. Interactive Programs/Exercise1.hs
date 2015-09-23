-- Exercise1.hs

getLine' :: IO String
getLine' = do c <- getChar
              if c == '\n' then
                return []
              else do cs <- getLine'
                      return (c : cs)

readLine :: IO String
readLine = readLine2 []

readLine2 :: String -> IO String
readLine2 acc = do c <- getChar
                   if c == '\n' then
                     return acc
                   else if c == '\DEL' && (not . null $ acc) then
                     backspace >> readLine2 (init acc)
                   else
                     readLine2 (acc ++ [c])

backup :: IO ()
backup = putStr "\ESC[1D"
 
backspace :: IO ()
backspace = 
  backup >> backup >> backup >> 
  putStr "   " >> 
  backup >> backup >> backup

