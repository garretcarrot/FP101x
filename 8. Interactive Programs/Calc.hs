box :: [String]
box = ["+---------------+",
       "|               |",
       "+---+---+---+---+",
       "| q | c | d | = |",
       "| 1 | 2 | 3 | + |",
       "| 4 | 5 | 6 | - |",
       "| 7 | 8 | 9 | * |",
       "| 0 | ( | ) | / |",
       "+---+---+---+---+"]

buttons :: [Char]
buttons = standard ++ extra
          where standard = "qcd=123+456-789*0()/"
                extra = "QCD \ESC\BS\DEL\n"

-- showbox :: IO ()
-- showbox = seqn [write ]