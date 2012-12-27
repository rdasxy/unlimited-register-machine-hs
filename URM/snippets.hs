 --put type signatures
                           -execute regs pc 
                                   | ir == Zero r = run (take r regs ++ [0] ++ drop (r+1) regs) (pc + 1)
                                   | ir == Jump x y n = run regs (if x == y
                                                                     then n else pc + 1)
                                   | ir == Incr x = run (take x regs ++ [(regs !! x) + 1] ++ drop (x+1) regs) (pc + 1)
                                   | ir == Assign x y = run (take x regs ++ [y] ++ drop (x+1) regs) (pc + 1)
                                     where ir = program !! pc-




--let currentTokens = stringSplit (==' ')  (program !! pc)  in print $ currentTokens

stringSplit     :: (Char -> Bool) -> String -> [String]
stringSplit p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : stringSplit p s''
                            where (w, s'') = break p s'
