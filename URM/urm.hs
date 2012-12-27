import System.IO

-- Z s
-- J s t n
-- A s t
-- I s

type Register = Int
data Instruction =
     Zero Register |
     Incr Register |
     Jump Register Register Register |
     Copy Register Register
     deriving Show

readInt :: String -> Int
readInt s = read s :: Int

readRegister :: String -> Register
readRegister s = read s :: Register

readReg :: String -> [Register]
readReg (s) = map readRegister (words s)

main = do putStr "Please enter program source file name: "
          hFlush stdout
          programFileName <- getLine
          programFileHandle <- openFile programFileName ReadMode
          program <- hGetContents programFileHandle
          putStr "Please enter initial file configuration file name: "
          hFlush stdout
          initConfigFileName <- getLine
          initConfigFileHandle <- openFile initConfigFileName ReadMode
          initConfigStr <- hGetContents initConfigFileHandle
          print (evaluateProgram (lines program) (readReg initConfigStr))

parse :: String -> Instruction
parse (s : ss)
      | s == 'Z' = Zero (readInt ss)
      | s == 'J' = Jump r1 r2 r3
      | s == 'A' = Copy r4 r5
      | s == 'I' = Incr (readInt ss)
        where r1 : r2 : r3 : [] = map readInt (words ss)
              r4 : r5 : [] = map readInt (words ss)

evaluateProgram :: [String] -> [Register] -> Register 
evaluateProgram prog regs = let program = map parse (prog) -- Gives list of Instructions
                                run :: [Register] -> Register -> Register
                                run regs pc = if pc >= length program
                                                 then regs !! 0
                                                 else execute regs pc
                                execute :: [Register] -> Register -> Register
                                execute regs pc =
                                   case program !! pc of
                                        Zero r -> run (take r regs ++ [0] ++ drop (r+1) regs) (pc + 1)
                                        Jump x y n -> run regs (if regs !! x == regs !! y
                                                                  then n else pc + 1)
                                        Incr x -> run (take x regs ++ [(regs !! x) + 1] ++ drop (x+1) regs) (pc + 1)
                                        Copy x y -> run (take x regs ++ [regs !! y] ++ drop (x+1) regs) (pc + 1)
                       in run (regs++[0..]) 0

