
import System.Environment

import Prelude hiding (error)
import System.Exit
import Parse
import Asm
import Pos
import Mem


main = do
    args <- getArgs
    case args of
        [] -> do
            putStrLn "usage: tmch-asm <file>"
            exitFailure
        file:_ -> do
            fdat <- readFile file
            ts <- checkMsg $ parse file fdat
            --putStr (show ts)
            --putStr (show (labels ts))
            result <- checkMsg $ assemble ts
            putStr (toHex result)
