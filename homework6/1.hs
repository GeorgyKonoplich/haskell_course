import Data.Array.IO
import System.Environment
import Data.List
import System.IO

main = do
    [str, file] <- getArgs
    content <- readFile file
    let matchedLines = filter (isInfixOf str) $ lines content
    mapM_ (\(x, y) -> putStrLn ((show x) ++ ". " ++ y)) $ zip [1..] matchedLines
    array <- newArray (1, length matchedLines) ""
    grep array

grep :: IOArray Int String -> IO ()
grep array = do 
    putStr "> Edit(E Number), Write to file(W OutputFileName), Quit(Q) ? "
    hFlush stdout
    input <- getLine
    let (command:argument) = (words input)
    case command of
        "E" -> do
            line <- getLine
            writeArray array (read $ head argument :: Int) (line)
            grep array
        "W" -> do
            lines <- getElems array
            appendFile (head argument) $ unlines (filter (not . null) lines)
            putStrLn $ "File `" ++ (head argument) ++ "` created, all changes saved | Changes appended to existing file"
            grep array
        "Q" -> return ()
        _ -> do
            grep array
