import System.IO  
import System.Directory  
import Data.List 

readDataFrom fileHandle = 
    do 
        isFileEnd <- hIsEOF fileHandle
        if isFileEnd 
            then
                return ("")
            else
                do
                    info <- hGetLine  fileHandle
                    if take 1 info == "#"
                        then 
                    readDataFrom fileHandle


main = 
    do
        putStrLn "Enter file name (Including full path) to read"
        fileName <- getLine
        fileHandle <- openFile fileName ReadMode

        readDataFrom fileHandle
