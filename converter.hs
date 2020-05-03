import Data.Char
import System.IO  
import System.Directory  
import Data.List 
--main = do
--    text <- readFile "markdown.md"
--    let cases = lines text
--        writeFile "md_to_html.html"

--strTrim wakes white space off both ends of the string!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

--data Mk = 
--make sure all whitespace on the line has been delt with before this function is executed
headerOne :: String -> String
headerOne (x:xs)
    | take 7 (x:xs) == "#######" = x:xs
    | take 6 (x:xs) == "######" = "<h6>" ++ drop 5 xs ++ "</h6>"
    | take 5 (x:xs) == "#####" = "<h5>" ++ drop 4 xs ++ "</h5>"
    | take 4 (x:xs) == "####" = "<h4>" ++ drop 3 xs ++ "</h4>"
    | take 3 (x:xs) == "###" = "<h3>" ++ drop 2 xs ++ "</h3>"
    | take 2 (x:xs) == "##" = "<h2>" ++ drop 1 xs ++ "</h2>"
    | otherwise = "<h1>" ++ xs ++ "</h1>"


--function will be passes a string that will have the number of markers at the beginning and end of the line already checked
asterisk :: String -> String
asterisk (x:xs) 
    | take 2 (x:xs) == "**"  && drop (length xs - 2) xs ==  "**" = "<p><strong>" ++ drop 1 (init (init xs)) ++ "</p></strong>"
    | take 1 (x:xs) == "*" && drop (length xs - 1) xs == "*" = "<p><em>" ++ init xs ++ "</p></em>"
    | otherwise = x:xs

underscore :: String -> String
underscore (x:xs)
    | take 2 (x:xs) == "__" && drop (length xs - 2) xs == "__" = "<p><strong>" ++ drop 1 (init (init xs)) ++ "</p></strong>"
    | take 1 (x:xs) == "_" && drop (length xs - 1) xs == "_" = "<p><em>" ++ init xs ++ "</p></em>"
    | otherwise = x:xs

strike :: String -> String
strike (x:xs) 
    | drop (length xs - 2) xs == "~~" =  "<p><s>" ++ drop 1 (init (init xs)) ++ "</p></s>"
    | otherwise = x:xs

link :: String -> String
link (x:xs) 
    | drop (length xs - 1) xs == ">" = "<p><a href='" ++ init xs ++ "></a></p>"
    | otherwise = x:xs

image :: String -> String
image (x:xs) = "<p><img src = \"" ++ drop 1 (getImgLink (x:xs)) ++ "\" " ++ "alt= \"" ++ imgAlt (x:xs) ++ "\">"


getImgLink :: String -> String
getImgLink (x:xs) = drop (head (elemIndices '(' (x:xs))) (init (x:xs))
--use take to get the second bookmark of image alt feild

imgAlt :: String -> String
imgAlt (x:xs) = drop 2 (take (head (elemIndices ']' (x:xs))) (x:xs))

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
                        then putStrLn (headerOne info) 
                        else return()
                    if take 1 info == "*" 
                        then putStrLn(asterisk info)
                        else return()
                    if take 1 info == "_"
                        then putStrLn(underscore info)
                        else return()
                    if take 2 info == "~~"
                        then putStrLn (strike info)
                        else return()
                    if take 1 info == "<"
                        then putStrLn (link info)
                        else return ()
                    if take 2 info == "!["
                        then putStrLn (image info)
                        else return()
                    readDataFrom fileHandle


main = 
    do
        putStrLn "Enter file name (Including full path) to read"
        fileName <- getLine
        fileHandle <- openFile fileName ReadMode

        readDataFrom fileHandle
