import Data.Char (isSpace)
import System.IO  
import System.Directory  
import Data.List
import Data.Char 
--main = do
--    text <- readFile "mark.md"
--    let cases = lines text
--        writeFile "md_to_html.html"

--strTrim wakes white space off both ends of the string!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

--data Mk = 
--make sure all whitespace on the line has been delt with before this function is executed
headerOne :: String -> String
headerOne (x:xs)
    | take 7 (x:xs) == "#######" = x:xs
    | take 6 (x:xs) == "######" = "<h6>" ++ drop 5 xs ++ "</h6>\n"
    | take 5 (x:xs) == "#####" = "<h5>" ++ drop 4 xs ++ "</h5>\n"
    | take 4 (x:xs) == "####" = "<h4>" ++ drop 3 xs ++ "</h4>\n"
    | take 3 (x:xs) == "###" = "<h3>" ++ drop 2 xs ++ "</h3>\n"
    | take 2 (x:xs) == "##" = "<h2>" ++ drop 1 xs ++ "</h2>\n"
    | otherwise = "<h1>" ++ xs ++ "</h1>\n"


unorderedList :: String -> String
unorderedList (x:xs)
    | (x:xs) !! 0 == '*' || (x:xs) !! 0 == '-' || (x:xs) !! 0 == '+' = "<ul><li>" ++ drop 1 xs ++ "</li></ul>\n"
    | (x:xs) !! 2 == '*' || (x:xs) !! 2 == '-' || (x:xs) !! 2 == '+' = "<ul><ul><li>" ++ drop 3 xs ++ "</li></ul></ul>\n" 
    | (x:xs) !! 4 == '*' || (x:xs) !! 4 == '-' || (x:xs) !! 4 == '+' = "<ul><ul><ul><li>" ++ drop 5 xs ++ "</li></ul></ul></ul>\n"
    | (x:xs) !! 6 == '*' || (x:xs) !! 6 == '-' || (x:xs) !! 6 == '+' = "<ul><ul><ul><ul><li>" ++ drop 7 xs ++ "</li></ul></ul></ul></ul>\n"
    | (x:xs) !! 8 == '*' || (x:xs) !! 8 == '-' || (x:xs) !! 8 == '+' = "<ul><ul><ul><ul><ul><li>" ++ drop 9 xs ++ "</li></ul></ul></ul></ul></ul>\n"
    | otherwise = x:xs


checkList :: String -> Bool
checkList (x:xs)
    | take 2 (trim (x:xs)) == "+ " = True
    | take 2 (trim (x:xs)) == "* " = True
    | take 2 (trim (x:xs)) == "- " = True
    | otherwise = False

checkNumList :: String -> Bool
checkNumList (x:xs)
    | x == '1' = True
    | x == '2' = True
    | x == '3' = True
    | x == '4' = True
    | x == '5' = True
    | x == '6' = True
    | x == '7' = True
    | x == '8' = True
    | x == '9' = True
    | otherwise = False

numList :: String -> String
numList (x:xs)
    | x == '1' = "</ol><ol><li>" ++ drop 1 xs ++ "</li>\n"
    | isDigit ((x:xs) !! 0) == True  = "<li>" ++ drop ( head (elemIndices '.' (x:xs))) (xs) ++ "</li>\n"
    | isDigit ((x:xs) !! 1) == True  = "<li>" ++ drop ( head (elemIndices '.' (x:xs))) (xs) ++ "</li>\n"
    | otherwise = x:xs

asterisk :: String -> String
asterisk (x:xs) 
    | take 2 (x:xs) == "**"  && drop (length xs - 2) xs ==  "**" = "<p><strong>" ++ drop 1 (init (init xs)) ++ "</p></strong>\n"
    | take 1 (x:xs) == "*" && drop (length xs - 1) xs == "*" = "<p><em>" ++ init xs ++ "</p></em>\n"
    | otherwise = x:xs

underscore :: String -> String
underscore (x:xs)
    | take 2 (x:xs) == "__" && drop (length xs - 2) xs == "__" = "<p><strong>" ++ drop 1 (init (init xs)) ++ "</p></strong>\n"
    | take 1 (x:xs) == "_" && drop (length xs - 1) xs == "_" = "<p><em>" ++ init xs ++ "</p></em>\n"
    | otherwise = x:xs

strike :: String -> String
strike (x:xs) 
    | drop (length xs - 2) xs == "~~" =  "<p><s>" ++ drop 1 (init (init xs)) ++ "</p></s>\n"
    | otherwise = x:xs

link :: String -> String
link (x:xs) 
    | drop (length xs - 1) xs == ">" = "<p><a href='" ++ init xs ++ "'>" ++ init xs ++ "</a></p>\n"
    | otherwise = x:xs

image :: String -> String
image (x:xs) = "<p><img src = \"" ++ drop 1 (getImgLink xs) ++ "\" " ++ "alt= \"" ++ imgAlt (x:xs) ++ "\">\n"


getImgLink :: String -> String
getImgLink (x:xs) = drop (head (elemIndices '(' (x:xs))) (init (x:xs))
--use take to get the second bookmark of image alt feild

imgAlt :: String -> String
imgAlt (x:xs) = drop 2 (take (head (elemIndices ']' (x:xs))) (x:xs))

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace



readDataFrom fileHandle = 
    do 
        isFileEnd <- hIsEOF fileHandle
        if isFileEnd 
            then
                return ("")
            else
                do
                    info <- hGetLine fileHandle
                    if take 1 info == "#"
                        then appendFile "markdown.html" (headerOne info) 
                        else return()
                    if tail info == "*" 
                        then appendFile "markdown.html" (asterisk info)
                        else return()
                    if take 1 info == "_"
                        then appendFile "markdown.html" (underscore info)
                        else return()
                    if take 2 info == "~~"
                        then appendFile "markdown.html" (strike info)
                        else return()
                    if take 1 info == "<"
                        then appendFile "markdown.html" (link info)
                        else return ()
                    if take 2 info == "!["
                        then appendFile "markdown.html" (image info)
                        else return()
                    if checkList (trim info) == True
                        then appendFile "markdown.html" (unorderedList info)
                        else return()
                    if checkNumList info == True
                        then appendFile "num.html" (numList info)
                        else return()
                    readDataFrom fileHandle 

main = 
    do
        writeFile "markdown.html" ""
        
        putStrLn "Enter file name (Including full path) to read"
        fileName <- getLine

        fileHandle <- openFile fileName ReadMode
        readDataFrom fileHandle
        
        appendFile "num.html" "</ol>\n"
        file2 <- readFile "num.html"
        appendFile "markdown.html" file2
        removeFile "num.html"
