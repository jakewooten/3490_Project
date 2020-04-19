import Data.Char
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

--this will be passed the indicating symbol (- or =) to determine what kind of header it will be
headerTwo :: Char -> String -> String
headerTwo x y 
    | x == '=' = "<h1>" ++ y ++ "</h1>"
    | x == '-' = "<h2>" ++ y ++ "</h2>"

--function will be passes a string that will have the number of markers at the beginning and end of the line already checked
bold :: String -> String
bold (x:xs) = "<p><strong>" ++ drop 1 (init (init xs)) ++ "</p></strong>"

italics :: String -> String
italics (x:xs) = "<p><em>" ++ drop 1 (init xs) ++ "</p></em>"

strike :: String -> String
strike (x:xs) = "<p><s>" ++ drop 1 (init (init xs)) ++ "</p></s>"


