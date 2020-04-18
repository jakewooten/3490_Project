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
    | take 6 (x:xs) == "######" = "<h6>" ++  xs ++ "<h6>"
    | take 5 (x:xs) == "#####" = "<h5>" ++ xs ++ "<h5>"
    | take 4 (x:xs) == "####" = "<h4>" ++ xs ++ "<h4>"
    | take 3 (x:xs) == "###" = "<h3>" ++ xs ++ "<h3>"
    | take 2 (x:xs) == "##" = "<h2>" ++ xs ++ "<h2>"
    | otherwise = "<h1>" ++ xs ++ "<h1>"

--this will be passed the indicating symbol (- or =) to determine what kind of header it will be
headerTwo :: Char -> String -> String
headerTwo x y 
    | x == '=' = "<h1>" ++ y ++ "<h1>"
    | x == '-' = "<h2>" ++ y ++ "<h2>"
