main :: IO ()
main = do 
    putStrLn "taro naam su che"
    name <- getLine
    putStrLn ("hello " ++ name)