import System.IO
import Text.Read (readMaybe)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "Welcome to Haskell Calculator!"
    calculatorLoop

calculatorLoop :: IO ()
calculatorLoop = do
    putStr "Enter expression (or 'q' to quit): "
    input <- getLine
    case input of
        "q" -> putStrLn "Goodbye!"
        _   -> case parseExpression input of
            Just result -> do
                putStrLn $ "Result: " ++ show result
                calculatorLoop
            Nothing -> do
                putStrLn "Invalid expression, please try again."
                calculatorLoop

parseExpression :: String -> Maybe Double
parseExpression input = case words input of
    [x, op, y] -> do
        x' <- readMaybe x
        y' <- readMaybe y
        applyOperator op x' y'
    _ -> Nothing

applyOperator :: String -> Double -> Double -> Maybe Double
applyOperator "+" x y = Just (x + y)
applyOperator "-" x y = Just (x - y)
applyOperator "*" x y = Just (x * y)
applyOperator "/" x y = if y /= 0 then Just (x / y) else Nothing
applyOperator _ _ _   = Nothing
