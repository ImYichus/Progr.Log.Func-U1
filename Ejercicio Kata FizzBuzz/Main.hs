import System.IO (hFlush, stdout)  

-- Es una lista de numeros del 0 al 9
-- Asi se declaran los arreglos en Haskell
units :: [String]
units = ["Zero","One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine"]

-- Es una lista de numeros del 10 al 19
teens :: [String]
teens = ["Ten", "Eleven", "Twelve", "Thirteen", "Fourteen", "Fifteen",
         "Sixteen", "Seventeen", "Eighteen", "Nineteen"]

-- Es una lista de numeros de los multiplos de 10
tens :: [String]
tens = ["", "", "Twenty", "Thirty", "Forty", "Fifty",
        "Sixty", "Seventy", "Eighty", "Ninety"]

-- Función para convertir los números que no son multiplos de 3 o de 5, a texto en inglés
duolingo :: Int -> String
duolingo n
    | n < 10    = units !! n
    | n < 20    = teens !! (n - 10)
    | n < 100   =
        let (d, u) = n `divMod` 10
        in if u == 0 then tens !! d else tens !! d ++ " " ++ units !! u
    | otherwise = show n

-- Funcion principal que pregunta si el numero es multiplo de 3 o de 5, si no llama a la funcion duolingo
fizz :: Int -> String
fizz n | n `mod` 5 == 0 && n `mod` 3 == 0 = "FizzBuzz"
       | n `mod` 5  == 0  = "Buzz"
       -- La linea de abajo es como un IF pero en haskell
       | n `mod` 3  == 0  = "Fizz"
       -- la linea de abajo es como un break pero en haskell
       | otherwise        = duolingo n 

-- Contenedor principal
main :: IO ()
main = do
    -- Imprimir un Mensaje
    putStr "Ingresa un número: "
    hFlush stdout  
    -- Es un escanner
    input <- getLine
    let numero = read input :: Int
    putStrLn $ fizz numero
