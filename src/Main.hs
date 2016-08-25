module Main where
import Text.ParserCombinators.Parsec
import Data.Char (digitToInt, chr)

data JValue = JNum Double
            | JString String
            | JBool Bool
            | JNull
            | JArr [JValue]
            | JObj [(String, JValue)]

data JToken = Comma | BrktL | BrktR | BrcL | BrcR | Colon | StringT String | NumT Double | BoolT Bool | NullT
              deriving (Eq, Show)

symbol :: JToken -> String -> GenParser Char st JToken
symbol tok c = do
  _ <- string c
  return tok

commaT = symbol Comma ","
brktlT = symbol BrktL "["
brktrT = symbol BrktR "]"
brclT = symbol BrcL "{"
brcrT = symbol BrcR "}"
colonT = symbol Colon ":"
nullT = symbol NullT "null"
trueT = symbol (BoolT True) "true"
falseT = symbol (BoolT False) "false"

utfCode :: GenParser Char st Char
utfCode = do
  digits <- count 4 hexDigit
  let n = foldl (\x d -> 16*x + toInteger (digitToInt d)) 0 digits
  return (chr (fromInteger n))

escapes :: GenParser Char st Char
escapes = do
  _ <- char '\\'
  c <- oneOf ['\\', '\"', 'n', 'b', 'f', 'r', 't', 'u']
  case c of
    '\\' -> return '\\'
    '\"' -> return '\"'
    'n' -> return '\n'
    'b' -> return '\b'
    'f' -> return '\f'
    'r' -> return '\r'
    't' -> return '\t'
    'u' -> utfCode

stringT :: GenParser Char st JToken
stringT = do
  _ <- char '"'
  s <- many (escapes <|> (noneOf ['\\', '\"']))
  _ <- char '"'
  return $ StringT s

numT :: GenParser Char st JToken
numT = do
  minus <- option "" (string "-")
  intp <- (string "0" <|> (do
                             d <- oneOf "123456789"
                             ds <- many digit
                             return (d:ds)))
  fracp <- option "" (do
                        _ <- char '.'
                        ds <- many1 digit
                        return $ "." ++ ds)
  exp <- option "" (do
                        _ <- oneOf "eE"
                        sgn <- option "" (string "+" <|> string "-")
                        ds <- many1 digit
                        return $ "e" ++ sgn ++ ds)
  return $ NumT (read $ minus ++ intp ++ fracp ++ exp)

jtoken :: GenParser Char st JToken
jtoken = do
  tok <- (numT <|> stringT <|> commaT <|> brktlT <|> brktrT <|> brclT <|> brcrT <|> colonT <|> nullT <|> trueT <|> falseT)
  _ <- spaces
  return tok

tokenize :: GenParser Char st [JToken]
tokenize = do
  _ <- spaces
  tokens <- many jtoken
  eof
  return tokens

parseJValue :: String -> JValue
parseJValue = undefined

printJValue :: JValue -> String
printJValue = undefined

main :: IO ()
main = do
  putStrLn $ show . (parse tokenize "") $ "{ \"Hello,\": \"\\u4598\\n\", \"foo\": [3, 6.9e4, 9] }"
