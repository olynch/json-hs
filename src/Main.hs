module Main where
import Text.ParserCombinators.Parsec
import Data.Char (digitToInt, chr)
import Text.Parsec.Pos (newPos)

data JValue = JNum Double
            | JString String
            | JBool Bool
            | JNull
            | JArr [JValue]
            | JObj [(String, JValue)]
            deriving (Eq, Show)

data JToken = Comma | BrktL | BrktR | BrcL | BrcR | Colon | StringT String | NumT Double | BoolT Bool | NullT
              deriving (Eq, Show)

symbol :: JToken -> String -> GenParser Char st JToken
symbol tok c = do
  string c
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
  char '"'
  s <- many (escapes <|> (noneOf ['\\', '\"']))
  char '"'
  return $ StringT s

numT :: GenParser Char st JToken
numT = do
  minus <- option "" (string "-")
  intp <- (string "0" <|> (do
                             d <- oneOf "123456789"
                             ds <- many digit
                             return (d:ds)))
  fracp <- option "" (do
                        char '.'
                        ds <- many1 digit
                        return $ "." ++ ds)
  exp <- option "" (do
                        oneOf "eE"
                        sgn <- option "" (string "+" <|> string "-")
                        ds <- many1 digit
                        return $ "e" ++ sgn ++ ds)
  return $ NumT (read $ minus ++ intp ++ fracp ++ exp)

jtoken :: GenParser Char st JToken
jtoken = do
  tok <- (numT <|> stringT <|> commaT <|> brktlT <|> brktrT <|> brclT <|> brcrT <|> colonT <|> nullT <|> trueT <|> falseT)
  spaces
  return tok

tokenParser :: GenParser Char st [JToken]
tokenParser = do
  spaces
  tokens <- many jtoken
  eof
  return tokens

jtok :: (JToken -> Maybe a) -> GenParser JToken st a
jtok f = token
  (\t -> show t)
  (\t -> newPos "<null>" 0 0)
  f

jsym :: JToken -> GenParser JToken st ()
jsym tok = jtok (\t -> if t == tok then Just () else Nothing)

jString = jtok (\t -> case t of
                   (StringT s) -> Just (JString s)
                   _ -> Nothing)

jNum = jtok (\t -> case t of
                (NumT n) -> Just (JNum n)
                _ -> Nothing)

jBool = jtok (\t -> case t of
                 (BoolT v) -> Just (JBool v)
                 _ -> Nothing)

jNull = jtok (\t -> case t of
                 NullT -> Just JNull
                 _ -> Nothing)

jArray :: GenParser JToken st JValue
jArray = do
  jsym BrktL
  vals <- jValue `sepBy` jsym Comma
  jsym BrktR
  return $ JArr vals

jObject :: GenParser JToken st JValue
jObject = do
  jsym BrcL
  let kvPair = (do
                   (JString s) <- jString
                   jsym Colon
                   v <- jValue
                   return (s, v))
  vals <- kvPair `sepBy` jsym Comma
  jsym BrcR
  return $ JObj vals

jValue :: GenParser JToken st JValue
jValue = jNum <|> jString <|> jBool <|> jNull <|> jArray <|> jObject

parseJValue :: String -> Either ParseError JValue
parseJValue s = (parse tokenParser "" s) >>= (parse jValue "")

printJValue :: JValue -> String
printJValue = undefined

main :: IO ()
main = do
  putStrLn $ show . parseJValue $ "{ \"Hello,\": \"\\u4598\\n\", \"foo\": [3, 6.9e4, 9] }"
