{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative        ((<|>))
import           Control.Monad              (join)
import           Data.Aeson
import           Data.Bifunctor             (first)
import           Data.ByteString.Lazy       (fromStrict)
import           Data.ByteString.Lazy.Char8 (pack, unpack)
import           GHC.Generics
import           Network.HTTP.Types         (status200, status404)
import           Network.Wai
import           Network.Wai.Handler.Warp   (run)
import           Text.Parsec                hiding ((<|>))
import           Text.Parsec.String

data Calculation = Calculation { expressions :: [String]} deriving (Generic,Show)
instance FromJSON Calculation

data CalculationResult = CalculationError String | CalculationSuccess Int
  deriving (Eq, Show, Generic)

instance ToJSON CalculationResult where
  toJSON (CalculationError err)      = object ["error" .= err]
  toJSON (CalculationSuccess result) = object ["result" .= result]

instance FromJSON CalculationResult where
  parseJSON = withObject "CalculationResult" $ \v ->
    CalculationSuccess <$> v .: "result" Control.Applicative.<|> CalculationError <$> v .: "error"


data Expr = Plus Expr Expr
          | Minus Expr Expr
          | Times Expr Expr
          | Div Expr Expr
          | Const Int
          deriving Show


----------------------------
-- parser
----------------------------

parseExpr :: Parser Expr
parseExpr = spaces *> parseSumDifference <* spaces

parseSumDifference :: Parser Expr
parseSumDifference = parseMulDiv `chainl1` (plus <|> minus)
  where
    plus  = Plus  <$ spacesAround (char '+')
    minus = Minus <$ spacesAround (char '-')

parseMulDiv :: Parser Expr
parseMulDiv = parseFactor `chainl1` (times <|> divide)
  where
    times  = Times <$ spacesAround (char '*')
    divide = Div   <$ spacesAround (char '/')

parseFactor :: Parser Expr
parseFactor = between (char '(' <* spaces) (char ')' <* spaces) parseExpr <|> parseConst

parseConst :: Parser Expr
parseConst = Const . read <$> many1 digit <* spaces

spacesAround :: Parser a -> Parser a
spacesAround p = spaces *> p <* spaces

----------------------------
-- calculations
----------------------------

evalOp2 :: (Int -> Int -> Int) -> Expr -> Expr -> Either String Int
evalOp2 op x y = do
  x' <- eval x
  y' <- eval y
  return $ x' `op` y'

eval :: Expr -> Either String Int
eval (Const x)         = return x
eval (Plus x y)        = evalOp2 (+) x y
eval (Minus x y)       = evalOp2 (-) x y
eval (Times x y)       = evalOp2 (*) x y
eval (Div _ (Const 0)) = Left "Cannot Divide by 0"
eval (Div x y)         = evalOp2 div x y

----------------------------
-- webservice
----------------------------

main :: IO ()
main = do
    putStrLn "Starting server on port 3000"
    run 3000 app
    return ()

app :: Application
app request respond =
    case rawPathInfo request of
        "/calculate" -> calculationHandler request respond
        _            -> notFoundHandler request respond

calculationHandler :: Application
calculationHandler request respond = do
    body <- requestBody request
    let maybeCalculation = decode $ fromStrict body :: Maybe Calculation
    case maybeCalculation of
        Just calculation -> do
            let results = performCalculation calculation
            respond $ responseLBS status200 [("Content-Type", "application/json")] (encode results)
        Nothing -> respond $ responseLBS status404 [("Content-Type", "text/plain")] "Could not parse request body"


performCalculation :: Calculation -> [CalculationResult]
performCalculation calculation =
  map (first show . parse parseExpr "") (expressions calculation) >>=
    either (pure . CalculationError) (either (pure . CalculationError) (pure . CalculationSuccess) . eval)


notFoundHandler :: Application
notFoundHandler _ respond = respond $ responseLBS status404 [("Content-Type", "text/plain")] "404 - Not found"
