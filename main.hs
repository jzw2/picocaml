import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language
import Text.Parsec.Expr

data Const =
  BoolConst Bool
  | IntConst Int
  | FloatConst Double
  | StringConst String
  | NilConst
  | UnitConst 
  deriving Show

data MonOp =
  HdOp
  | TlOp
  | PrintOp
  | IntNegOp
  | FstOp
  | SndOp 
  deriving Show

data BinOp =
  IntPlusOp
  | IntMinusOp
  | IntTimesOp
  | IntDivOp
  | FloatPlusOp
  | FloatMinusOp
  | FloatTimesOp
  | FloatDivOp
  | ConsOp
  | ConcatOp
  | CommaOp
  | EqOp
  | GreaterOp
  | ModOp
  | ExpoOp 
  deriving Show
  

data Exp =
  VarExp String
  | ConstExp Const
  | MonOpAppExp MonOp Exp
  | BinOpAppExp BinOp Exp Exp
  | IfExp Exp Exp Exp
  | AppExp Exp Exp
  | FunExp String Exp
  | LetInExp String Exp Exp
  | LetRecInExp String String Exp Exp
  | RaiseExp Exp 
  deriving Show

data Dec =
  Anon Exp
  | Let String Exp
  | LetRec String String Exp 
  deriving Show

languageDef =
  emptyDef { 
           Token.identStart      = letter
          , Token.identLetter     = alphaNum
          , Token.reservedNames   = [ "if"
                                    , "then"
                                    , "else", "rec", "_", "let", "nil", "fst"
                                    , "snd", "hd", "st", "fun", "let", "in", "raise"
                                    ]
          , Token.reservedOpNames = ["**", "+", "-", "*", "/", "=", "->", "()"
                                    , "<", ">", "~", "+.", "-.", "*.", "/.", "^", "<=", ">=", "<>" , "::", "||", "&&", "+.", "-.", "*.", "/.",
                                      "," --mmore i guess
                                    ]
          }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
whiteSpace = Token.whiteSpace lexer -- parses whitespace

operators =
          [
            [
              Prefix (reservedOp "hd" >> return (MonOpAppExp HdOp)),
              Prefix (reservedOp "tl" >> return (MonOpAppExp TlOp)),
              Prefix (reservedOp "fst" >> return (MonOpAppExp FstOp)),
              Prefix (reservedOp "snd" >> return (MonOpAppExp SndOp))
            ], 
            [
              Infix (return AppExp) AssocLeft --fuction application
            ], 
            [
              Prefix (reserved "raise" >> return (RaiseExp))
            ],
            [
              Infix  (reservedOp "**"   >> return (BinOpAppExp ExpoOp)) AssocRight
            ],
            [
              Infix  (reservedOp "*"   >> return (BinOpAppExp IntTimesOp)) AssocLeft,
              Infix  (reservedOp "/"   >> return (BinOpAppExp IntDivOp  )) AssocLeft,
              Infix  (reservedOp "*."   >> return (BinOpAppExp FloatTimesOp)) AssocLeft,
              Infix  (reservedOp "/."   >> return (BinOpAppExp FloatDivOp  )) AssocLeft,
              Infix  (reserved "mod"   >> return (BinOpAppExp ModOp  )) AssocLeft
            ],
            [
              Infix  (reservedOp "+"   >> return (BinOpAppExp IntPlusOp     )) AssocLeft,
              Infix  (reservedOp "-"   >> return (BinOpAppExp IntMinusOp)) AssocLeft,
              Infix  (reservedOp "+."   >> return (BinOpAppExp FloatPlusOp     )) AssocLeft,
              Infix  (reservedOp "-."   >> return (BinOpAppExp FloatMinusOp)) AssocLeft,
              Infix (reservedOp "^" >> return (BinOpAppExp ConcatOp)) AssocLeft
            ], 
            [
              
              Infix  (reservedOp "::" >> return (BinOpAppExp ConsOp)) AssocRight
            ], 
            [
              Infix (reservedOp ">" >> return (BinOpAppExp GreaterOp)) AssocLeft,
              Infix (reservedOp ">=" >> return (desugarGe)) AssocLeft,
              Infix (reservedOp "<" >> return (flip (BinOpAppExp GreaterOp))) AssocLeft,
              Infix (reservedOp "=" >> return (BinOpAppExp EqOp)) AssocLeft
            ],
            [
              Infix (reservedOp "&&" >> return (desugarAnd)) AssocLeft
            ],
            [
              Infix (reservedOp "||" >> return (desugarOr)) AssocLeft
            ],
            [
              Infix (reservedOp "," >> return (BinOpAppExp CommaOp)) AssocLeft
            ]
          ]
ident :: Parser String
ident = identifier 



desugarAnd :: Exp -> Exp -> Exp
desugarAnd x y = IfExp x y $ ConstExp $ BoolConst False

desugarOr :: Exp -> Exp -> Exp 
desugarOr x y = IfExp x ( ConstExp $ BoolConst True) y

desugarLt :: Exp -> Exp -> Exp
desugarLt = flip $ BinOpAppExp GreaterOp

desugarLe :: Exp -> Exp -> Exp
desugarLe x y = desugarOr (desugarLt x y) (BinOpAppExp EqOp x y) 

  
desugarGe :: Exp -> Exp -> Exp
desugarGe x y = desugarOr (BinOpAppExp GreaterOp x y) (BinOpAppExp EqOp x y) 


desugarNe :: Exp -> Exp -> Exp
desugarNe x y = IfExp (BinOpAppExp EqOp x y) (ConstExp $ BoolConst False) (ConstExp $ BoolConst True)

  -- expresison with temriantor
exprWithTerm =
  do
    expression <- expr
    string ";;"
    return expression


letRec = do
  string "let"
  spaces
  string "rec"
  spaces
  func <- ident
  val <- ident
  spaces
  char '='
  spaces
  expression <- exprWithTerm
  return $ LetRec func val expression

letParse = do
  string "let"
  spaces
  param <- ident
  char '='
  spaces
  expression <- exprWithTerm
  return $ Let param expression

anon = exprWithTerm >>= (return . Anon)

mainLevel =
  try (letRec) <|> try (letParse) <|> try anon <?> "Failed top levele"

varExpr = do
  var <- identifier
  return $ VarExp var 

parenParse = do
  char '('
  spaces
  e <- expr
  spaces
  char ')'
  spaces
  return e

boolConst :: Parser Const
boolConst = do
  val <- string "true" <|> (string "false")
  spaces
  return $ BoolConst $ val == "true"

unitConst :: Parser Const
unitConst = do
  val <- reservedOp "()"
  spaces
  return UnitConst

intConst :: Parser Const
intConst = do
  num <- many1 digit
  spaces
  return $ IntConst $ read num
  

stringConst :: Parser Const
stringConst = do
  char '"'
  inside <- many (noneOf "\"")
  char '"'
  spaces
  return $ StringConst inside



nilConst = do
  char '['
  spaces
  char ']'
  return NilConst

floatConst :: Parser Const
floatConst = fmap FloatConst $ Token.float lexer
  

parseConst =
  boolConst
  <|> try floatConst
  <|> intConst
  <|> unitConst
  <|> stringConst
  <|> nilConst
  <?> " failed on parse const"

constExp = do
  con <- parseConst
  return $ ConstExp con

intPlusOp :: Parser BinOp
intPlusOp = do
  op <- char '+'
  return IntPlusOp

commaOp = do
  char ','
  return  CommaOp 

parseBinOp =
  try commaOp
  <|> try intPlusOp
  <?> "failed on parse binop"
  -- <|> commaOp

  

funExp = do
  reserved "fun"
  spaces
  i <- identifier
  spaces
  reservedOp "->"
  spaces
  e <- expr
  spaces
  return $ FunExp i e


appExp = do
  func <- parens funExp <|> varExpr
  spaces
  param <- expr
  return $ AppExp func param

expr = buildExpressionParser operators terms
  <?> "Failed on expr "

letInExp = do
  reserved "let"
  spaces
  i <- identifier
  spaces
  reservedOp "="
  spaces
  e1 <- expr
  spaces
  reserved "in"
  spaces
  e2 <- expr
  spaces
  return $ LetInExp i e1 e2

ifExp = do
  reserved "if"
  spaces
  cond <- expr
  spaces
  reserved "then"
  spaces
  trueExpr <- expr
  spaces
  reserved "else"
  spaces
  falseExpr <- expr
  spaces
  return $ IfExp cond trueExpr falseExpr
  

letRecInExp = do
  reserved "let"
  spaces
  reserved "rec"
  spaces
  func <- identifier
  spaces
  var <- identifier
  spaces
  reservedOp "="
  spaces
  e1 <- expr
  spaces
  reserved "in"
  spaces
  e2 <- expr
  spaces
  return $ LetRecInExp func var e1 e2
  
  

terms =
  try constExp
  <|> try varExpr
  <|> parens expr
  <|> try ifExp
  <|> try funExp
  <|> try letInExp
  <|> try letRecInExp
  <?> "failed terms"
  

  
parseAll :: String -> Either ParseError Dec
parseAll = parse mainLevel "(unknown)" 
