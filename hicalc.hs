import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Control.Monad (when)
import System.IO

type Environment = [(String, Double)]

doAssign :: String -> Double -> Environment -> Environment
doAssign var val env = (var, val) : filter ((/= var) . fst) env

data MyParserState =
    MyParserState {
        global :: Environment
        }
    deriving (Show)

type MyParser a = GenParser Char MyParserState a

lexer :: P.TokenParser MyParserState
lexer = P.makeTokenParser (haskellDef { reservedOpNames = ["*","/","+","-","**", "==", "/=", "<", "<=", ">", ">=", "&&", "||", "?", ":"] })

naturalOrFloat = P.naturalOrFloat lexer
parens         = P.parens lexer
reservedOp     = P.reservedOp lexer
identifier     = P.identifier lexer
lexeme         = P.lexeme lexer

expr = assignExpr

assignExpr :: MyParser Double
assignExpr = try(assign) <|> condExpr
    where
        assign = do
            var <- identifier
            lexeme $ char '='
            e <- expr

            updateState $ \st -> st{ global = doAssign var e (global st) }
            return e

condExpr :: MyParser Double
condExpr = try(cond) <|> expr'
    where
        cond = do
            c <- expr'
            lexeme $ char '?'
            t <- expr
            lexeme $ char ':'
            e <- expr
            return $ if isTrue c then t else e

expr' :: MyParser Double
expr' = buildExpressionParser table factor <?> "expression"
    where
        table = [
            [unary "-" negate, unary "+" id],
            [op "**" (**) AssocRight],
            [op "*" (*) AssocLeft, op "/" (/) AssocLeft],
            [op "+" (+) AssocLeft, op "-" (-) AssocLeft],
            [op "==" (cmp (==)) AssocNone, op "/=" (cmp (/=)) AssocNone, op "<" (cmp (<)) AssocNone, op "<=" (cmp (<=)) AssocNone, op ">" (cmp (>)) AssocLeft, op ">=" (cmp (>=)) AssocNone],
            [op "&&" (logiand) AssocLeft],
            [op "||" (logior) AssocLeft]
            ]
        op s f assoc = Infix (do{ reservedOp s; return f } <?> "operator") assoc
        unary s f = Prefix (do{ reservedOp s; return f })
        cmp op x y = if x `op` y then true else false
        logiand x y = if isFalse x then x else y
        logior  x y = if isTrue  x then x else y

false = 0.0
true = 1.0
isFalse = (== false)
isTrue = (/= false)

factor :: MyParser Double
factor = parenedExpr <|> floatLiteral <|> funcallOrVarref <?> "factor"

parenedExpr :: MyParser Double
parenedExpr = parens expr

floatLiteral :: MyParser Double
floatLiteral = do
    norf <- naturalOrFloat
    case norf of
        Left i    -> return $ fromInteger i
        Right f    -> return $ f

funcallOrVarref :: MyParser Double
funcallOrVarref = do
    name <- identifier
    do {
        params <- lexeme formalparams;
        case (applyFunc name params) of
            Right v        -> return v
            Left err    -> fail err
    } <|> do
        st <- getState;
        case lookup name (global st) of
            Nothing    -> fail $ "undefined variable: " ++ name
            Just v    -> return v

formalparams :: MyParser [Double]
formalparams = do
    lexeme $ char '('
    params <- expr `sepBy` lexeme (char ',')
    char ')'
    return params

applyFunc :: String -> [Double] -> Either String Double
applyFunc fname params = call $ lookup fname functbl
    where
        functbl = [
            ("sin", (1, apply1 sin)),
            ("cos", (1, apply1 cos)),
            ("tan", (1, apply1 tan)),
            ("log", (1, apply1 log)),
            ("sqrt", (1, apply1 sqrt))
            ]

        call Nothing = Left $ fname ++ ": no function"
        call (Just (argnum, fn))
            | length params /= argnum    = Left $ fname ++ ": illegal argnum, " ++ show (length params) ++ " for " ++ show argnum
            | otherwise                    = Right $ fn params

        apply1 f [x]   = f x
        apply2 f [x,y] = f x y

repl :: String -> (String -> Bool) -> (String -> st -> (String, st)) -> st -> IO st
repl prompt bQuit eval = loop
    where
        loop st = putStr prompt >> getLine >>= act st
        act st s
            | bQuit s = return st
            | otherwise = do
                let (res, st') = eval s st
                putStrLn res
                loop st'

calc :: MyParserState -> IO MyParserState
calc = repl "> " (== ":q") eval
    where
        eval line st = do
            case (runParser stmt st "" line) of
                Left err       -> (show err, st)
                Right (v, st') -> (show v, st')
        stmt = do
            e <- expr
            eof
            st <- getState
            return (e, st)

initialState = MyParserState genv
    where
        genv = [
            ("pi",    pi)
            ]

main = hSetBuffering stdout NoBuffering >> putStrLn "type ':q' to quit." >> calc initialState >>= print >> putStrLn "Bye"

