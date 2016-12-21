module Main where
import Control.Monad
import System.Environment
--import System.Cmd
import System.Process
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Error (Error, ErrorT, noMsg, strMsg, runErrorT)
import Data.IORef
import Text.ParserCombinators.Parsec hiding (spaces)
import System.IO hiding (try)
import qualified Graphics.UI.Threepenny as UI hiding ((<|>), many, apply)
import Graphics.UI.Threepenny.Core hiding ((<|>), many, apply)

--OK, Let's go.


{-
  The main function takes input from GUI and output into GUI.
-}

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = liftIO primitiveBindings >>= flip setup_env window

setup_env :: Env -> Window -> UI ()
setup_env env window = void $ do
    return window # set title "Grapheme"

    elHistoryList <- UI.ul
        # set style [("color","#333")]
    elInput       <- UI.input
        # set style [("width", "800px"), ("font-size", "18px")]
    elBtn         <- UI.button # set UI.text "Run"
    elGraph       <- UI.div
        # set style [("width", "850px"), ("overflow", "scroll"), ("border", "solid #ddd")]
        # set UI.html "Graph showed here."
    elOutput      <- UI.span
        # set style [("width", "800px")]
    elDot         <- UI.textarea
        # set UI.text "Dot format showed here."
        # set style [("width", "850px"), ("height", "300px")]
    elModBtn      <- UI.button
        # set UI.text "Repaint"
        # set style [("display", "none")]
    
    getBody window #+ [
            UI.h1 #+ [UI.string "Welcome to Grapheme!"]
            , row[
              column [
                  UI.h2 # set UI.text "Type Command and Run"
                  , grid [[UI.string "Input:", element elInput]
                       ,[element elBtn]
                       ,[UI.hr, UI.hr]
                       ,[UI.string "Output:", element elOutput]]
                  , UI.hr
                  , element elDot
                  , UI.hr
                  , element elModBtn
                  , element elGraph
              ]
              ,
              column [
                  UI.h2 # set UI.text "Command History"
                  , element elHistoryList
              ]
            ] # set style [("width", "95%")]
            ]

    let 
        interpretOuput :: String -> String -> UI ()
        interpretOuput ('(':'p':'a':'i':'n':'t':xs)
                       ('I':'n':'v':'a':'l':'i':'d':ys) = void $ do
            element elOutput # set UI.text ("Invalid" ++ ys)
            element elGraph # set UI.html "Graph showed here."
            element elModBtn # set style [("display", "none")]
            element elDot # set UI.value "Dot format showed here."
        interpretOuput ('(':'p':'a':'i':'n':'t':xs) ys = void $ do
            outh <- liftIO $ openFile "graph.dot" WriteMode
            liftIO $ hPutStrLn outh ys
            liftIO $ hClose outh
            liftIO $ rawSystem "dot" ["-O", "-Tsvg", "graph.dot"]
            gs <- liftIO $ readFile "graph.dot.svg"
            element elGraph # set UI.html gs
            element elModBtn # set style [("display", "inline")]
            element elDot # set UI.value ys
            element elOutput # set UI.text ""
        interpretOuput xs ys = void $ do
            element elOutput # set UI.text ys
            element elGraph # set UI.html "Graph showed here."
            element elModBtn # set style [("display", "none")]
            element elDot # set UI.value "Dot format showed here."

        calculateResult :: Env -> UI ()
        calculateResult env = void $ do
            xs <- get value elInput
            ys <- liftIO $ evalString env xs
            interpretOuput xs ys
            element elInput # set UI.value ""
            element elHistoryList #+ [UI.li # set text xs]
        
        calculateInput :: Env -> Int -> UI ()
        calculateInput env 13 = void $ do
            xs <- get value elInput
            ys <- liftIO $ evalString env xs
            interpretOuput xs ys
            element elInput # set UI.value ""
            element elHistoryList #+ [UI.li # set text xs]
        calculateInput env key = void $
            element elOutput # set text ""

        repaintGraph = void $ do
            ys <- get value elDot
            outh <- liftIO $ openFile "graph.dot" WriteMode
            liftIO $ hPutStrLn outh ys
            liftIO $ hClose outh
            liftIO $ rawSystem "dot" ["-O", "-Tsvg", "graph.dot"]
            gs <- liftIO $ readFile "graph.dot.svg"
            element elGraph # set html gs


    on UI.click elBtn     $ \_ -> calculateResult env
    on UI.keydown elInput $ calculateInput env
    on UI.click elModBtn  $ \_ -> repaintGraph

-- Symbols allowed in scheme

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~#"

atomsymbol :: Parser Char
atomsymbol = oneOf "[]"

-- A function calling our parser and handling possible errors. 
-- Parsec returns an Either data type, using the Left constructor to indicate an error and the Right one for a normal value

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val


readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)

-- Ignoring whitesapces

spaces :: Parser ()
spaces = skipMany space

{-
  Beyond any shadow of a doubt, we need to define a data type that can hold any scheme value.
    - Atom, simply stores a string representing it.
    - List, a haskell list containing its elements.
    - DottedList, stors the last element as another field
    - Number, a haskell number
    - String, a haskell string
    - Comment, a haskell string
    - Bool, a haskell bool 
    - IOFunc, file IO converting
    - Port, a haskell Handle 
    - PrimitiveFunc, taking a list of arguments to a Throws Error LispVal
    - Func, the names of the parameters, whether the function accepts a variable-lngth list of arguments, the function body, the environment
-}

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Comment String
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Port Handle
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Func {params :: [String], vararg :: (Maybe String), 
                      body :: [LispVal], closure :: Env}



{-
  A string is a double quote mark, followed by any number of non-quote characters, followed by a closing quote mark.
-}

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x

parseComment :: Parser LispVal
parseComment = do char ';'
                  x <- many (noneOf "\n\r")
                  eol <- try (Text.ParserCombinators.Parsec.string "\n\r")
                     <|> try (Text.ParserCombinators.Parsec.string "\r\n")
                     <|> Text.ParserCombinators.Parsec.string "\n"
                     <|> Text.ParserCombinators.Parsec.string "\r"
                  return $ Comment x

{-
  An atom is a letter or symbol, followed by any number of letters, digits, or symbols.
  
  The choice operator "<|>" is a Parsec combinator, trying parsers in order and returning the value as soon as succeeds.
-}

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol <|> atomsymbol
               rest <- many (letter <|> digit <|> symbol <|> atomsymbol)
               let atom = [first] ++ rest
               return $ case atom of 
                          "#t" -> Bool True
                          "#f" -> Bool False
                          otherwise -> Atom atom

{-
  Matching one or more digits, converting them into a number, wrapping it as a LispVal by a combining and "liftMed" function.
-}


parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

{-
  "sepBy parseExpr spaces" parses a series of expressions separated by whitespace and then apply the "monadiac" List consturctor.
-}

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

{-
  Almost the same, except for the dot.
-}

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

{-
  La-de-da!
-}

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

-- Combining them all together

{-
  OK... Lists an DottedLists are identical up to the dot.
  The try combinator attempts to run the specified parse, but if it fails, it backs up to the previous state, which can be used in a choice alternative without interfering with the other alternative.
-}

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseQuoted
        <|> parseNumber
        <|> parseComment
        <|> do char '('
               x <- (try parseList) <|> parseDottedList
               char ')'
               return x

{-
  Just print something out.

  Instead of showing the full function, we just prin out the word <primitive> for primitives and the header info for user-defined functions.
-}

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"
showVal (Comment contents) = contents
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) = 
  "(lambda (" ++ unwords (map show args) ++ 
     (case varargs of 
        Nothing -> ""
        Just arg -> " . " ++ arg) ++ ") ...)" 

{-
  It works lke the unwords function provided in Haskell Prelude, which glues together a list of words with spaces.
-}

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

-- Environment functions
{-
	@ is just syntactic sugar, giving the name for the whole variable.

	We use a few helper functions (makeFunc, makeNormalFunc, makeVarArgs) to make it easier to create function objects.
-}

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env val@(Comment _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = 
    do result <- eval env pred
       case result of
         Bool False -> eval env alt
         otherwise -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) =
    eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
    makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarargs varargs env [] body
eval env (List (Atom "load" : String filename : xs)) =
	load filename >>= liftM last . mapM (eval env) >> eval env (List (Atom "load" : xs)) 
eval env (List (Atom "load": xs)) = 
    return $ Atom $ "Done!"
eval env (List (function : args)) = do 
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

{-
  apply:
  it'll be passed a LispVal representing the actual function. For primitives, that makes the code simpler: we need only read the function out of the value and apply it.
  As for a user defined function ... 
  	1. Checking the length of the parameter list against the expected number of arguments.
  	2. Bind the arguments to a new environment using zip.
	3. Building up a new environment to evaluate the function in.
	4. Binding the remaining args to a singleton list
  	5. Evaluating the body in this new environment using the local function evalBody, whcih maps the monadic function eval env over every statement in the body.

-}

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (IOFunc func) args = func args
apply (Func params varargs body closure) args = 
    if num params /= num args && varargs == Nothing
       then throwError $ NumArgs (num params) args
       else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
    where remainingArgs = drop (length params) args
          num = toInteger . length
          evalBody env = liftM last $ mapM (eval env) body 
          bindVarArgs arg env = case arg of
              Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
              Nothing -> return env 

{-
  The list of primitives.
-}

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string>?", strBoolBinop (>)),
              ("string<?", strBoolBinop (<)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("atom>?", atomBoolBinop (>)),
              ("atom<?", atomBoolBinop (<)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("list?", isList),
              ("atom-append", atom_append),
              ("atom-add", atom_add),
              ("wrapp", wrapp),
              ("wraps", wraps),
              ("sflip", sflip),
              ("eqs?", eqs),
              ("acc?", acc),
              ("convert", convert),
              ("atomtolist", atomtolist),
              ("atom-split", atom_split),
              ("listtoatom", listtoatom),
              ("paint", paint)
              
              ]


{-
  "sthBoolBinops" differ from each other only in the type of argument they expect, so it's natural to factor the duplication into a generic "boolBinop" function that's parametrized by the unpacker function it applies to its arguments.
-}

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2 
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

-- Now we define three functions that specialize boolBinop with different unpackers.

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool
atomBoolBinop = boolBinop unpackAtom

{-
  Dozens of unpacking functions, works by pattern matching against the value and either returning it or throwing an error.
  Um... we do some silent transistion. 
-}

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in 
                          if null parsed 
                            then throwError $ TypeMismatch "number" $ String n
                            else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

unpackAtom :: LispVal -> ThrowsError String
unpackAtom (Atom s) = return s
unpackAtom (String s) = return s
unpackAtom (Bool s) = return $ show s
unpackAtom (Number s) = return $ show s
unpackAtom notAtom = throwError $ TypeMismatch "atom" notAtom

atomContent :: LispVal -> String
atomContent (Atom s) = s
atomContent (Number s) = show s

atom_con :: String -> String -> String
atom_con "#" [] = "#"
atom_con [] "#" = "#"
atom_con "#" y = y
atom_con x "#" = x
atom_con x y = x ++ y

atom_append :: [LispVal] -> ThrowsError LispVal
atom_append params = return $ Atom $ foldl (atom_con) [] $ map (\x -> atomContent $ x) params 

atom_add :: [LispVal] -> ThrowsError LispVal
atom_add params = return $ Atom $ foldl (++) [] $ map (\x -> atomContent $ x) params

wrapp :: [LispVal] -> ThrowsError LispVal
wrapp [Atom s] = return $ Atom $ "[" ++ s ++ "]"
wrapp [notAtom] = throwError $ TypeMismatch "atom" notAtom
wrapp badArgList = throwError $ NumArgs 1 badArgList

wraps :: [LispVal] -> ThrowsError LispVal
wraps [Atom s] = return $ Atom $ "[" ++ s ++ "]*"
wraps [notAtom] = throwError $ TypeMismatch "atom" notAtom
wraps badArgList = throwError $ NumArgs 1 badArgList

acc :: [LispVal] -> ThrowsError LispVal
acc [Atom ('#' : _)] = return $ Bool $ True
acc [Atom s] = return $ Bool $ False
acc [notAtom] = throwError $ TypeMismatch "atom" notAtom
acc badArgList = throwError $ NumArgs 1 badArgList

sflip :: [LispVal] -> ThrowsError LispVal
sflip [Atom ('#' : s)] = return $ Atom $ s
sflip [Atom s] = return $ Atom $ "#" ++ s
sflip [notAtom] = throwError $ TypeMismatch "atom" notAtom
sflip badArgList = throwError $ NumArgs 1 badArgList

eqs :: [LispVal] -> ThrowsError LispVal
eqs [(Atom ('#' : s1)), (Atom ('#' : s2))] = return $ Bool $ s1 == s2
eqs [(Atom ('#' : s1)), (Atom s2)] = return $ Bool $ s1 == s2
eqs [(Atom s1), (Atom ('#':s2))] = return $ Bool $ s1 == s2
eqs [(Atom s1), (Atom s2)] = return $ Bool $ s1 == s2
eqs [_, _] = return $ Bool False
eqs badArgList = throwError $ NumArgs 2 badArgList

atom_s :: String -> [LispVal]
atom_s (x : xs) = ([Atom $ [x] ] ++ (atom_s xs))
atom_s [] = []

atom_split :: [LispVal] -> ThrowsError LispVal
atom_split [Atom s] = return $ List $ atom_s s
atom_split [badArg] = throwError $ TypeMismatch "atom" badArg
atom_split bdaArgList = throwError $ NumArgs 1 bdaArgList

atom_l :: String -> String -> Int -> [LispVal]
atom_l ('[' : xs) stmp count =  atom_l xs (stmp ++ "[") (count + 1)
atom_l (']' : xs) stmp count =  atom_l xs (stmp ++ "]") (count + 1)
atom_l ('|' : xs) stmp 0 = [Atom $ stmp] ++ (atom_l xs [] 0)
atom_l (x : xs) stmp count = atom_l  xs (stmp ++ [x]) count  
atom_l [] stmp count = [Atom $ stmp]

atomtolist :: [LispVal] -> ThrowsError LispVal
--atomtolist [Atom "#"] = return $ List $ []
atomtolist [Atom s] = return $ List $ (atom_l s [] 0)
atomtolist [badArg] = throwError $ TypeMismatch "atom" badArg
atomtolist badArgList = throwError $ NumArgs 1 badArgList

lta :: [LispVal] -> String
lta [List ((Atom x) : xs)] =  x ++ "|" ++ (lta [List xs])
lta [List []] = []

listtoatom :: [LispVal] -> ThrowsError LispVal
listtoatom [List xs] = return $ Atom $ init $ lta $ [List xs]
listtoatom [badArg] = throwError $ TypeMismatch "pair" badArg
listtoatom badArgList = throwError $ NumArgs 1 badArgList 

status :: String -> String
status ('#' : x) = "\"" ++ x ++ "\"" ++ " [shape=doublecircle width=0.75]\n"
status x = "\"" ++ x ++ "\"" ++ " [shape=circle width=0.85]\n"

showArc :: LispVal -> String
showArc (List (a : b : c : d)) = "\"" ++ (show a) ++ "\"" ++ " -> " ++ "\"" ++ (show b) ++ "\"" ++ " [label=\"" ++ (show c) 

showStart :: LispVal -> String
showStart (List ((Atom ('#' : x)) : xs)) = show x
showStart (List (x : xs)) = "\"" ++ (show x) ++ "\""

convertV :: LispVal -> ThrowsError LispVal
--convertV [List (Atom ("#":x) : (List xs))] = "  " ++ x ++ " [shape=doublecircle]"  ++ "\n" ++ convertV $ List $ xs
convertV (List (x : xs)) = do
  v <- convertV (List xs)
  return $ Atom $ "  " ++ (status $ show $ x) ++ (atomContent v)
convertV (List []) = return $ Atom $ ""
convertV badArg = throwError $ TypeMismatch "Vertexes" badArg 

convertA :: LispVal -> ThrowsError LispVal
convertA (List (x : xs))  = do
  v <- convertA (List xs)
  --return $ Atom $ "  " ++ (show a) ++ " -> " ++ (show b) ++ " [label=\"" ++ (show c) ++ "\"]\n" ++ (atomContent v)
  return $ Atom $ "  " ++ (showArc x) ++ "\"]\n" ++ (atomContent v)
convertA (List []) = return $ Atom $ ""
convertA badArg = throwError $ TypeMismatch "Arcs" badArg

convert :: [LispVal] -> ThrowsError LispVal
convert [List (x : xs)] = do
   v <- convertV $ x
   a <- convertA $ (head xs)
   return $ Atom $ "digraph G {\n  rankdir=LR;\n  node [fixedsize=true]\n  \"\" [shape=none]\n" ++ (atomContent v) ++ "\n\n" ++ "  \"\" -> " ++ (showStart x) ++ "\n" ++ (atomContent a) ++ "}"  
convert [badArg] = throwError $ TypeMismatch "NFA" badArg

isList :: [LispVal] -> ThrowsError LispVal
isList [List _] = return $ Bool $ True 
isList _        = return $ Bool $ False

{-
  list primitives

  car:  (car '(a b c)) = a
        (car '(a)) = a
        (car '(a b . c)) = a
        eror 
             - not alist
             - more than one argument


-}

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

{-
  cdr:  (cdr '(a b c)) = (b c)
        (cdr '(a b)) = (b)
        (cdr '(a)) = ()
        (cdr '(a . b)) = b
        (cdr '(a b . c)) = (b . c)
        error
             - not a list
             - more than one argument
-}

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [DottedList [xs] x] = return x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

{-
  cons: (cons a '()) = (a)
        (cons a '(b c)) = (a b c)
        (cons a '(b. c)) = (a b . c)
        (cons a b) = (a . b)
        error
		- more or less than two arguments
-}

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ [x] ++ xs
cons [x, DottedList xs xlast] = return $ DottedList ([x] ++ xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

{-
  Um.. we recognize two items as the same if they print the same, fairly reasonable isn't it?
-}

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) && 
                                                    (and $ map eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                               Left err -> False
                               Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList



paint :: [LispVal] -> ThrowsError LispVal
paint [List xs] = convert [List xs]
paint [badArg] = throwError $ TypeMismatch "NFA" badArg
paint badArgList = throwError $ NumArgs 1 badArgList     


{-
  Boring part... Error checking

  Initially, we define a data type to represent errors along with a few more constructors to print them out.
-}
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected 
                                  ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

{-
  Er... Some steps making our error type into an instance of "Error", GHC's built-in error handling functions.
  Being an instance of error just means that it must provide functions to creat an instance either from a previous error message or by itself.
  To tell the truth, I almost know nothing about that... 
  La-de-da, la-de-da...
-}

instance Show LispError where show = showError

instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

{-
  There are several helper functions to simplify some of our IO tasks.
  flushStr: printing out a string and immediately flushes the stream.
  readPrompt: printing out a prompt and reading in a line of input.
  evalString: pulling the code to parse and evaluate a string.
  evalAndPrint: evaluating a string and printing the result.
-}

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =  evalString env expr >>= putStrLn

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

until_ pred prompt action = do 
  result <- prompt
  if pred result 
     then return ()
   --  else haha result action >> action result >>= putStrLn >> until_ pred prompt action  
     else action result >>= putStrLn >> until_ pred prompt action

runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr

{-
  Um... We use the combination "sequence . repeat . interact" to get an infinite loop.
-}
runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Scheme>>> ") . evalString

{-
  The IORef module lets you use stateful variables within the IO monad. 
  Since our state has to be interleaved with IO anyway (it persists between lines in the REPL, and we will eventually have IO functions within the language itself), we'll be using IORefs.

  By defining a type for our environments, we can observe that an Env is implemented as an IORef holding a list that maps Strings to mutable LispVals.
  We need IORefs for both the list itself and of individual values because it ight use "set!" to change the value of an individual variable, or it might use define to add a new variable, which should be visible on all subsequent statements.
-}

type Env = IORef [(String, IORef LispVal)]


--Creating an empty environment.

nullEnv :: IO Env
nullEnv = newIORef []

-- Storing primitives as regular values in variables, we have to bind them when the program starts up.


primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                                              ++ map (makeFunc PrimitiveFunc) primitives)
    where makeFunc constructor (var, func) = (var, constructor func)
{-
  For we have two monads to deal with there (IO monad and Error monad), we can't just catch all the exceptions and return only normal values to the IO monad.

  Using ErrorT, which lets us layer error-handling functionality on top of the IO monad is a good idead. 

  
-}

type IOThrowsError = ErrorT LispError IO


{-  
  So... We define "IOThrowsError".
  Being confronted with the potential situation of mixing, while they can't exist at the same do-block, we gotta write our own "lift". 
  The Either type again, either re-throws the error type or returns the ordinary value.
  Basically it's the same as lifting.
  In fact, the signature should be (MonadError m a) => Either e a -> m a?
-}

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

{-
  And... a helper function to run the whole top-level IOThrowsError is needed. 
  We can't escape from the IO monad anyway, considering all kinds of sided effects caused by IO.
  That's why returning an IO action.
-}

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

{-
  Functions for environment handling.
  isBound: determining if a given variable is already bound in the environment(envRef).
  		   The lookup returns a Maybe value, so we return False if that value wa Nothing and True otherwise.
  getVar: retriving the current value of a variable (using the IOThrowsError monad, because it also needs to do some error handling).
  		   we use liftIO . readIORef to generate an IOThrowsError action that reads the returned IORef.
  setVar: simply setting values (with the help of writeIORf action and flip).
  defineVar: setting a variable if already bound or creating a new one if not.
             In the latter case (where the variable is unbound), a new IORef is created to finish all the process we expected. Then we lift that whole do-block into the IOThrowsError monad with liftIO.  
  bindVars: being able to bind a whole bunch of variables at once, as happens whan a function is invoked.
  			addBinding takes a variable name and value, creates an IORef to hold the new variable, and then returns the name–value pair. 
  			extendEnv calls addBinding on each member of bindings to create a list of (String, IORef LispVal) pairs, and then appends the current environment to the end of that (++ env).
  writeIORef :: IORef a -> a -> IO ()
  readIORef :: IORef a -> IO a 
-}

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var  =  do env <- liftIO $ readIORef envRef
                         maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                               (liftIO . readIORef)
                               (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var) 
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do 
    alreadyDefined <- liftIO $ isBound envRef var 
    if alreadyDefined 
       then setVar envRef var value >> return value
       else liftIO $ do 
          valueRef <- newIORef value
          env <- readIORef envRef
          writeIORef envRef ((var, valueRef) : env)
          return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
          addBinding (var, value) = do ref <- newIORef value
                                       return (var, ref)

-- makeNormalFunc and makeVarArgs should just be considered specializations of maeFunc with the first argument set appropriately for normal functions and variable args.

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . showVal


-- a list of IO primitives

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

{- 
	makePort wraps the Haskell function open File.
	It is used in open-input-file and close-input-file
-}

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine stdin) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

{-
	reading and parsing a file, used in readAll and load (in eval)
-}

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

{-
	It just wraps that return value with the List constructor.
-}

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename

