{-# LANGUAGE FlexibleContexts #-}

module Text.Nonok.Parser where

import qualified Data.Map.Strict as M

import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)

import Control.Monad.Identity (Identity)
import Control.Monad (filterM)

import Text.Nonok.Types

generateAST :: String -> Either ParseError [Piece]
generateAST str = parse parseTemplate "" str

parseTemplate :: Parser [Piece]
parseTemplate = do
    maybeExtends <- optionMaybe $ try (spaces >> parseExtends)
    pieces <- parseBody eof
    return $ maybe pieces (\extends -> extends:pieces) maybeExtends


-- | Conusme parser but igonre result
void :: Parser a -> Parser ()
void p = p >> return ()

dot :: Parser Char
dot = char '.'

spaces1 :: Parser ()
spaces1 = void $ many1 space

-- | Parse the beginning of a tag
tagStart :: Parser String
tagStart = string "{{"

-- | Parse the end of a tag.
tagEnd :: Parser String
tagEnd = string "}}"

exprTag :: Parser String
exprTag = string "{-"

-- | Parse alpha-numeric characters and '_'
wordString :: Parser String
wordString = many1 $ oneOf "_" <|> alphaNum

-- | Parse alpha-numeric characters and '_./'
pathString :: Parser String
pathString = many1 $ oneOf "_./" <|> alphaNum

-- | Parse variable, which is dollar sign followed by wordString
parseVariable :: Char -> Parser String
parseVariable pref = do
    spaces >> char pref
    e <- wordString
    return e

parseLocalVariable :: Parser String
parseGlobalVariable :: Parser String
parseLocalVariable = parseVariable '$'
parseGlobalVariable = parseVariable '@'

parseReference :: Parser Reference
parseReference = (try parseLocalVariable >>= (return . RefLocal)) <|>
                 (try parseGlobalVariable >>= (return . RefGlobal))

parseRefExpression :: Parser Expression
parseRefExpression = parseReference >>= (return . LiteralExpression . LitRef)

-- | Parse content between `tagStart` and `tagEnd`
tag :: Parser a -> Parser a
tag p = between (tagStart >> spaces) (spaces >> tagEnd) p <?> "Tag"

parseFloat :: Parser Double
parseFloat = do sign <- option 1 (do s <- oneOf "+-"
                                     return $ if s == '-' then-1.0 else 1.0)
                x  <- P.float $ P.makeTokenParser emptyDef
                return $ sign * x

parseInt :: Parser Integer
parseInt = do sign <- option 1 (do s <- oneOf "+-"
                                   return $ if s == '-' then-1 else 1)
              x    <- P.integer $ P.makeTokenParser emptyDef
              return $ sign * x


-- | Parsers string literal between esc ex. ' or "
parseStringContents ::  Char -> Parser String
parseStringContents esc = between (char esc) (char esc) (many chars)
    where chars = (try escaped) <|> noneOf [esc]
          escaped = char '\\' >> choice (zipWith escapedChar codes replacements)
          codes        = ['b',  'n',  'f',  'r',  't',  '\\', '\"', '\'', '/']
          replacements = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '\'', '/']
          escapedChar code replacement = char code >> return replacement


parseMap :: Parser Expression
parseMap = do
    list <- between (char '{') (char '}') (sepBy parseMapPair $ spaces >> char ',' >> spaces)
    return $ MapExpression $ M.fromList list
    where
      parseMapPair = do
          spaces
          label <- ((try $ parseStringContents '\'') <|> (try $ parseStringContents '\"'))
          spaces >> char ':' >> spaces
          expr <- parseExpr
          spaces
          return (label, expr)

parseExpr :: Parser Expression
parseExpr =
    try parseMapMemberExpr <|>
    try parseFunc <|>
    try parseRefExpression <|>
    try parseNum <|>
    try parseBool <|>
    try parseString <|>
    try parseMap <|>
    try parseList
    where
        parseNum = parseInt >>= (return . LiteralExpression . LitNum)
        parseBool = do
           bool <- (try $ string "True" >> return True) <|> (try $ string "False" >> return False)
           return $ LiteralExpression $ LitBool bool
        parseString =
            ((try $ parseStringContents '\'') <|> parseStringContents '\"') >>=
                return . LiteralExpression . LitString
        parseList = do
            list <- between (char '[') (char ']') (sepBy parseExpr $ spaces >> char ',' >> spaces)
            return $ ListExpression list
        parseMapMemberExpr = do
            ref <- parseReference
            dot
            keys <- sepBy wordString dot
            return $ MapMemberExpression ref keys
        parseFunc = do
            funcName <- wordString
            args <- between (char '(') (char ')') (sepBy parseExpr $ spaces >> char ',' >> spaces)
            return $ FuncExpression funcName args

parseFor :: Parser Piece
parseFor = do
    var <- tagStart >> spaces >> string "for" >> spaces1 >> parseLocalVariable
    exp <- spaces1 >> string "in" >> spaces1 >> parseExpr
    spaces >> tagEnd
    blocks <- parseBody (try $ tag $ string "endfor")
    return $ ForPiece var exp blocks

parseIf :: Parser Piece
parseIf = do
    exp <- tagStart >> spaces >> string "if" >> spaces1 >> parseExpr
    spaces >> tagEnd
    block <- blockNotEmpty
    (exprs, blocks) <- parseNext
    return $ IfPiece (exp:exprs) (block:blocks)
    where
       endBlock = (try $ void $ lookAhead elifParser) <|>
                  (try $ void $ lookAhead elseParser) <|>
                  (try $ void $ lookAhead $ tag $ string "endif")
       elifParser = do
           exp <- tagStart >> spaces >> string "elif" >> spaces1 >> parseExpr
           spaces >> tagEnd
           return exp
       elseParser = do
           tag $ string "else"
           return $ LiteralExpression $ LitBool True
       blockNotEmpty = do
           block <- parseBody endBlock
           return $ case block of {[] -> [StaticPiece ""]; x -> id x}
       parseNext = do
           isEnd <- optionMaybe $ (try $ void $ lookAhead $ tag $ string "endif")
           isElse <- optionMaybe $ (try $ lookAhead elseParser)
           case (isEnd, isElse) of
               (Just _, Nothing) -> do
                   tag $ string "endif"
                   return ([], [])
               (Nothing, Just _) -> do -- else block
                   exp <- elseParser
                   block <- blockNotEmpty
                   tag $ string "endif"
                   return ([exp], [block])
               (Nothing, Nothing) -> do
                   exp <- elifParser
                   block <- blockNotEmpty
                   (exprs, blocks) <- parseNext
                   return $ (exp:exprs, block:blocks)
               _ -> unexpected "Unable to parse if. Detected both endif and else."


parseDecl :: Parser Piece
parseDecl = do
    tagStart >> spaces >> string "let" >> spaces1
    decls <- sepBy1 parseSingle (try (spaces >> char ',' >> spaces))
    spaces >> tagEnd
    return $ Decl decls
    where
        parseSingle = do
            var  <- parseLocalVariable
            expr <- spaces >> char '=' >> spaces >> parseExpr
            return (var, expr)

parseIncludeRef :: Parser Piece
parseIncludeRef = do
    tagStart >> spaces >> string "include" >> spaces1
    var <- parseReference
    newGlobals <- optionMaybe $ (try (spaces >> char ',' >> spaces >> parseMap)) <|>
                                (try (spaces >> char ',' >> spaces >> parseRefExpression))
    spaces >> tagEnd
    return $ IncludeRefPiece var newGlobals

parseIncludePath :: Parser Piece
parseIncludePath = do
    tagStart >> spaces >> string "include" >> spaces1
    path <- (try $ between (char '\'')  (char '\'') pathString) <|>
                  (between (char '\"')  (char '\"') pathString)
    newGlobals <- optionMaybe $ (try (spaces >> char ',' >> spaces >> parseMap)) <|>
                                (try (spaces >> char ',' >> spaces >> parseRefExpression))
    spaces >> tagEnd
    return $ IncludePathPiece path newGlobals

parseExtends :: Parser Piece
parseExtends = do
    path <- tagStart >> spaces >> string "extends" >> spaces1
    path <- (try $ between (char '\'')  (char '\'') pathString) <|>
                  (between (char '\"')  (char '\"') pathString)
    spaces >> tagEnd
    return $ ExtendsPiece path

parseBlock :: Parser Piece
parseBlock = do
    blockId <- tagStart >> spaces >> string "block" >> spaces1 >> wordString
    spaces >> tagEnd
    pieces <- parseBody (try $ tag $ string "endblock")
    return $ BlockPiece blockId pieces

parseComment :: Parser Piece
parseComment = do
    tag $ string "comment"
    void $ manyTill anyChar (try $ tag $ string "endcomment")
    return CommentPiece

parseRaw :: Parser Piece
parseRaw = do
    tag $ string "raw"
    text <- manyTill anyChar (try $ tag $ string "endraw")
    return $ RawPiece text

-- | Tag which evaluates and prints expression
parseCall :: Parser Piece
parseCall = do
    expr <- exprTag >> spaces >> parseExpr
    spaces >> tagEnd
    return $ CallPiece expr

static :: Parser Piece
static = do
  isTagAhead <- optionMaybe tryTag
  case isTagAhead of
      Just _ -> fail "Unable to parse tags." --if finds tag ahead, it means that tag parsers failed
      _ -> do
         c <- anyChar
         s <- manyTill anyChar (tryTag <|> eof)
         return $ StaticPiece $ c:s
  where
      tryTag = (try $ void $ lookAhead tagStart) <|>
               (try $ void $ lookAhead $ exprTag) <|>
               (try $ void $ lookAhead tagEnd)

nonStatic :: Parser Piece
nonStatic = try parseFor <|>
            try parseIf <|>
            try parseDecl <|>
            try parseIncludeRef <|>
            try parseIncludePath <|>
            try parseBlock <|>
            try parseCall <|>
            try parseRaw <|>
            try parseComment

parseBody :: Parser a -> Parser [Piece]
parseBody end = do
    manyTill (nonStatic <|> static) end
