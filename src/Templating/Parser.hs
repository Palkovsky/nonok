{-# LANGUAGE FlexibleContexts #-}

module Templating.Parser where

import qualified Data.Text as T
import qualified Data.Map.Strict as M

import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)

import Control.Monad.Identity (Identity)
import Control.Monad (filterM)

import Templating.Types


(<^>) :: Parser a -> String -> Either ParseError a
p <^> t = parse p "" t

-- | Strips every line
purify :: String -> String
purify input = foldr (++) [] $ map (T.unpack . T.strip . T.pack) $ lines input

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
parseVariable :: Parser String
parseVariable = do
    spaces >> char '$'
    e <- wordString
    return e

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

parseExpr :: Parser Expression
parseExpr = try parseMapMemberExpr <|> try parseRefExpr <|> try parseDouble <|> try parseInteger <|>
            try parseBool <|> try parseString <|> try parseMap <|> try parseList
    where
        parseRefExpr = do
            var <- parseVariable
            return $ ReferenceExpression var
        parseInteger = parseInt >>= (return . LiteralExpression . LitInteger)
        parseDouble = parseFloat >>= (return . LiteralExpression . LitDouble)
        parseBool = do
           bool <- (try $ string "True" >> return True) <|> (try $ string "False" >> return False)
           return $ LiteralExpression $ LitBool bool
        parseString =
            ((try $ parseStringContents '\'') <|> parseStringContents '\"') >>=
                return . LiteralExpression . LitString
        parseList = do
            list <- between (char '[') (char ']') (sepBy parseExpr $ spaces >> char ',' >> spaces)
            return $ ListExpression list
        parseMapPair = do
            label <- ((try $ parseStringContents '\'') <|> (try $ parseStringContents '\"'))
            spaces >> char ':' >> spaces
            expr <- parseExpr
            case expr of {LiteralExpression lit -> return (label, lit); _ -> unexpected "Not literal."}
        parseMap = do
            list <- between (char '{') (char '}') (sepBy parseMapPair $ spaces >> char ',' >> spaces)
            return $ LiteralExpression $ LitMap $ M.fromList list
        parseMapMemberExpr = do
            var <- parseVariable
            dot
            keys <- sepBy wordString dot
            return $ MapMemberExpression var keys

parseFor :: Parser Piece
parseFor = do
    var <- tagStart >> spaces >> string "for" >> spaces1 >> parseVariable
    exp <- spaces1 >> string "in" >> spaces1 >> parseExpr
    spaces >> tagEnd
    blocks <- parseBlock (try $ tag $ string "endfor")
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
           block <- parseBlock endBlock
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
    decls <- sepBy1 parseSingle (spaces >> char ',' >> spaces)
    spaces >> tagEnd
    return $ Decl decls
    where
        parseSingle = do
            var  <- parseVariable
            expr <- spaces >> char '=' >> spaces >> parseExpr
            return (var, expr)

parseIncludeRef :: Parser Piece
parseIncludeRef = do
    tagStart >> spaces >> string "include" >> spaces1
    var <- parseVariable
    spaces >> tagEnd
    return $ IncludeRefPiece var

parseIncludePath :: Parser Piece
parseIncludePath = do
    tagStart >> spaces >> string "include" >> spaces1
    path <- (try $ between (char '\'')  (char '\'') pathString) <|>
                  (between (char '\"')  (char '\"') pathString)
    spaces >> tagEnd
    return $ IncludePathPiece path

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
         let str = {-(T.unpack . T.strip . T.pack) $-} c:s
         return $ StaticPiece str
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
            try parseCall <|>
            try parseRaw <|>
            try parseComment

parseBlock :: Parser a -> Parser [Piece]
parseBlock end = do
    pieces <- manyTill (nonStatic <|> static) end
    filterM (\x -> return $ x /= (StaticPiece "")) pieces

parseAll :: Parser [Piece]
parseAll = parseBlock eof
