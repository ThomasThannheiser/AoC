import MPCAS (Parser, runParser, sepBy, symbol, upper, lower, token, space)
import Control.Applicative (Alternative ((<|>), empty, many, some))

type Env a b = [(a,b)]

assoc :: Eq s => s -> Env s d -> d
assoc x ((u, v) : ws) 
      | x == u    = v
      | otherwise = assoc x ws

data Symbol = Term String
            | Nont String
            deriving (Eq, Show)

type Alt = [Symbol]
type Rhs = [Alt]

type Gram = Env Symbol Rhs

bnf :: Parser Maybe String -> Parser Maybe String -> Parser Maybe Gram
bnf nontp termp = some rule
  where rule = (,) <$> nont <*> (symbol "::=" *> rhs <* symbol ".")
        rhs = sepBy alt (symbol "|")
        alt = some (nont <|> term)
        nont = Nont <$> nontp
        term = Term <$> termp

data Tree = Node Symbol [Tree]
          deriving Show

parsGram :: Gram -> Symbol -> Parser Maybe Tree
parsGram gram = parsSym
  where parsSym :: Symbol -> Parser Maybe Tree
        parsSym s@(Term t) = Node s [] <$ symbol t
        parsSym s@(Nont n) = Node s <$> parsRhs (assoc s gram)
        parsAlt :: Alt -> Parser Maybe [Tree]
        parsAlt = mapM parsSym
        parsRhs :: Rhs -> Parser Maybe [Tree]
        parsRhs = foldr ((<|>) . parsAlt) empty

type SymbolSet = Parser Maybe String

type CFG = (SymbolSet, SymbolSet, String, Symbol)

parsegen :: CFG -> Parser Maybe Tree
parsegen (nontp, termp, bnfstring, start) = 
  parsGram (maybe [] fst $ runParser (bnf nontp termp) bnfstring) start   

main = do
    let block = "BLOCK ::= begin BLOCK end BLOCK | empty."
        cfg = (nont, term, block, Nont "BLOCK")  
        nont = token (some upper) 
        term = token (some lower)
        input = "begin empty end begin begin empty end empty end empty"
    print . runParser (parsegen cfg) $ input
