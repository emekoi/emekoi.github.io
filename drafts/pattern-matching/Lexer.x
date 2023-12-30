{
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lexer
  ( Alex
  , AlexPosn (..)
  , alexGetInput
  , alexError
  , runAlex
  , alexMonadScan
  , Range (..)
  , Token (..)
  , TokenClass (..)
  , scanMany
  , unToken
  , unToken'
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
}

%wrapper "monadUserState-bytestring"

$digit = [0-9]
$alpha = [a-zA-Z]

@identifier  = ($alpha | $digit | \_ | \')
@constructor = ([A-Z]) @identifier* (\?)?
@variable    = ([a-z] @identifier* | \_ @identifier+) (\?)?

tokens :-

-- whitespace and comments
<0> $white+ ;
<0> "--" .* ;

-- variables and constructors
<0> @variable     { tokByteString Variable }
<0> @constructor  { tokByteString Constructor }

-- keywords
<0> "match" { tok Match }
<0> "when"  { tok When }

-- operators and symbols
<0> "{"  { tok LBracket }
<0> "}"  { tok RBracket }

<0> "("  { tok LParen }
<0> ")"  { tok RParen }

<0> "->" { tok RArrow }
<0> "<-" { tok LArrow }

<0> ","  { tok Comma }
<0> "|"  { tok Pipe }
<0> "@"  { tok At }
<0> "_"  { tok Underscore }

{
data AlexUserState = AlexUserState
  { filePath :: FilePath
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<stdin>"

alexEOF :: Alex Token
alexEOF = do
  (pos, _, _, _) <- alexGetInput
  pure $ Token (Range pos pos) EOF

data Range = Range
  { start :: AlexPosn
  , stop :: AlexPosn
  } deriving (Eq, Show)

instance Semigroup Range where
  Range start _ <> Range _ end = Range start end

instance Monoid Range where
  mempty = Range alexStartPos alexStartPos

data Token = Token
  { rtRange :: Range
  , rtToken :: TokenClass
  } deriving (Eq, Show)

mkRange :: AlexInput -> Int64 -> Range
mkRange (start, _, str, _) len = Range{start = start, stop = stop}
  where
    stop = BS.foldl' alexMove start $ BS.take len str

data TokenClass
  = EOF
  | Constructor ByteString
  | Variable ByteString
  | LBracket
  | RBracket
  | LParen
  | RParen
  | RArrow
  | LArrow
  | Comma
  | Pipe
  | At
  | Underscore
  | Match
  | When
  deriving (Eq, Show)

unToken :: Token -> (Range -> TokenClass -> a) -> a
unToken (Token range token) f = f range token

unToken' :: Token -> (TokenClass -> a) -> a
unToken' token f = unToken token (const f)

tokByteString :: (ByteString -> TokenClass) -> AlexAction Token
tokByteString t inp@(_, _, str, _) len =
  pure Token
    { rtRange = mkRange inp len
    , rtToken = t $ BS.take len str
    }

tok :: TokenClass -> AlexAction Token
tok ctor inp len =
  pure Token
    { rtRange = mkRange inp len
    , rtToken = ctor
    }

scanMany :: ByteString -> Either String [Token]
scanMany input = runAlex input go
  where
    go = do
      output <- alexMonadScan
      if rtToken output == EOF
        then pure [output]
        else (output :) <$> go
}
