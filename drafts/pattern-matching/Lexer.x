{
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lexer
  ( Alex
  , AlexPosn (..)
  , alexGetInput
  , getFilePath
  , alexError'
  , runAlex
  , runAlex'
  , alexMonadScan'
  , Range (..)
  , Token (..)
  , TokenClass (..)
  , unToken
  , scanMany
  ) where

import Control.Monad.State.Class
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BS
import Prettyprinter              qualified as P
}

%wrapper "monadUserState-bytestring"

$digit = [0-9]
$alpha = [a-zA-Z]

@identifier  = ($alpha | $digit | \_ | \')
@constructor = ([A-Z]) @identifier* (\? | \!)?
@variable    = ([a-z] @identifier* | \_ @identifier+) (\? | \!)?

tokens :-

-- whitespace and comments
<0> $white+ ;
<0> "--" .* ;

-- literals
<0> "-"? $digit+ { tokInteger }
<0> \"[^\"]*\"   { tokByteString String }

-- keywords
<0> data  { tok Data }
<0> in    { tok In }
<0> let   { tok Let }
<0> match { tok Match }

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
<0> "="  { tok Eq }

-- variables and constructors
<0> @variable     { tokByteString Variable }
<0> @constructor  { tokByteString Constructor }

{
data AlexUserState = AlexUserState
  { filePath :: FilePath
  }

instance MonadState AlexUserState Alex where
  get    = Alex $ \s@AlexState{alex_ust=ust} -> Right (s,ust)
  put ss = Alex $ \s -> Right (s{alex_ust=ss}, ())

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<stdin>"

getFilePath :: Alex FilePath
getFilePath = gets filePath

setFilePath :: FilePath -> Alex ()
setFilePath = put . AlexUserState

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

instance P.Pretty AlexPosn where
  pretty (AlexPn _ l c) = P.pretty l <> ":" <> P.pretty c

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
  -- literals
  | String ByteString
  | Int Integer
  -- keywords
  | Data
  | In
  | Let
  | Match
  -- operators and symbols
  | LBracket
  | RBracket
  | LParen
  | RParen
  | RArrow
  | LArrow
  | Comma
  | Pipe
  | At
  | Eq
  | Underscore
  deriving (Eq, Show)

unToken :: Token -> (Range -> TokenClass -> a) -> a
unToken (Token range token) f = f range token

tok :: TokenClass -> AlexAction Token
tok ctor inp len =
  pure Token
    { rtRange = mkRange inp len
    , rtToken = ctor
    }

tokByteString :: (ByteString -> TokenClass) -> AlexAction Token
tokByteString t inp@(_, _, str, _) len =
  pure Token
    { rtRange = mkRange inp len
    , rtToken = t $ BS.take len str
    }

tokInteger :: AlexAction Token
tokInteger inp@(_, _, str, _) len =
  pure Token
    { rtRange = mkRange inp len
    , rtToken = Int . read . BS.unpack $ BS.take len str
    }

alexMonadScan' = do
  inp__@(_,_,_,n) <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp__ sc of
    AlexEOF -> alexEOF
    AlexError (p,_,_,_) ->
      alexError' p $ "lexical error"
    AlexSkip  inp__' _len -> do
        alexSetInput inp__'
        alexMonadScan
    AlexToken inp__'@(_,_,_,n') _ action -> let len = n'-n in do
        alexSetInput inp__'
        action (ignorePendingBytes inp__) len

alexError' :: AlexPosn -> String -> Alex a
alexError' (AlexPn _ l c) msg = do
  fp <- getFilePath
  alexError (fp ++ ":" ++ show l ++ ":" ++ show c ++ ": " ++ msg)

runAlex' :: FilePath -> ByteString -> Alex a -> Either String a
runAlex' fp input a = runAlex input (setFilePath fp >> a)

scanMany :: ByteString -> Either String [Token]
scanMany input = runAlex input go
  where
    go = do
      output <- alexMonadScan
      if rtToken output == EOF
        then pure [output]
        else (output :) <$> go
}
