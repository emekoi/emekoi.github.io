{
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
import Data.ByteString.Internal   qualified as BS (c2w)
import Data.Int                   (Int64)
import Data.Word                  (Word8)
}

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
-- -----------------------------------------------------------------------------
-- The input type
type AlexInput = (AlexPosn, -- current position,
                  Char, -- previous char
                  ByteString, -- current input string
                  Int64) -- bytes consumed so far

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes i = i -- no pending bytes when lexing bytestrings

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_,c,_,_) = c

alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (p,_,cs,n) =
  case BS.uncons cs of
    Nothing -> Nothing
    Just (c, cs') ->
      let p' = alexMove p c; n' = n+1
      in p' `seq` cs' `seq` n' `seq` Just (BS.c2w c, (p', c, cs',n'))

-- -----------------------------------------------------------------------------
-- Token positions
-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of characters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.

data AlexPosn = AlexPn !Int !Int !Int
  deriving (Eq, Ord, Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1) l (c+alex_tab_size-((c-1) `mod` alex_tab_size))
alexMove (AlexPn a l _) '\n' = AlexPn (a+1) (l+1) 1
alexMove (AlexPn a l c) _ = AlexPn (a+1) l (c+1)

-- -----------------------------------------------------------------------------
-- Monad (default and with ByteString input)

data AlexUserState = AlexUserState
  { filePath :: FilePath
  }

data AlexState = AlexState
  { alex_pos  :: !AlexPosn
    -- position at current input location
  , alex_bpos :: !Int64
    -- bytes consumed so far
  , alex_inp  :: ByteString
    -- the current input
  , alex_chr  :: !Char
    -- the character before the input
  , alex_scd  :: !Int
    -- the current startcode
  , alex_ust  :: AlexUserState
    -- AlexUserState will be defined in the user program
  }

-- Compile with -funbox-strict-fields for best results!
runAlex :: ByteString -> Alex a -> Either String a
runAlex input__ (Alex f)
   = case f (AlexState {alex_bpos = 0,
                        alex_pos = alexStartPos,
                        alex_inp = input__,
                        alex_chr = '\n',
                        alex_ust = alexInitUserState,
                        alex_scd = 0}) of Left msg       -> Left msg
                                          Right ( _, a ) -> Right a

newtype Alex a
  = Alex { unAlex :: AlexState -> Either String (AlexState, a) }

instance Functor Alex where
  fmap f a = Alex $ \s -> case unAlex a s of
                            Left msg       -> Left msg
                            Right (s', a') -> Right (s', f a')

instance Applicative Alex where
  pure a = Alex $ \s -> Right (s, a)
  fa <*> a = Alex $ \s -> case unAlex fa s of
                            Left msg -> Left msg
                            Right (s', f) -> case unAlex a s' of
                                               Left msg -> Left msg
                                               Right (s'', b) -> Right (s'', f b)
instance Monad Alex where
  m >>= k = Alex $ \s -> case unAlex m s of
                                Left msg     -> Left msg
                                Right (s',a) -> unAlex (k a) s'

alexGetInput :: Alex AlexInput
alexGetInput
 = Alex $ \s@AlexState{alex_pos=pos,alex_bpos=bpos,alex_chr=c,alex_inp=inp__} ->
        Right (s, (pos,c,inp__,bpos))

alexSetInput :: AlexInput -> Alex ()
alexSetInput (pos,c,inp__,bpos)
 = Alex $ \s -> case s{alex_pos=pos,
                       alex_bpos=bpos,
                       alex_chr=c,
                       alex_inp=inp__} of
                    state__@(AlexState{}) -> Right (state__, ())

alexError :: String -> Alex a
alexError message = Alex $ const $ Left message

alexGetStartCode :: Alex Int
alexGetStartCode = Alex $ \s@AlexState{alex_scd=sc} -> Right (s, sc)

alexSetStartCode :: Int -> Alex ()
alexSetStartCode sc = Alex $ \s -> Right (s{alex_scd=sc}, ())

alexGetUserState :: Alex AlexUserState
alexGetUserState = Alex $ \s@AlexState{alex_ust=ust} -> Right (s,ust)

alexSetUserState :: AlexUserState -> Alex ()
alexSetUserState ss = Alex $ \s -> Right (s{alex_ust=ss}, ())

alexMonadScan = do
  inp__@(_,_,_,n) <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp__ sc of
    AlexEOF -> alexEOF
    AlexError ((AlexPn _ line column),_,_,_) -> alexError $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
    AlexSkip inp__' _len -> do
        alexSetInput inp__'
        alexMonadScan
    AlexToken inp__'@(_,_,_,n') _ action -> let len = n'-n in do
        alexSetInput inp__'
        action (ignorePendingBytes inp__) len

-- -----------------------------------------------------------------------------
-- Useful token actions
type AlexAction result = AlexInput -> Int64 -> Alex result

-- just ignore this token and scan another one
skip :: AlexAction Token
skip _input _len = alexMonadScan

-- ignore this token, but set the start code to a new value
begin :: Int -> AlexAction Token
begin code _input _len = do alexSetStartCode code; alexMonadScan

-- perform an action for this token, and set the start code to a new value
andBegin :: AlexAction Token -> Int -> AlexAction Token
(action `andBegin` code) input__ len = do
  alexSetStartCode code
  action input__ len

token :: (AlexInput -> Int64 -> token) -> AlexAction token
token t input__ len = pure (t input__ len)

-- -----------------------------------------------------------------------------
-- epilogue

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
