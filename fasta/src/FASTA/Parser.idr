module FASTA.Parser

import Data.Bits
import Data.Buffer
import Data.ByteString
import Data.Linear.Ref1
import Derive.Prelude
import FS.Posix
import IO.Async.Loop.Epoll
import IO.Async.Loop.Posix
import Syntax.T1
import Text.ILex.Derive
import Text.ILex.FS

import public Text.ILex

%default total
%language ElabReflection

--------------------------------------------------------------------------------
--          FASTAValue
--------------------------------------------------------------------------------

public export
data FASTAValue : Type where
  NL          : ByteString -> FASTAValue
  HeaderStart : FASTAValue
  HeaderValue : String -> FASTAValue
  Adenine     : FASTAValue
  Thymine     : FASTAValue
  Guanine     : FASTAValue
  Cytosine    : FASTAValue

%runElab derive "FASTAValue" [Show,Eq]

isHeader : FASTAValue -> Bool
isHeader HeaderStart     = True
isHeader (HeaderValue _) = True
isHeader _               = False

isData : FASTAValue -> Bool
isData Adenine  = True
isData Thymine  = True
isData Guanine  = True
isData Cytosine = True
isData _        = False

--------------------------------------------------------------------------------
--          FASTALine
--------------------------------------------------------------------------------

public export
record FASTALine where
  constructor MkFASTALine
  nr     : Nat
  values : List FASTAValue

%runElab derive "FASTALine" [Show,Eq]

Interpolation FASTALine where interpolate = show

--------------------------------------------------------------------------------
--          FASTA
--------------------------------------------------------------------------------

public export
0 FASTA : Type
FASTA = List FASTALine

--------------------------------------------------------------------------------
--          RExp
--------------------------------------------------------------------------------

linebreak : RExp True
linebreak = '\n' <|> '\r' <|> "\r\n"

nucleotide : RExp True
nucleotide = 'A' <|> 'T' <|> 'G' <|> 'C'

adenine : RExp True
adenine = 'A'

thymine : RExp True
thymine = 'T'

guanine : RExp True
guanine = 'G'

cytosine : RExp True
cytosine = 'C'

--------------------------------------------------------------------------------
--          Parser State
--------------------------------------------------------------------------------

public export
record FSTCK (q : Type) where
  constructor F
  line        : Ref q Nat
  col         : Ref q Nat
  psns        : Ref q (SnocList Position)
  strs        : Ref q (SnocList String)
  err         : Ref q (Maybe $ BoundedErr Void)
  fastavalues : Ref q (SnocList FASTAValue)
  fastalines  : Ref q (SnocList FASTALine)
  bytes       : Ref q ByteString

export %inline
HasPosition FSTCK where
  line      = FSTCK.line
  col       = FSTCK.col
  positions = FSTCK.psns

export %inline
HasError FSTCK Void where
  error = err

export %inline
HasStringLits FSTCK where
  strings = strs

export %inline
HasStack FSTCK (SnocList FASTALine) where
  stack = fastalines

export %inline
HasBytes FSTCK where
  bytes = FSTCK.bytes

export
fastainit : F1 q (FSTCK q)
fastainit = T1.do
  l  <- ref1 Z
  c  <- ref1 Z
  bs <- ref1 [<]
  ss <- ref1 [<]
  er <- ref1 Nothing
  fvs <- ref1 [<]
  fls <- ref1 [<]
  by <- ref1 ""
  pure (F l c bs ss er fvs fls by)

--------------------------------------------------------------------------------
--          Parser State
--------------------------------------------------------------------------------

%runElab deriveParserState "FSz" "FST"
  ["FIni", "FBroken", "FHdr", "FHdrToNLR", "FHdrToNLS", "FHdrDone", "FD", "FDNL", "FEmpty", "FComplete"]

--------------------------------------------------------------------------------
--          Errors
--------------------------------------------------------------------------------

fastaErr : Arr32 FSz (FSTCK q -> F1 q (BoundedErr Void))
fastaErr =
  arr32 FSz (unexpected [])
    [ E FBroken $ unexpected ["character other than '>'"]
    , E FEmpty $ unexpected ["sequence data"]
    , E FHdr $ unexpected ["sequence line"]
    ]

--------------------------------------------------------------------------------
--          State Transitions
--------------------------------------------------------------------------------

onFASTAValueHdrS : (x : FSTCK q) => FASTAValue -> F1 q FST
onFASTAValueHdrS v = push1 x.fastavalues v >> pure FHdrToNLS

onFASTAValueHdrR : (x : FSTCK q) => FASTAValue -> F1 q FST
onFASTAValueHdrR v = push1 x.fastavalues v >> pure FHdrToNLR

onFASTAValueAdenine : (x : FSTCK q) => FASTAValue -> F1 q FST
onFASTAValueAdenine v = push1 x.fastavalues v >> pure FD

onFASTAValueThymine : (x : FSTCK q) => FASTAValue -> F1 q FST
onFASTAValueThymine v = push1 x.fastavalues v >> pure FD

onFASTAValueGuanine : (x : FSTCK q) => FASTAValue -> F1 q FST
onFASTAValueGuanine v = push1 x.fastavalues v >> pure FD

onFASTAValueCytosine : (x : FSTCK q) => FASTAValue -> F1 q FST
onFASTAValueCytosine v = push1 x.fastavalues v >> pure FD

onNLFHdr : (x : FSTCK q) => FASTAValue -> F1 q FST
onNLFHdr v = T1.do
  incline 1
  push1 x.fastavalues v
  fvs@(_::_) <- getList x.fastavalues | [] => pure FEmpty
  case Prelude.any isHeader fvs && Prelude.any isData fvs of
    True  => pure FBroken
    False => T1.do
      ln <- read1 x.line
      push1 x.fastalines (MkFASTALine ln fvs)
      pure FHdrDone

onNLFD : (x : FSTCK q) => FASTAValue -> F1 q FST
onNLFD v = T1.do
  incline 1
  push1 x.fastavalues v
  fvs@(_::_) <- getList x.fastavalues | [] => pure FEmpty
  case Prelude.any isHeader fvs && Prelude.any isData fvs of
    True  => pure FBroken
    False => T1.do
      ln <- read1 x.line
      push1 x.fastalines (MkFASTALine ln fvs)
      pure FDNL

onEOI : (x : FSTCK q) => F1 q (Either (BoundedErr Void) FST)
onEOI = T1.do
  incline 1
  fvs@(_::_) <- getList x.fastavalues
    | [] => arrFail FSTCK fastaErr FEmpty x
  ln <- read1 x.line
  push1 x.fastalines (MkFASTALine ln fvs)
  pure (Right FComplete)

fastaInit : DFA q FSz FSTCK
fastaInit =
  dfa
    [ read '>' (\_ => onFASTAValueHdrS HeaderStart)
    ]

fastaHdrStrStart : DFA q FSz FSTCK
fastaHdrStrStart =
  dfa
    [ read dot (onFASTAValueHdrR . HeaderValue)
    ]

fastaHdrStrRest : DFA q FSz FSTCK
fastaHdrStrRest =
  dfa
    [ read dot (onFASTAValueHdrR . HeaderValue)
    , conv linebreak (onNLFHdr . NL)
    ]

fastaFDInit : DFA q FSz FSTCK
fastaFDInit =
  dfa
    [ read adenine (\_ => onFASTAValueAdenine Adenine)
    , read thymine (\_ => onFASTAValueThymine Thymine)
    , read guanine (\_ => onFASTAValueGuanine Guanine)
    , read cytosine (\_ => onFASTAValueCytosine Cytosine)
    ]

fastaFD : DFA q FSz FSTCK
fastaFD =
  dfa
    [ conv linebreak (onNLFD . NL)
    , read adenine (\_ => onFASTAValueAdenine Adenine)
    , read thymine (\_ => onFASTAValueThymine Thymine)
    , read guanine (\_ => onFASTAValueGuanine Guanine)
    , read cytosine (\_ => onFASTAValueCytosine Cytosine)
    ]

fastaSteps : Lex1 q FSz FSTCK
fastaSteps =
  lex1
    [ E FIni fastaInit
    , E FHdrToNLS fastaHdrStrStart
    , E FHdrToNLR fastaHdrStrRest
    , E FHdrDone fastaFDInit
    , E FDNL fastaFDInit
    , E FD fastaFD
    ]

fastaEOI : FST -> FSTCK q -> F1 q (Either (BoundedErr Void) FASTA)
fastaEOI st x =
  case st == FIni || st == FHdr || st == FEmpty || st == FBroken of
    True  => arrFail FSTCK fastaErr st x
    False => T1.do
      _ <- onEOI
      fasta <- getList x.fastalines
      pure (Right fasta)

--------------------------------------------------------------------------------
--          Parser
--------------------------------------------------------------------------------

export
fasta : P1 q (BoundedErr Void) FSz FSTCK FASTA
fasta = P FIni fastainit fastaSteps snocChunk fastaErr fastaEOI

export %inline
parseFASTA : Origin -> String -> Either (ParseError Void) FASTA
parseFASTA = parseString fasta

--------------------------------------------------------------------------------
--          Streaming
--------------------------------------------------------------------------------

streamFASTA :  String
            -> AsyncPull Poll Void [ParseError Void, Errno] ()
streamFASTA pth =
     readBytes pth
  |> streamParse fasta (FileSrc pth)
  |> C.count
  |> printLnTo Stdout

streamFASTAFiles :  AsyncPull Poll String [ParseError Void, Errno] ()
                 -> AsyncPull Poll Void [ParseError Void, Errno] ()
streamFASTAFiles pths =
     flatMap pths (\p => readBytes p |> streamParse fasta (FileSrc p))
  |> C.count
  |> printLnTo Stdout
