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
import Text.ILex.State.Derive
import Text.ILex.FS

import public Text.ILex

%default total
%language ElabReflection

--------------------------------------------------------------------------------
--          Coordinate System
--------------------------------------------------------------------------------

public export
data CoordinateSystem = ZeroBased
                      | OneBased

%runElab derive "CoordinateSystem" [Show,Eq]

--------------------------------------------------------------------------------
--          FASTAValue
--------------------------------------------------------------------------------

public export
data FASTAValue : Type where
  NL          : ByteString -> FASTAValue
  HeaderStart : FASTAValue
  HeaderValue : String -> FASTAValue
  Adenine     : Nat -> FASTAValue
  Thymine     : Nat -> FASTAValue
  Guanine     : Nat -> FASTAValue
  Cytosine    : Nat -> FASTAValue

%runElab derive "FASTAValue" [Show,Eq]

isHeader : FASTAValue -> Bool
isHeader HeaderStart     = True
isHeader (HeaderValue _) = True
isHeader _               = False

isData : FASTAValue -> Bool
isData (Adenine _)  = True
isData (Thymine _)  = True
isData (Guanine _)  = True
isData (Cytosine _) = True
isData _            = False

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
  bufSize_     : Nat
  prev_        : ByteString
  cur_         : IBuffer bufSize_
  prevOffset_  : Nat
  curOffset_   : Nat
  from_        : Ref q (LTENat bufSize_)
  till_        : Ref q (LTENat bufSize_)
  positions_   : Ref q (SnocList BytePos)
  strs         : Ref q (SnocList String)
  err          : Ref q (Maybe $ BBErr Void)
  fastavalues  : Ref q (SnocList FASTAValue)
  fastalines   : Ref q (SnocList FASTALine)
  fastacounter : Ref q Nat
  line         : Ref q Nat

export %inline
HasBBErr FSTCK Void where
  error = err

export %inline
HasStringLits FSTCK where
  strings = strs

export %inline
HasStack FSTCK (SnocList FASTALine) where
  stack = fastalines

export %inline
HasBytes FSTCK where
  bufSize    = bufSize_
  prev       = prev_
  cur        = cur_
  prevOffset = prevOffset_
  curOffset  = curOffset_
  from       = from_
  till       = till_
  positions  = positions_
  copy s o bs buf rf rt sk =
    { bufSize_    := s
    , cur_        := buf
    , prev_       := bs
    , prevOffset_ := o
    , curOffset_  := o + bs.size
    , from_       := rf
    , till_       := rt
    } sk

export
fastainit : CoordinateSystem -> (n : Nat) -> IBuffer n -> F1 q (FSTCK q)
fastainit coordsys n buf = T1.do
  rf <- ref1 (first n)
  rt <- ref1 (first n)
  ps <- ref1 [<]
  ss <- ref1 [<]
  er <- ref1 Nothing
  fvs <- ref1 [<]
  fls <- ref1 [<]
  ln  <- ref1 Z
  fc <- case coordsys of
          ZeroBased => ref1 Z
          OneBased => ref1 (S Z)
  by <- ref1 ""
  pure (F n empty buf 0 0 rf rt ps ss er fvs fls fc ln)

--------------------------------------------------------------------------------
--          Parser State
--------------------------------------------------------------------------------

%runElab deriveParserState "FSz" "FST"
  ["FIni", "FBroken", "FHdr", "FHdrToNLR", "FHdrToNLS", "FHdrDone", "FD", "FDNL", "FEmpty", "FComplete"]

--------------------------------------------------------------------------------
--          Errors
--------------------------------------------------------------------------------

fastaErr : Arr32 FSz (FSTCK q -> F1 q (BBErr Void))
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

onFASTAValueAdenine : (x : FSTCK q) => F1 q FST
onFASTAValueAdenine = T1.do
  fc <- read1 x.fastacounter
  push1 x.fastavalues (Adenine fc) >> write1 x.fastacounter (S fc) >> pure FD

onFASTAValueThymine : (x : FSTCK q) => F1 q FST
onFASTAValueThymine = T1.do
  fc <- read1 x.fastacounter
  push1 x.fastavalues (Thymine fc) >> write1 x.fastacounter (S fc) >> pure FD

onFASTAValueGuanine : (x : FSTCK q) => F1 q FST
onFASTAValueGuanine = T1.do
  fc <- read1 x.fastacounter
  push1 x.fastavalues (Guanine fc) >> write1 x.fastacounter (S fc) >> pure FD

onFASTAValueCytosine : (x : FSTCK q) => F1 q FST
onFASTAValueCytosine = T1.do
  fc <- read1 x.fastacounter
  push1 x.fastavalues (Cytosine fc) >> write1 x.fastacounter (S fc) >> pure FD

onNLFHdr : (x : FSTCK q) => ByteString -> F1 q FST
onNLFHdr v = T1.do
  mod1 x.line S
  push1 x.fastavalues (NL v)
  fvs@(_::_) <- getList x.fastavalues | [] => pure FEmpty
  case Prelude.any isHeader fvs && Prelude.any isData fvs of
    True  => pure FBroken
    False => T1.do
      ln <- read1 x.line
      push1 x.fastalines (MkFASTALine ln fvs)
      pure FHdrDone

onNLFD : (x : FSTCK q) => ByteString -> F1 q FST
onNLFD v = T1.do
  mod1 x.line S
  push1 x.fastavalues (NL v)
  fvs@(_::_) <- getList x.fastavalues | [] => pure FEmpty
  case Prelude.any isHeader fvs && Prelude.any isData fvs of
    True  => pure FBroken
    False => T1.do
      ln <- read1 x.line
      push1 x.fastalines (MkFASTALine ln fvs)
      pure FDNL

onEOI : (x : FSTCK q) => F1 q (Either (BBErr Void) FST)
onEOI = T1.do
  mod1 x.line S
  fvs@(_::_) <- getList x.fastavalues
    | [] => arrFail FSTCK fastaErr FEmpty x
  ln <- read1 x.line
  push1 x.fastalines (MkFASTALine ln fvs)
  pure (Right FComplete)

fastaInit : DFA q FSz FSTCK
fastaInit =
  dfa
    [ string '>' (\_ => onFASTAValueHdrS HeaderStart)
    ]

fastaHdrStrStart : DFA q FSz FSTCK
fastaHdrStrStart =
  dfa
    [ string dot (onFASTAValueHdrR . HeaderValue)
    ]

fastaHdrStrRest : DFA q FSz FSTCK
fastaHdrStrRest =
  dfa
    [ string dot (onFASTAValueHdrR . HeaderValue)
    , bytes linebreak (\bs => onNLFHdr bs)
    ]

fastaFDInit : DFA q FSz FSTCK
fastaFDInit =
  dfa
    [ step adenine onFASTAValueAdenine
    , step thymine onFASTAValueThymine
    , step guanine onFASTAValueGuanine
    , step cytosine onFASTAValueCytosine
    ]

fastaFD : DFA q FSz FSTCK
fastaFD =
  dfa
    [ bytes linebreak onNLFD
    , step adenine onFASTAValueAdenine
    , step thymine onFASTAValueThymine
    , step guanine onFASTAValueGuanine
    , step cytosine onFASTAValueCytosine
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

fastaEOI : FST -> FSTCK q -> F1 q (Either (BBErr Void) FASTA)
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

public export
fasta : CoordinateSystem -> P1 q (BBErr Void) FASTA
fasta coordsys = P FIni (fastainit coordsys) fastaSteps snocChunk fastaErr fastaEOI

export %inline
parseFASTA : CoordinateSystem -> Origin -> String -> Either (ParseError Void) FASTA
parseFASTA coordsys origin str = parseString (fasta coordsys) origin str

--------------------------------------------------------------------------------
--          Streaming
--------------------------------------------------------------------------------

streamFASTA :  CoordinateSystem
            -> String
            -> AsyncPull Poll Void [BBErr Void, Errno] ()
streamFASTA coordsys pth =
     readBytes pth
  |> streamParse (fasta coordsys)
  |> C.count
  |> printLnTo Stdout

streamFASTAFiles :  CoordinateSystem
                 -> AsyncPull Poll String [BBErr Void, Errno] ()
                 -> AsyncPull Poll Void [BBErr Void, Errno] ()
streamFASTAFiles coordsys pths =
     flatMap pths (\p => readBytes p |> streamParse (fasta coordsys))
  |> C.count
  |> printLnTo Stdout
