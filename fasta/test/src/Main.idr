module Main

import FASTA.Parser
import FASTA.Show

import Hedgehog

%default total

--------------------------------------------------------------------------------
--          Example FASTA (String)
--------------------------------------------------------------------------------

fastastrminimal : String
fastastrminimal =
  """
  >x
  A
  """

fastastr : String
fastastr =
  """
  >This is the header line of a FASTA file
  ATGTGAAAAACCCGGGGGTTTTGTTGTTGTGTGTGGTGTTTG
  ATGTGAAAAACCCGGGGGTTTTGTTGTTGTGTGTGGTGTTTG
  ATGTGAAAAACCCGGGGGTTTTGTTGTTGTGTGTGGTGTTTG
  ATGTGAAAAACCCGGGGGTTTTGTTGTTGTGTGTGGTGTTTG
  ATGTGAAAAACCCGGGGGTTTTGTTGTTGTGTGTGGTGTTTG
  ATGTGAAAAACCCGGGGGTTTTGTTGTTGTGTGTGGTGTTTG
  ATGTGAAAAACCCGGGGGTTTTGTTGTTGTGTGTGGTGTTTG
  ATGTGAAAAACCCGGGGGTTTTGTTGTTGTGTGTGGTGTTTG
  ATGTGAAAAACCCGGGGGTTTTGTTGTTGTGTGTGGTGTTTG
  ATGTGAAAAACCCGGGGGTTTTGTTGTTGTGTGTGGTGTTTG
  ATGTGAAAAACCCGGGGGTTTTGTTGTTGTGTGTGGTGTTTG
  ATGTGAAAAACCCGGGGGTTTTGTTGTTGTGTGTGGTGTTTG
  ATGTGAAAAACCCGGGGGTTTTGTTGTTGTGTGTGGTGTTTG
  ATGTGAAAAACCCGGGGGTTTTGTTGTTGTGTGTGGTGTTTG
  """

fastastrnoheader : String
fastastrnoheader =
  """
  ATTG
  ATGA
  """

fastastrheaderaftersequence : String
fastastrheaderaftersequence =
  """
  >x
  A
  >x
  """

fastastrbadsequence : String
fastastrbadsequence =
  """
  >x
  ATGO
  """

fastastrempty : String
fastastrempty =
  """
  """

--------------------------------------------------------------------------------
--          Errors
--------------------------------------------------------------------------------

fastastrnoheadererr : String
fastastrnoheadererr =
  "Error: Expected \"no header line\", but got 'T'\n\nvirtual: 1:2--1:3\n 1 | ATTG\n      ^\n"

fastastrheaderaftersequenceerr : String
fastastrheaderaftersequenceerr =
  "Error: Expected \"header line already encountered\", but got 'x'\n\nvirtual: 3:3--3:4\n 1 | >x\n 2 | A\n 3 | >x\n       ^\n"

fastastrbadsequenceerr : String
fastastrbadsequenceerr =
  "Error: Unexpected 'O'\n\nvirtual: 2:5--2:6\n 1 | >x\n 2 | ATGO\n         ^\n"

fastastremptyerr : String
fastastremptyerr =
  "Error: Unexpected end of input\n\nvirtual: 1:1--1:2\n     ^\n"

--------------------------------------------------------------------------------
--          testErr
--------------------------------------------------------------------------------

testErr : String -> String -> Property
testErr s exp =
  let res := case parseFASTA Virtual s of
        Left e  => interpolate e
        Right v => show v
   in property1 $ res === exp

--------------------------------------------------------------------------------
--          Property Tests
--------------------------------------------------------------------------------

prop_minimal_roundtrip : Property
prop_minimal_roundtrip = property1 $ do
  Right parsedfastastrminimal <- pure $ parseFASTA Virtual fastastrminimal
    | Left _ => failure
  parseFASTA Virtual (showFASTA parsedfastastrminimal) === Right parsedfastastrminimal

prop_roundtrip : Property
prop_roundtrip = property1 $ do
  Right parsedfastastr <- pure $ parseFASTA Virtual fastastr
    | Left _ => failure
  parseFASTA Virtual (showFASTA parsedfastastr) === Right parsedfastastr

prop_no_header : Property
prop_no_header = testErr fastastrnoheader fastastrnoheadererr

prop_header_after_sequence : Property
prop_header_after_sequence = testErr fastastrheaderaftersequence fastastrheaderaftersequenceerr

prop_bad_sequence : Property
prop_bad_sequence = testErr fastastrbadsequence fastastrbadsequenceerr

prop_empty : Property
prop_empty = testErr fastastrempty fastastremptyerr

--------------------------------------------------------------------------------
--          Properties
--------------------------------------------------------------------------------

properties : Group
properties =
  MkGroup
    "FASTA.Parser"
    [ ("prop_minimal_roundtrip", prop_minimal_roundtrip)
    , ("prop_roundtrip", prop_roundtrip)
    , ("prop_no_header", prop_no_header)
    , ("prop_header_after_sequence", prop_header_after_sequence)
    , ("prop_bad_sequence", prop_bad_sequence)
    , ("prop_empty", prop_empty)
    ]

main : IO ()
main = test [ properties ]
