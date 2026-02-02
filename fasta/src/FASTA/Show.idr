module FASTA.Show

import FASTA.Parser

%default total

--------------------------------------------------------------------------------
--          Show
--------------------------------------------------------------------------------

export
showFASTAValue : FASTAValue -> String
showFASTAValue (NL (_, v))          = toString v
showFASTAValue (HeaderStart _)      = ">"
showFASTAValue (HeaderValue (_, v)) = v
showFASTAValue (Adenine _)          = "A"
showFASTAValue (Thymine _)          = "T"
showFASTAValue (Guanine _)          = "G"
showFASTAValue (Cytosine _)         = "C"

export
showFASTALine : FASTALine -> String
showFASTALine (MkFASTALine _ values) = concat $ map showFASTAValue values

export
showFASTA : FASTA -> String
showFASTA fastalines = concat $ map showFASTALine fastalines
