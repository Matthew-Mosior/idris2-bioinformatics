module FASTA.Show

import FASTA.Parser

%default total

--------------------------------------------------------------------------------
--          Show
--------------------------------------------------------------------------------

export
showFASTAValue : FASTAValue -> String
showFASTAValue (NL v)          = toString v
showFASTAValue HeaderStart     = ">"
showFASTAValue (HeaderValue v) = v
showFASTAValue Adenine         = "A"
showFASTAValue Thymine         = "T"
showFASTAValue Guanine         = "G"
showFASTAValue Cytosine        = "C"

export
showFASTALine : FASTALine -> String
showFASTALine (MkFASTALine _ values) = concat $ map showFASTAValue values

export
showFASTA : FASTA -> String
showFASTA fastalines = concat $ map showFASTALine fastalines
