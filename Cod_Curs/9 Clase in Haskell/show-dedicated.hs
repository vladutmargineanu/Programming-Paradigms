show4Bool True  = "True"
show4Bool False = "False"

show4Char c     = "'" ++ [c] ++ "'"

show4String s   = "\"" ++ s ++ "\""