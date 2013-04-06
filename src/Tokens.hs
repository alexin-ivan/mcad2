

module Tokens where

data TokenType = 
    TModule |
    TEndmodule |
    TAssign |
    TParameter |
    TDefparam |

    TInput |
    TInout |
    TOutput |

    TTri |
    TWire |

    TParROpen |
    TParRClose |
    TParFOpen |
    TParFClose |
    TParQOpen |
    TParQClose |
    TColon |
    TSemiColon |
    TComma |
    TPoint |
    TEqual |
    TQuest |

	TTilda |
    TAnd |
    TOr |

    
    TBinDigit String |
    THexDigit String |
    
    TBinIxDigit String |
    THexIxDigit String |
    
    TDecDigit String |
    TId String |
    TString String |

    TLexError String|
    TEOF

    deriving(Eq,Ord,Show,Read)


class SimpleShow a where
	simple_show :: a -> String


data Token = Token {
    token_line :: Int,
    token_col :: Int,
    token_current_char :: Char,
    token_current_string :: String,
    token_type :: TokenType
    }
    deriving(Eq,Ord,Read)

instance Show Token where
    show = show_token

show_token (Token l c cc cs t) = show t
-- show_token (Token l c cc cs t) = token_simple_show t



token_simple_show TModule      = "module"                   
token_simple_show TEndmodule   = "endmodule"                
token_simple_show TAssign      = "assign"                   
token_simple_show TParameter   = "parameter"                
token_simple_show TDefparam    = "defparam"                
token_simple_show TInput       = "input"                    
token_simple_show TOutput      = "output"                   
token_simple_show TInout       = "inout"                    
token_simple_show TTri         = "tri"                      
token_simple_show TWire        = "wire"					   
token_simple_show TParROpen    = "("                        
token_simple_show TParRClose   = ")"                        
token_simple_show TParFOpen    = ""                         
token_simple_show TParFClose   = ""                         
token_simple_show TParQOpen    = "["                        
token_simple_show TParQClose   = "]"                        
token_simple_show TColon       = ":"                        
token_simple_show TSemiColon   = ";"                        
token_simple_show TEqual       = "="                        
token_simple_show TComma       = ","                        
token_simple_show TPoint       = "."                        
token_simple_show TQuest       = "?"

token_simple_show TTilda       = "~"
token_simple_show TAnd         = "&"
token_simple_show TOr          = "|"

token_simple_show (TBinDigit s) = s
token_simple_show (THexDigit s) = s

token_simple_show (TBinIxDigit s) = s
token_simple_show (THexIxDigit s) = s


token_simple_show (TDecDigit s) = s

token_simple_show (TId s) = s

token_simple_show (TString s) = s

token_simple_show TEOF = ""

token_simple_show x = error ("Undefined show token :" ++ show x)


showPos (Token line col _ _ _ ) = show line ++ ':': show col

syntax_error_token :: String -> Token -> Token -> String
syntax_error_token s t nt =
    (showPos t ++ ": " ++ s ++ " before `" ++ show nt ++ "` token")
    where
    (Token l c cc rest tt) = t
    (Token _ _ _  _   ntt) = nt

findError' ::  [TokenType] -> [Token] -> String
findError' err toks = syntax_error_token " syntax error " (head ts') (head $ tail ts')
    where
    rtoks = reverse toks
    rerr = reverse err
    ts' = map fst $ reverse $ zip rtoks rerr
	-- ts  = map fst ts'

findError ::  String -> [Token] -> String
findError err_str toks = syntax_error_token " syntax error " (head ts') (head $ tail ts')
    where
    err = read err_str :: [TokenType]
    rtoks = reverse toks
    rerr = reverse err
    ts' = map fst $ reverse $ zip rtoks rerr
	-- ts  = map fst ts'
	

