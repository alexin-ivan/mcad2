-- Lex.x -*- mode: haskell -*-
{
module Lex where
import Tokens
}


%wrapper "monad"

$alpha = [a-zA-Z]
$digit = 0-9

$bin_value = [0-1 z x]
$bin_ix_value = [0-1]

$hex_value = [0-9 A-F a-f z x]
$hex_ix_value = [0-9 A-F a-f]

$quote = "

-- $graphic    = $printable # $white

@string     = \" ($printable # \")* \"

@var_id = [$alpha] [$alpha $digit \_\~]*

@extra_var_id = \\[$alpha$digit\_\~\[\]\|]*

tokens :-
    $white+                     ;
    "module"                    {token_simple TModule}
    "endmodule"                 {token_simple TEndmodule}
    "assign"                    {token_simple TAssign }
    "parameter"                 {token_simple TParameter}
    "defparam"                   {token_simple TDefparam}

    "input"                     {token_simple TInput }
    "output"                    {token_simple TOutput }
    "inout"                     {token_simple TInout }
    
    "tri"                       {token_simple TTri }
    "wire"						{token_simple TWire }

    "("                         {token_simple TParROpen }
    ")"                         {token_simple TParRClose }
    "{"                         {token_simple TParFOpen }
    "}"                         {token_simple TParFClose }
    "["                         {token_simple TParQOpen }
    "]"                         {token_simple TParQClose }
    ":"                         {token_simple TColon }
    ";"                         {token_simple TSemiColon }
    "="                         {token_simple TEqual }
    ","                         {token_simple TComma }
    "."                         {token_simple TPoint }
    "?"                         {token_simple TQuest }

    "~"                         {token_simple TTilda }
    "&"                         {token_simple TAnd }
    "|"                         {token_simple TOr }
    
    "//" .*                     ; -- comment
    "`" .*                      ; -- vpp
    
    @extra_var_id               {token_input (\s -> TId $ tail s) }

    $digit+ "'b" $bin_value+  {token_input (\s -> TBinDigit s) }
    $digit+ "'h" $hex_value+  {token_input (\s -> THexDigit s) }

    $digit+ "'b" $bin_ix_value+  {token_input (\s -> TBinIxDigit s) }
    $digit+ "'h" $hex_ix_value+  {token_input (\s -> THexIxDigit s) }


    @var_id                     {token_input (\s -> TId s) }
    $digit+                     {token_input (\s -> TDecDigit s) }

    @string                     {token_input (\s -> TString s) }

{

-- tconst :: Tokens -> ( (AlexPn,Char,[Byte],[Char]) -> Int -> Alex Tokens)
-- tconst x ainp len = return x

alexError' :: String -> Alex a
alexError' message = Alex $ \s -> Left message


showPosn (AlexPn _ line col) = show line ++ ':': show col

lexError s = do
  (p,c,rest,input) <- alexGetInput
  alexError' (showPosn p ++ ": " ++ s ++ 
                   (if (not (null input))
                     then " before " ++ show (head input)
                     else " at end of file"))

lexError2 s (p,c,rest,input) =  showPosn p ++ ": " ++ s ++ 
                   (if (not (null input))
                     then " before " ++ show (head input)
                     else " at end of file")



-- type AlexInput = (AlexPn,Char,[Byte],[Char])

token_simple :: TokenType -> AlexInput -> Int -> Alex Token
token_simple tok_type (AlexPn _ line col,cur_char,rest_bytes,sinput) len  = return $
        Token line col cur_char (take len sinput) tok_type 

token_input :: (String -> TokenType) -> AlexInput -> Int -> Alex Token
token_input tok_type (AlexPn _ line col,cur_char,rest_bytes,sinput) len  = return $
        Token line col cur_char (take len sinput) (tok_type $ take len sinput)


alexEOF = return $ Token 0 0 'E' "EOF" TEOF


alexMonadScan' fn = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError inp' -> error $ ("In file \"" ++ fn ++ "\":") ++ (lexError2 "lexical error in" inp')
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan
    AlexToken inp' len action -> do
        alexSetInput inp'
        action inp len

alexScanTokens :: FilePath -> String -> Either String [Token]
alexScanTokens fn str = runAlex str $ do
    let loop = do
        tok <- alexMonadScan' fn
        case (token_type tok) of
            TEOF -> return [tok]
            _    -> do toks <- loop
                       return (tok:toks)
    loop

}
