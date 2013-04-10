-- Syntax.y -*- mode : haskell -*-
{

module Syntax where

import AST
import Lex
import Tokens
import qualified Data.Tree as Tree

}


%name syntax_parser
%tokentype { TokenType }
%monad { Either String } { (>>=) } { return }

%token

    module			            {TModule}
    endmodule			        {TEndmodule}
    assign                      {TAssign }
    parameter                   {TParameter}
    defparam			{TDefparam }

    input                       { TInput }
    output                      { TOutput }
    inout                       { TInout }

    tri                         { TTri }
    wire                        { TWire }

    "("                         { TParROpen }
    ")"                         { TParRClose }
    attr_begin                  { TAttrBegin }
    attr_end                    { TAttrEnd }
    "{"                         { TParFOpen }
    "}"                         { TParFClose }
    "["                         { TParQOpen }
    "]"                         { TParQClose }
    ":"                         { TColon }
    ";"                         { TSemiColon }
    "="                         { TEqual }
    ","                         { TComma }
    "."                         { TPoint }
    "?"                         { TQuest }

    "~"                         { TTilda }
    "&"                         { TAnd }
    "|"                         { TOr }
    
    bin_digit                   {TBinDigit $$ }
    hex_digit                   {THexDigit $$ }
    
    bin_ix_digit                {TBinIxDigit $$ }
    hex_ix_digit                {THexIxDigit $$ }
    
    dec_digit                   {TDecDigit $$ }
    var_id                      {TId $$ }

    string_t                    {TString $$ }

    eof                         { TEOF }

%right "~"
%left "&"
%left "|"
%left "?"
%left ":"
-- %left Module
-- %right ";"
-- %left "="
-- %left ","

%%

VerilogFile :: { [Module] }
    : ModuleList eof               { $1 }

ModuleList  :: { [Module] }
    :
	  Module		        { [$1] }
	  | ModuleList Module	{ $2 : $1 }
      |                     { [] }


Module :: { Module }
    :
    AttributeList  module var_id "(" VarList ")" ";" ModuleStms endmodule { Module $1 $3 $5 $8 }

-- 0 and any
VarList :: { [String] }
    :
    var_id                  { [$1] }
    | var_id "," VarList    { $1 : $3 }
    |                       { [] }

-- one or any
VarListOneOrMore :: { [String] }
    :
    var_id VarListOneOrMoreRest  { $1 : $2 }

VarListOneOrMoreRest :: { [String] }
    :
    "," VarListOneOrMore   { $2 }
    |                      { [] }


ModuleStms :: { ModuleStms }
    :
	  PortImplList WireDefList ParameterList StmsList	{ ModuleStms $1 $2 $3 $4 } -- $2 $3 }

PortImplList :: { [PortDecl] }
    : PortImpl ";"		       { [$1] }
    | PortImpl ";" PortImplList { $1 : $3 }
    |			       { [] }

PortImpl :: { PortDecl }
    : Direction Range VarListOneOrMore { PortDecl $1 $2 $3 }

Direction :: { Direction }
    : input         { Input }
    | output        { Output }
    | inout         { Inout }


AttributeList :: { AttributeList }
    : attr_begin Attributes attr_end { AttributeList $2 }
    |                                { AttributeList [] }

Attributes :: { [Attribute] }
    : Attributes "," Attribute          { $3 : $1 }
    | Attribute                     { [$1] }
    |                               { [] }

Attribute :: { Attribute }
    : var_id "=" dec_digit          { Attribute $1 (read $3) }

AnyIxDigit :: { Digit }
    : bin_ix_digit                  { digit_to_int (BinIxDigit $1) }
    | hex_ix_digit                  { digit_to_int (HexIxDigit $1) }
    | dec_digit                     { digit_to_int (DecDigit $1) }

AnyNoIxDigit :: { Digit }
    : bin_digit                     { BinIxDigit $1 }
    | hex_digit                     { HexIxDigit $1 }


WireDefList :: { [Wire]  }
    : WireDef ";"                         { [$1]    }
    | WireDef ";" WireDefList             { $1 : $3 }
    |                                     { []      }

WireDef :: { Wire }
    : WireType Range VarListOneOrMore    { Wire $1 $2 $3 }

WireType :: {WireType }
    : tri                               { WireTri }
    | wire                              { WireSimple }


ParameterList :: { [Parameter] }
    : Parameter ";"                       { [$1] }
    | Parameter ";" ParameterList       { $1 : $3 }
    |                                   { []   }

Parameter :: { Parameter }
    : parameter var_id "=" Expr        { Parameter $2 $4 }

AnyDigit :: {Digit}
    : AnyIxDigit                      { $1 }
    | AnyNoIxDigit                    { $1 }


StmsList :: {[Stm]}
    : Stm ";"                           { [$1] }
    | Stm ";" StmsList                  { $1 : $3 }
    |                                   { []    }


Stm :: {Stm}
    : AssignStmt                         { $1 }
    | SubcktStm                          { $1 }
    | Defparam				 { $1 }


Defparam :: {Stm }
    : defparam var_id "." var_id "=" Expr { Defparam $2 $4 $6 }

AssignStmt :: {Stm}
    : assign AttributeList Expr "=" Expr           { Assign $2 $3 $5 }

Range :: { Range }
     : "[" Expr ":" Expr "]"            {Range $2 $4 }
     | "[" Expr "]"                     {Index $2 }
     |                                  {EmptyRange }

{- {{{
Assign 
    : assign PortName "=" PortName ? PortName
    | dyn_assign

PortName :: {PortName}
    : var_id CRange
    | "{" PortNameList "}"

}}} -}

{-{{{
-- var's

CRange :: {CRange} 
    : "[" Constant ":" Constant "]"       { CRange $2 $4 }
    | "[" Constant "]"                    { CIndex $2    }
    |                                     { EmptyCRange  }
}}} -}


Expr :: { Expr }
    : var_id Range                   { E_Var $1 $2          }
    | Constant                       { E_Constant $1        }
    | Constant Range                 { E_ConstantRange $1 $2}
    | Expr "?" Expr ":" Expr         { E_BitOp $ BE_Select $1 $3 $5    }
    | "{" ExprList "}"               { E_Union $2           }
    | "~" Expr                       { E_BitOp (BE_Neg $2)    }
    | Expr "&" Expr                  { E_BitOp (BE_And $1 $3) }
    | Expr "|" Expr                  { E_BitOp (BE_Or  $1 $3) }

{- {{{
Expr :: { Expr }
    : var_id Range                   { E_Var $1 $2          }
    | Constant                       { E_Constant $1        }
    | Constant Range                 { E_ConstantRange $1 $2}
    | Expr "?" Expr ":" Expr         { E_Select $1 $3 $5    }
    | "{" ExprList "}"               { E_Union $2           }
    | "~" Expr                       { E_Neg $2             }
    | Expr "&" Expr                  { E_And $1 $3          }
    | Expr "|" Expr                  { E_Or  $1 $3          }
}}} -}

ExprList :: {[Expr]}
    : Expr                            { [$1] }
    | Expr "," ExprList               { $1 : $3 }
    |                                 { []   }


Constant :: {Const}
    : AnyDigit                              { C_Digit $1  }
    | string_t                              { C_String $1 }
    | "~" Constant                          { C_Neg $2    }
    | Constant "&" Constant                 { C_And $1 $3 }
    | Constant "|" Constant                 { C_Or $1 $3  }
    | Constant "?" Constant ":" Constant    { C_Select $1 $3 $5 }


SubcktStm :: {Stm}
    : var_id  var_id  FormalList          { Submodule $1 (Just $2) $3 }
    | var_id          FormalList          { Submodule $1 Nothing $2 }  

FormalList :: {SubmoduleFlist }
    : "(" PortMap ")"                 { $2 }

PortMap :: {SubmoduleFlist}
    : ExprList                           { SubmoduleFlist $1 }
    | ValueStrictList                    { SubmoduleFlistStrict $1 }


ValueStrictList :: {[(String,Expr)]}
    : ValueStrict                         { [$1] }
    | ValueStrict "," ValueStrictList     { $1 : $3 }
    |                                     { [] }

ValueStrict :: { (String,Expr)}
    : "." var_id "(" Expr ")"        { ($2,$4) }



{

-- happyError a = Left ("syntax error: " ++ show a)
happyError a = Left (show a)
-- happyError :: [TokenType] -> Either [TokenType] [TokenType]
-- happyError a = Left a

-- alexScanTokens :: String -> Either String [Token]


-- analyze_toks :: [Token] -> Either String [Module]
analyze_toks ts = syntax_parser simple_ts
    where
    simple_ts = map (\(Token _ _ _ _ t) -> t) ts    


}

