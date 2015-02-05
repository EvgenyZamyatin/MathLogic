{
module Parser where
import Data.Char
import Prelude
import Expression
}

%name parser
%tokentype { Token }
%error { parseError }

%token
	var	{TokenVar $$}
	'&' {TokenAnd}
	'|' {TokenOr}
	'(' {TokenOB}
	')' {TokenCB}
	'!' {TokenNot}
	'->' {TokenImpl}


%right '->'
%left '&' '|'
%nonassoc '!'

%% 

A : B '->' A { Impl $1 $3 }
	| B { $1 }

B : B '|' C { Or $1 $3 }
	| C { $1 }

C : C '&' D { And $1 $3 }
	| D { $1 }

D : '!' D { Not $2 }
	| E { $1 }

E	: var { Var $1 }
	| '(' A ')' { $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token = 
				TokenVar String
      | TokenAnd
      | TokenOr
      | TokenNot
      | TokenImpl
      | TokenOB
      | TokenCB
 			deriving Show

isAlphaOrNumber c = or ((isAlpha c) : (isNumber c) : [])

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
      | isSpace c = lexer cs
      | isAlphaOrNumber c = lexAlpha (c:cs)
lexer ('&':cs) = TokenAnd : lexer cs
lexer ('|':cs) = TokenOr : lexer cs
lexer ('-':'>':cs) = TokenImpl : lexer cs
lexer ('!':cs) = TokenNot : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs

lexAlpha cs = TokenVar var : lexer rest
      where (var,rest) = span isAlphaOrNumber cs

parse = parser . lexer

}


