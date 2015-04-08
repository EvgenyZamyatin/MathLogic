{
module Parser where
import Data.Char
import Prelude
import OrdinalCalculator

}

%name parser
%tokentype { Token }
%error { parseError }

%token
	const	{TokenConst $$}
	'+' {TokenPlus}
	'*' {TokenMul}
	'^' {TokenPow}
	')' {TokenCB}
	'(' {TokenOB}
	'=' {TokenEq}

%right '^'
%left '*' '+'

%% 

A : B '=' B { O ($1, 1) (O ($3, 1) (A 0))}

B : B '+' C { addOrd $1 $3 }
	| C { $1 }

C : C '*' D { mulOrd $1 $3 }
	| D { $1 }

D : E '^' D { expOrd $1 $3 }
	| E { $1 }

E	: const { str2Ord $1 }
	| '(' B ')' { $2 }

{
parseError :: [Token] -> a
parseError s = error "Parse error: "

data Token = 
				TokenConst String
      | TokenPlus
      | TokenMul
      | TokenPow
      | TokenEq
      | TokenOB
      | TokenCB
 			deriving Show

isAlphaOrNumber c = or ((isAlpha c) : (isNumber c) : [])

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
      | isSpace c = lexer cs
      | isAlphaOrNumber c = lexAlpha (c:cs)
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('*':cs) = TokenMul : lexer cs
lexer ('^':cs) = TokenPow : lexer cs
lexer ('=':cs) = TokenEq : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs

lexAlpha cs = TokenConst cst : lexer rest
      where (cst,rest) = span isAlphaOrNumber cs

parse = parser . lexer

}


