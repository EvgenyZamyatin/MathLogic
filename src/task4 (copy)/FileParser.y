{
module FileParser where
import Data.Char
import Prelude
import Expression
}

%name fileParser
%tokentype { Token }
%error { parseError }

%token
	var {TVar $$}
	pr {TPr $$}
	'&' {TAnd}
	'|' {TOr}
	'(' {TOB}
	')' {TCB}
	'!' {TNot}
	'->' {TImpl}
	',' {TComma}
	'@' {TAll}
	'?' {TEx}
	'+' {TPlus}
	'*' {TMul}
	'0' {TZero}
	'\''{TNxt}
	'=' {TEq}
	'\n' {TEol}
	'|-' {TTrq}
	
%right '->'
%left '&' '|'
%nonassoc '!'

%% 

File : Header '\n' Proof {InFile (assumptions $1) (statement $1) $3}
		 | Header {InFile (assumptions $1) (statement $1) []}

Header : Expr ',' Header { appendAssumption $3 $1 }
			 | Expr '|-' Expr {InFile [$1] $3 []}

Proof : Expr '\n' Proof {$1 : $3}
			| Expr {[$1]}

Expr : D {$1}
		 | D '->' Expr {Impl $1 $3}

D : K {$1}
	| D '|' K {Or $1 $3}

K : U {$1}
	| K '&' U {And $1 $3}

U : P {$1}
	| '!' U {Not $2}
	| '(' Expr ')' {$2}
	| '@' var U {Each $2 $3}
	| '?' var U {Exist $2 $3}

P : pr '(' TL ')' {SomePr $1 $3}
	| T '=' T {EqPr $1 $3}

TL : T {[$1]}
	 | T ',' TL {$1 : $3}

T : S {$1}
	| T '+' S {Sum $1 $3}

S : M {$1}
	| S '*' M {Mul $1 $3}

M : 
	var {Var $1}
	| '(' T ')' {$2}
	| '0' {Zero}
	| M '\'' {Nxt $1}
	| var '(' TL ')' {Fun $1 $3}

{
parseError :: [Token] -> a
parseError (s:xs) = error ("Parse error on token " ++ (show s))
parseError [] = error ("Parse error on token at the end")

data Token = 
				TVar String
      | TAnd
      | TOr
      | TNot
      | TImpl
      | TOB
      | TCB
      | TEol
      | TComma
      | TTrq
      | TAll
      | TEx
      | TPlus
      | TMul
      | TZero
      | TNxt
      | TEq
      | TPr String
 			deriving Show

isLowerAlphaOrNumber c = ((isAlpha c) && (isLower c)) || (isNumber c)
isUpperAlphaOrNumber c = ((isAlpha c) && (isUpper c)) || (isNumber c)

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
      | isSpace c = lexer cs
      | (isUpperAlphaOrNumber c) = lexPred (c:cs)
      | (isLowerAlphaOrNumber c) = lexAlpha (c:cs)
lexer ('&':cs) = TAnd : lexer cs
lexer ('|':'-':cs) = TTrq : lexer cs
lexer ('|':cs) = TOr : lexer cs
lexer ('-':'>':cs) = TImpl : lexer cs
lexer ('!':cs) = TNot : lexer cs
lexer ('(':cs) = TOB : lexer cs
lexer (')':cs) = TCB : lexer cs
lexer ('\n':cs) = TEol : lexer cs
lexer (',':cs) = TComma : lexer cs
lexer ('@':cs) = TAll : lexer cs
lexer ('?':cs) = TEx : lexer cs
lexer ('+':cs) = TPlus : lexer cs
lexer ('*':cs) = TMul : lexer cs
lexer ('0':cs) = TZero : lexer cs
lexer ('\'':cs) = TNxt : lexer cs
lexer ('=':cs) = TEq : lexer cs

lexAlpha cs = TVar var : lexer rest
      where (var,rest) = span isLowerAlphaOrNumber cs

lexPred cs = TPr var : lexer rest
      where (var,rest) = span isUpperAlphaOrNumber cs
      
parse = parser . lexer

}


