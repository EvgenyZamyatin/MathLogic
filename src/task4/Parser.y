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
	var {TVar $$}
	pr {TPr $$}
	tmp {TTmp $$}
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
	
%right '->'
%left '&' '|'
%nonassoc '!'

%% 

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
	| tmp {Template $1}

P : pr '(' TL ')' {SomePredicate $1 $3}
	| T '=' T {EqualPredicate $1 $3}
	| pr {SomePredicate $1 []}

TL : T {[$1]}
	 | T ',' TL {($1) : $3}

T : S {$1}
	| T '+' S {Sum $1 $3}

S : M {$1}
	| S '*' M {Mul $1 $3}

M : var {Var $1}
	| '(' T ')' {$2}
	| '0' {Zero}
	| M '\'' {Nxt $1}
	| var '(' TL ')' {Fun $1 $3}

{
parseError :: [Token] -> a
parseError [] = error ("Parse error on token at the end")
parseError (xs) = error ("Parse error on token " ++ (show xs))

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
      | TAll
      | TEx
      | TPlus
      | TMul
      | TZero
      | TNxt
      | TEq
      | TPr String
      | TTmp String
      | TOSB
      | TCSB
      | TAs
 			deriving Show

isLowerAlphaOrNumber c = ((isAlpha c) && (isLower c)) || (isNumber c)
isUpperAlphaOrNumber c = ((isAlpha c) && (isUpper c)) || (isNumber c)
isAlphaOrNumber c = ((isAlpha c)) || (isNumber c)


lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
      | isSpace c = lexer cs
      | ((isUpper c) && isAlpha c) = lexPred (c:cs)
      | ((isLower c) && isAlpha c) = lexAlpha (c:cs)
lexer ('&':cs) = TAnd : lexer cs
lexer ('|':'-':cs) = parseError []
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
lexer ('#':cs) = (lexTmp cs)
lexer ('0':cs) = TZero : lexer cs

lexAlpha cs = TVar var : lexer rest
      where (var,rest) = span isLowerAlphaOrNumber cs

lexTmp cs = TTmp var : lexer rest
      where (var,rest) = span isAlphaOrNumber cs

lexPred cs = TPr var : lexer rest
      where (var,rest) = span isUpperAlphaOrNumber cs
      
parse = parser . lexer

}


