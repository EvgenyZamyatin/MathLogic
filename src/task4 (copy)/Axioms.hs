module Axioms where
import Parser
import Expression
import Matcher
import qualified Data.Map as H
import qualified Data.Set as S

axiomList = map parse [   "#A -> #B -> #A"
                        , "(#A -> #B) -> (#A -> #B -> #C) -> (#A -> #C)"
                        , "#A -> #B -> #A & #B"
                        , "#A & #B -> #A"
                        , "#A & #B -> #B"
                        , "#A -> #A | #B"
                        , "#B -> #A | #B"
                        , "(#A -> #C) -> (#B -> #C) -> (#A | #B -> #C)"
                        , "(#A -> #B) -> (#A -> !#B) -> !#A"
                        , "!!#A -> #A"
                        , "a=b->a'=b'"
                        , "a=b->a=c->b=c"
                        , "a'=b'->a=b"
                        , "!a'=0"
                        , "a+b'=(a+b)'"
                        , "a+0=a"
                        , "a*0=0"
                        , "a*b'=a*b+a"
                      ]


isTenthAxiom :: Exp -> Error
isTenthAxiom (Impl (Each s a) b) = 
  let t = match' H.empty S.empty a b
    in case t of 
      (f, NoError)  -> if (((H.size f) == 1) && (H.member s f)) || (H.size f == 0) then NoError else SomeError
      (_, s) -> s 
isTenthAxiom _ = SomeError

isEleventhAxiom :: Exp -> Error
isEleventhAxiom (Impl a (Exist s b)) = 
  let t = match' H.empty S.empty b a
    in case t of 
      (f, NoError)  -> if (((H.size f) == 1) && (H.member s f)) || (H.size f == 0) then NoError else SomeError
      (_, s) -> s 
isEleventhAxiom _ = SomeError

isTwentythAxiom (Impl (And a (Each x (Impl b' c))) b) = 
  let t1 = match' H.empty S.empty b a
    in let t2 = match' H.empty S.empty b c
      in case (t1, t2) of 
        ((f, NoError), (g, NoError)) -> 
          if b' == b && ((((H.size f) == 1) && (H.lookup x f == Just (show Zero))) || (H.size f == 0)) &&
            ((((H.size g) == 1) && (H.lookup x g == Just (show (Nxt (Var x))))) || (H.size g == 0)) 
              then NoError else SomeError
        ((_,a),(_,b)) -> b
                      
											