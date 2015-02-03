module Axioms where
import Parser

axiomList = map parse [ "A -> B -> A"
                        , "(A -> B) -> (A -> B -> C) -> (A -> C)"
                        , "A -> B -> A & B"
                        , "A & B -> A"
                        , "A & B -> B"
                        , "A -> A | B"
                        , "B -> A | B"
                        , "(A -> C) -> (B -> C) -> (A | B -> C)"
                        , "(A -> B) -> (A -> !B) -> !A"
                        , "!!A -> A"
                      ]
                      
											