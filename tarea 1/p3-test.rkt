
#lang play

(require "p3.rkt")
(require "p2-test.rkt")

(test (run '{{define {positive {x : Num}} {< 0 x}}
             {define {sub {x : Num @ positive} {y : Num}} : Num
               {+ y x}}
             {sub 5 3}})
      (numV 8))
(test/exn (run '{{define {positive {x : Num}} {< 0 x}}
             {define {sub {x : Num @ positive} {y : Num}} : Num
               {+ y x}}
             {sub -100 3}})
      "Runtime contract error: (num -100) does not satisfy positive")

(test (run '{{define {positive {x : Num}} {< 0 x}}
             {define {negate {x : Num @ positive}} {+ -100 x}}
             {negate 23}})
      (numV -77))

(test/exn (run '{{define {pair-non-zero? {p : {Pair Num Num}}} {! {|| {= 0 {fst p}} {= 0 {snd p}}}}}
                 {define {pair-div {p : {Pair Num Num} @ pair-non-zero?}} {+ {fst p} {snd p}}}
                 {+ {pair-div {cons 30 5}} {pair-div {cons 60 0}}}
                 })
          "Runtime contract error: (my-cons (num 60) (num 0)) does not satisfy pair-non-zero?")

(test/exn (run '{{define {add {x : Num} {y : Num}} {+ x y}}
                 {define {oh-no {x : Num @ add}} x}
                 {oh-no 21}})
          "Static contract error: invalid type for add")

(test/exn (run '{{define {add {x : Num}} {+ x 3}}
                 {define {oh-no {x : Num @ add}} x}
                 {oh-no 21}})
          "Static contract error: invalid type for add")

(test/exn (run '{{define {add {x : Bool}} {! x}}
                 {define {oh-no {x : Bool @ add}} x}
                 {oh-no #t}})
          "Runtime contract error: (bool #t) does not satisfy add")

(test (run '{{define {add {x : Bool}} {! x}}
                 {define {oh-no {x : Bool @ add}} x}
                 {oh-no #f}})
          (boolV #f))
