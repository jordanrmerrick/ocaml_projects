# 1 "book/duniverse/result.1.4/result-as-alias.ml"
type nonrec ('a, 'b) result = ('a, 'b) result = Ok of 'a | Error of 'b
