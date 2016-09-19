-module(test).
-export([main/0]).

main() ->
    assert(sexp:eval("(+ 5 6)"), 11),
    assert(sexp:eval("(* 345 (/ 56 (+ 3 4)))"), 2760),
    assert(sexp:eval("(/ -7 (- (+ 567 34) 9))"), -0.011824324324324325).

assert(_Res, _Expected) when _Res == _Expected ->
    io:format("Passed.~n"),
    ok;
assert(Res, Expected) when Res /= Expected ->
    io:format("Error occured in assertion.~n Result: ~p. Expected: ~p.~n", [Res, Expected]),
    error.
