-module(sexp).
-export([main/]).
-compile(export_all).

main() ->
    Sexp = "(* 14 (- 45 (/ 5 6)))",
    eval(Sexp).

eval(Sexp) ->
    Stoks = tokenize(Sexp),
    Res = eval_action(transform(tl(lists:reverse(tl(lists:reverse(Stoks)))))),
    Res.

tokenize(Sexp) ->
    Lb = re:replace(Sexp, "\\(", " \\( ", [global, {return, list}]),
    Rb = re:replace(Lb, "\\)", " \\) ", [global, {return, list}]),
    Spl = re:split(string:strip(Rb), " ", [{return, list}]),
    lists:filter(fun(X) -> X /= [] end, Spl).

%% Accumulator
transform(List) ->
    transform(List, []).

transform([], Acc) ->
    lists:reverse(Acc);
transform(["("|T], Acc) ->
    transform(T, {[],Acc});
transform([")"|T], {L,{L2,Acc}}) ->
    transform(T, {[lists:reverse(L)|L2],Acc});
transform([")"|T], {L,Acc}) ->
    transform(T, [lists:reverse(L)|Acc]);
transform([H|T], {L,Acc}) ->
    transform(T, {[H|L],Acc});
transform([H|T], Acc) ->
    transform(T, [H|Acc]).

%% Evaluator
eval_action(Int) when is_number(Int) ->
    Int;
eval_action(List) when is_list(List) ->
    [Sign|Ints] = List,
    eval_action(operations(Sign), to_number({from, list}, Ints)).

eval_action(multiple, [Hd|Tl]) ->
    lists:foldl(fun(X, Acc) -> eval_action(Acc) * eval_action(X) end, Hd, Tl);
eval_action(divide, [Hd|Tl]) ->
    lists:foldl(fun(X, Acc) -> eval_action(Acc) / eval_action(X) end, Hd, Tl);
eval_action(plus, [Hd|Tl]) ->
    lists:foldl(fun(X, Acc) -> eval_action(Acc) + eval_action(X) end, Hd, Tl);
eval_action(minus, [Hd|Tl]) ->
    lists:foldl(fun(X, Acc) -> eval_action(Acc) - eval_action(X) end, Hd, Tl).

operations(Sign) ->
    Operations = dict:from_list([{"+", plus}, {"-", minus}, {"*", multiple}, {"/", divide}]),
    dict:fetch(Sign, Operations).

to_number({from, list}, Args) -> 
    [to_number(X) || X <- Args].

to_number(Arg) ->
    case string:to_float(Arg) of
	{error, no_float} ->
	    case string:to_integer(Arg) of
		{error, no_integer} -> Arg;
		{Num, _} -> Num
	    end;
	{Num, _} -> 
	    Num
    end.

