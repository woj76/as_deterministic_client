-module(q_eqc).
-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-record(state, {ptr, size, elements, puts=0}).

initial_state() ->
  #state{}.


%% --- generators ----
positive() ->
  ?SUCHTHAT(X, nat(), X >= 1).

%% --- commands ----

new(Size) ->
  q:new(Size).

new_args(_S) -> [ positive() ].

new_next(_S, Ptr, [Size]) ->
  #state{ptr=Ptr, size=Size, elements=[]}.

put(Ptr, Val) ->
  q:put(Ptr, Val).

put_args(S) -> [S#state.ptr, int()].

put_pre(S) ->
  length(S#state.elements) < S#state.size.

put_next(S,_V,[_,X]) ->
  S#state{elements = S#state.elements ++ [X],
          puts = S#state.puts + 1}.

put_features(S, _Args, _Res) ->
  [ {wrap, S#state.puts == S#state.size} ].


get(Ptr) ->
  q:get(Ptr).

get_args(S) -> [S#state.ptr].

get_pre(S) ->
  S#state.elements /= [].

get_next(S,_V,[_]) ->
  S#state{elements=tl(S#state.elements)}.

get_post(S, Args, R) ->
  lists:member(R, get_as(S, Args)).

get_as(S, _Args) ->
    [ R || R <- S#state.elements,
           R == hd(S#state.elements)].

get_res(S, [_]) ->
    hd(S#state.elements).


size(Ptr) ->
  q:size(Ptr).

size_args(S) ->
  [S#state.ptr].

size_post(S, [_], Res) ->
  eq(Res,length(S#state.elements)).

size_res(S, [_]) ->
    length(S#state.elements).

prop_q() ->
  eqc:dont_print_counterexample(
    ?FORALL(Cmds, commands(?MODULE),
	    begin
              {H,S,Res} = run_commands(Cmds),
              statistics({H, S, Res}, Cmds, Res==ok)
	    end)).

start() ->
  eqc_c:start(q).

%% -- additional callbacks to have fun ----
%% -- or, better, to improve the model ----

new_shape([N]) -> [N].

put_shape([_,_]) -> ['_', '_'].

% command_precondition_common(S, Cmd) ->
%  (Cmd == new andalso S#state.ptr == undefined)
%    orelse (Cmd /= new andalso S#state.ptr /= undefined).

command_precondition_common(S, Cmd) ->
  % io:format("PTR: ~p~n",[S#state.ptr]),
  (Cmd == new orelse S#state.ptr /= undefined).

statistics({H, S, Res}, Cmds, Prop) ->
  aggregate(command_names(Cmds),
  features(call_features(H),
  aggregate(call_features(H),
  pretty_commands( ?MODULE, Cmds, {H,S,Res}, Prop)))).

suite() ->
  eqc_suite:feature_based(q_eqc:prop_q()).

bugs() ->
  eqc_statem:print_bugs(more_bugs(eqc:testing_time(10, prop_q()), 20, [])).

weight(_S, put) ->
  10;
weight(_S, _) ->
  1.
