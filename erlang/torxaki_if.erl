%%%-------------------------------------------------------------------
%%% @author Thomas Arts <thomas@SpaceGrey.local>
%%% @copyright (C) 2022, Thomas Arts
%%% @doc Simulation of torxaki tests against QuickCheck model
%%%
%%% @end
%%% Created : 22 Jun 2022 by Thomas Arts <thomas@SpaceGrey.local>
%%%-------------------------------------------------------------------
-module(torxaki_if).

-behaviour(gen_server).

%% API
-export([start_link/1,
         call/2, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, format_status/2]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================


start_link(EqcModel) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, EqcModel, []).

call(Fun, Args) ->
    gen_server:call(?SERVER, {call, Fun, Args}).

stop() ->
    gen_server:stop(?SERVER).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init(EqcModel) ->
    process_flag(trap_exit, true),
    {ok, #{eqc_state => EqcModel:initial_state(),
           model => EqcModel,
           counter => 0}}.

handle_call({call, Fun, Args} , _From, #{eqc_state := S,
                                         model := M,
                                         counter := Counter} = State) ->
    case M:precondition(S, {call, M, Fun, Args}) of
        false ->
            {reply, {error, precondition_failed}, State};
        true ->
            NextS = M:next_state(S, {var, Counter+1}, {call, M, Fun, Args}),
            Reply =
                try result(M, Fun, [S, Args])
                catch _:_ ->
                        {ok, list_to_atom(lists:flatten(io_lib:format("symvar_~p",[Counter+1])))}
                end,
            {reply, Reply, State#{eqc_state => NextS, counter => Counter+1}}
    end.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.


format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================


result(M, Atom, Args) ->
    F = list_to_atom(lists:concat([Atom, "_res"])),
    apply(M, F, Args).
