% torxaki_if:start_link(q_eqc). % returns Erlang process
% torxaki_if:call(put, [x, 3]).

%11> {ResScan, Tokens, _ } = erl_scan:string("eqc:start().").
%{ok,[{atom,1,eqc},
%     {':',1},
%     {atom,1,start},
%     {'(',1},
%     {')',1},
%     {dot,1}],
%    1}
%12> {Res, Term} = erl_parse:parse_exprs(Tokens).
%{ok,[{call,1,{remote,1,{atom,1,eqc},{atom,1,start}},[]}]}
%13> erl_eval:exprs(Term,  erl_eval:new_bindings()).
%{value,ok,[]}

% c(q_eqc), c(torxaki_if), c(socket_server).

-module(socket_server).
-compile(export_all).
-define(PORTNUM, 1234).

answer_request(Request) ->
	{_, Tokens, _ } = erl_scan:string(Request),
	{_, Term} = erl_parse:parse_exprs(Tokens),
	{value, Value, _} = erl_eval:exprs(Term,  erl_eval:new_bindings()),
	Value.

% {value, V, ...}
% {value, {ok, void}, ...}
% {value, {error, precondition_failed}, ...}

start() ->
       torxaki_if:start_link(q_eqc),
       spawn(?MODULE, loop_listen, [?PORTNUM]).

loop_listen(PortNum) ->
  case gen_tcp:listen(PortNum, [binary, {packet, 0}, {reuseaddr, true}, {active, false}]) of
    {ok, LSock} -> loop_accept(LSock);
    {error, Reason} -> io:format("Could not start the server "++atom_to_list(Reason)++"\n"),
                       exit(Reason)
  end.

loop_accept(Listen) ->
  case gen_tcp:accept(Listen) of
    {ok, Socket} ->
        HR = handle_requests(Socket),
        gen_tcp:close(Socket),
        case HR of
          terminate -> exit(normal);
          continue -> loop_accept(Listen)
        end;
    {error, _} -> loop_accept(Listen)
  end.

handle_requests(Socket) ->
    Request = gen_tcp:recv(Socket, 0),
    case Request of
        {ok, Packet} ->
             io:format("Received [~p]\n", [Packet]),
             case Packet of
                <<"close",_/binary>> -> io:format("closing socket\n"), continue;
                <<"terminate",_/binary>> -> io:format("closing socket, terminating\n"), terminate;
                _ -> lists:foreach(fun
			             (<<>>) -> ok;
		                     (X) ->
				     I = binary_to_list(X),
			     	     io:format("REQ: ~p~n", [I]),
			             Res = answer_request(I),
				     io:format("RES: ~p~n", [Res]),
				     case Res of
					     {ok,_SV} -> ok; % gen_tcp:send(Socket, io_lib:format("~p~n", [SV]));
				             _ ->
					     gen_tcp:send(Socket, io_lib:format("~p~n", [Res]))
				     end
		             end, string:split(Packet,"\n",all)),
                     handle_requests(Socket)
             end;
         {error, _} -> continue
    end.
