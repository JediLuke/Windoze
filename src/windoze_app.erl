%%%
%%%	@doc
%%%	wxWidgets application in Erlang
%%%	Author: Luke Taylor
%%%	@end
%%%

-module(windoze_app).
-behaviour(application).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc windoze_app
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-export([start/2, stop/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc API functions
%%% @doc Application callbacks

start(_StartType, _StartArgs) ->
	print_welcome_msg(),
	windoze_sup:start_link().

stop(_State) ->
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Internal functions

print_welcome_msg() ->
	io:format("Starting windoze application.~n").