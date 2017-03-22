%%%
%%%	@doc
%%%	wxWidgets application in Erlang
%%%	Author: Luke Taylor
%%%
%%%	This is the highest level supervisor in the application.
%%%	@end
%%%

-module(windoze_sup).
-behaviour(supervisor).
-define(SERVER, ?MODULE).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% windoze_sup
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%	@doc API functions
-export([start_link/0]).

%%%	Supervisor callbacks
-export([init/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API functions

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->

	%%% List of ChildSpecs for all workers under this sup
	ChildSpecs = [
		worker(window_mgr)
		],

	ok = supervisor:check_childspecs(ChildSpecs),


	%%% Restart strategy and auto-shutdown parameters
	SupFlags = #{strategy => one_for_one,
	 	 		intensity => 1,
	 	 		   period => 5},

	{ok, {SupFlags, ChildSpecs}}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal functions

worker(Module) ->
		#{id => Module,
	   start => {Module, start_link, []},
	 restart => permanent,
	shutdown => 2000,
		type => worker,
	 modules => [Module]}.