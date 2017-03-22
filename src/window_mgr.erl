%%%
%%%	@doc
%%%	wxWidgets application in Erlang
%%%	Author: Luke Taylor
%%%
%%%	This gen_server is a window manager for the application.
%%%	@end
%%%

-module(window_mgr).
-behaviour(gen_server).
-define(SERVER, ?MODULE).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% window_mgr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%	@doc External API functions
-export([
	start_link/0,
	stop/0
	]).
%%%	@doc gen_server callback modules
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
	]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc API functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, no_parameters, []).

stop() ->
	%%%	Stop server asynchronously
	gen_server:cast(?MODULE, shutdown).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc gen_server callback modules


init(no_parameters) ->
	Wx = wx:new(),
	Frame = draw_frame(Wx, "Hello, World!"),
	decorate(Frame),
	add_events(Wx, Frame),
	wxFrame:show(Frame),
	show_modal(Frame, "Ready to go!"),
    %{ok, {Wx, Frame}}.
    {ok, Frame}.

handle_call(Message, From, Frame) ->
	io:format("Generic call handler: '~p' from '~p' while in '~p'~n", [Message, From, Frame]),
	{reply, ok, Frame}.
	%% Synchronous, possible return values
	%% {reply, Reply, NewFrame} 			| {reply, Reply, NewFrame, Timeout} |
	%% {reply, Reply, NewFrame, hibernate} 	| {noreply,NewFrame} 				|
	%% {noreply, NewFrame, Timeout} 		| {noreply, NewFrame, hibernate} 	|
	%% {stop, Reason, Reply, NewFrame} 		| {stop,Reason,NewFrame}

%%% normal termination clause
handle_cast(shutdown, Frame) ->
	wxFrame:destroy(Frame),
	{stop, by_request, Frame};
%%%	generic async handler
handle_cast(Message, Frame) ->
	io:format("Generic cast handler: '~p' while in '~p'~n", [Message, Frame]),
	{noreply, Frame}.
	%% Asynchronous, possible return values
	%% {noreply, NewFrame} 				| {noreply, NewFrame, Timeout} |
	%% {noreply, NewFrame, hibernate} 	| {stop, Reason, NewFrame}

handle_info(_Message, _Frame) ->
	io:format("Generic info handler: '~p' '~p'~n", [_Message, _Frame]),
	{noreply, _Frame}.
	%% Informative calls - possible return values
	%% {noreply, NewFrame} 		| {noreply, NewFrame, Timeout} | {noreply, NewFrame, hibernate} |
	%% {stop, Reason, NewFrame} | {stop, Reason, NewFrame}

terminate(_Reason, _Frame) ->
	io:format("Generic termination handler: '~p' '~p'~n", [_Reason, _Frame]),
	ok.

code_change(_OldVersion, Frame, _Extra) ->
	{ok, Frame}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

draw_frame(Wx, Title) ->
	wxFrame:new(Wx, -1, Title).

%%%	@doc This method adds menus and other items to the frame
decorate(Frame) ->
	MenuBar = wxMenuBar:new(),
	wxFrame:setMenuBar (Frame, MenuBar),

	FileMn = wxMenu:new(),
	wxMenuBar:append (MenuBar, FileMn,"&File"),

	Quit = wxMenuItem:new ([{id,400},{text, "&Quit"}]),
	wxMenu:append (FileMn, Quit),

	HelpMn = wxMenu:new (),
	wxMenuBar:append (MenuBar, HelpMn, "&Help"),

	About = wxMenuItem:new ([{id,500},{text,"About"}]),
	wxMenu:append (HelpMn, About).


%%%	Attach callbacks to events in the window
add_events(Wx, Frame) ->
	{WxRef, _Num, NewWx, []} = Wx,
	wxFrame:connect (Frame, command_menu_selected),

	Ding = fun (_,_) -> io:format("WORKED~n") end,

	%Ding(#wx{},#wx_ref{}),
	%Ding(),
	wxFrame:connect (Frame, close_window, [{callback, Ding}]).


show_modal(Frame, Msg) ->
	D = wxMessageDialog:new(Frame, Msg),
	wxMessageDialog:showModal(D).