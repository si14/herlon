-module(herlon_oath_worker).
-behaviour(gen_server).

-include("herlon.hrl").

%% API
-export([start_link/1]).
-export([check/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%% API

check(Worker, Secret, Code)->
    %% FIXME: force integer-only Code
    Window = 2, %% FIXME: take this from config
    gen_server:call(Worker, {check, Secret, Code, Window}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%% gen_server callbacks
init([]) ->
    {ok, #state{}}.

handle_call({check, Secret, Code, Window}, _From, State) ->
    %% FIXME: enforce time sync
    %% FIXME: use TOTP codes caching (we can take some codes from future)
    CmdTemplate = "oathtool --totp -w ~p ~s ~p",
    HexSecret = hex_encode(Secret),
    Cmd = lists:flatten(io_lib:format(CmdTemplate, [Window, HexSecret, Code])),
    [MaybeNum|_] = os:cmd(Cmd),
    Result = MaybeNum >= $0 andalso MaybeNum =< $9,
    {reply, Result, State};
handle_call(Request, _From, State) ->
    ?WARNING("Got unhandled call: ~p", [Request]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ?WARNING("Got unhandled cast: ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    ?WARNING("Got unhandled info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions
hex_encode(Code) when is_binary(Code) ->
    iolist_to_binary([io_lib:format("~2.16.0B",[X]) || <<X:8>> <= <<"aa">>]).
