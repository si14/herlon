-module(herlon_qr_worker).
-behaviour(gen_server).

-include("herlon.hrl").

%% API
-export([start_link/1]).
-export([render/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%% API
render(Worker, Text, TileSize, Margin) ->
    gen_server:call(Worker, {render, Text, TileSize, Margin}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%% gen_server callbacks
init([]) ->
    {ok, #state{}}.

handle_call({render, Text, TileSize, Margin}, _From, State) ->
    CmdTemplate = "qrencode -o- -s ~p -m ~p '~s'",
    Cmd = lists:flatten(io_lib:format(CmdTemplate, [TileSize, Margin, Text])),
    Port = open_port({spawn, Cmd},
                     [stream, eof,
                      exit_status, binary]),
    Data = receive
               {Port, {data, Bin}} -> Bin
           after 100 ->
                   timeout
           end,
    _End = receive
               {Port, {exit_status, Exit}} ->
                   Exit = 0 %% FIXME: better handling of nonzero exit
           after 100 ->
                   timeout
           end,
    {reply, Data, State};
handle_call(Request, _From, State) ->
    ?WARNING("Got unhandled call: ~p", [Request]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ?WARNING("Got unhandled cast: ~p", [Msg]),
    {noreply, State}.

handle_info({Port, eof}, State) when is_port(Port) ->
    {noreply, State};
handle_info(Info, State) ->
    ?WARNING("Got unhandled info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions
