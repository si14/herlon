-module(herlon_internal).
-behaviour(gen_server).
-behaviour(nakaz_user).

-include_lib("nakaz/include/nakaz.hrl").

-include("herlon.hrl").

%% API
-export([start_link/0]).
-export([get_secret_qr/1, get_secret_qr/2, get_secret_qr/4,
         check_code/2]).

%% nakaz callbacks
-export([nakaz_check/1, nakaz_load/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%% API

get_secret_qr(Secret) ->
    [{config, Conf}] = ets:lookup(herlon, config),
    D = Conf#herlon_conf.defaults,
    get_secret_qr(Secret, D#defaults.label,
                  D#defaults.tile_size, D#defaults.margin).

get_secret_qr(Secret, Label) ->
    [{config, Conf}] = ets:lookup(herlon, config),
    D = Conf#herlon_conf.defaults,
    get_secret_qr(Secret, Label,
                  D#defaults.tile_size, D#defaults.margin).

get_secret_qr(Secret, Label, TileSize, Margin) ->
    KeyUri = form_key_uri(Secret, Label),
    poolboy:transaction(
      qr_pool,
      fun(Worker) ->
              herlon_qr_worker:render(Worker, KeyUri, TileSize, Margin)
      end).

check_code(Secret, Code) ->
    %% FIXME: rate limit
    poolboy:transaction(
      oath_pool,
      fun(Worker) ->
              herlon_oath_worker:check(Worker, Secret, Code)
      end).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%% nakaz callbacks

nakaz_check(_Conf) ->
    %% FIXME: actually check config
    ok.

nakaz_load(Conf) ->
    gen_server:call(?SERVER, {load_config, Conf}).

%%% gen_server callbacks
init([]) ->
    Conf = ?NAKAZ_USE(#herlon_conf{}),
    ets:new(herlon, [protected, named_table, {read_concurrency, true}]),
    ets:insert(herlon, {config, Conf}),
    {ok, #state{}}.

handle_call({load_config, Conf}, _From, State) ->
    %% FIXME: edit pool size on config reload
    ets:insert(herlon, {config, Conf}),
    {reply, ok, State};
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

form_key_uri(Label, Secret) ->
    Base32Secret = base32:encode(Secret),
    IOList = io_lib:format("otpauth://totp/~s?secret=~s",
                           [Label, Base32Secret]),
    iolist_to_binary(IOList).
