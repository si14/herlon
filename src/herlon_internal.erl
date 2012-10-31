-module(herlon_internal).
-behaviour(gen_server).
-behaviour(nakaz_user).

-include_lib("nakaz/include/nakaz.hrl").

-include("herlon.hrl").

%% API
-export([start_link/0]).
-export([get_secret_qr/1,
         check_code/2]).

%% nakaz callbacks
-export([nakaz_check/1, nakaz_load/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {config :: #herlon_conf{}}).

%%% API

get_secret_qr(Params) ->
    [{config, Conf}] = ets:lookup(herlon, config),
    D = Conf#herlon_conf.defaults,
    Secret = case lists:keyfind(secret, 1, Params) of
                 {secret, S} -> S;
                 false       -> crypto:rand_bytes(Conf#herlon_conf.secret_size)
             end,
    Label = case lists:keyfind(label, 1, Params) of
                {label, L} -> L;
                false      -> D#defaults.label
            end,
    TileSize = case lists:keyfind(tile_size, 1, Params) of
                   {tile_size, T} -> T;
                   false          -> D#defaults.tile_size
               end,
    Margin = case lists:keyfind(margin, 1, Params) of
                 {margin, M} -> M;
                 false       -> D#defaults.margin
             end,
    get_secret_qr_full(Secret, Label, TileSize, Margin).

get_secret_qr_full(Secret, Label, TileSize, Margin) ->
    KeyUri = form_key_uri(Secret, Label),
    Qr = poolboy:transaction(
           qr_pool,
           fun(Worker) ->
                   herlon_qr_worker:render(Worker, KeyUri, TileSize, Margin)
           end),
    {ok, {Secret, Qr}}.

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
    self() ! init,
    {ok, #state{config=Conf}}.

handle_call({load_config, Conf}, _From, State) ->
    %% FIXME: edit pool size on config reload
    ets:insert(herlon, {config, Conf}),
    {reply, ok, State#state{config=Conf}};
handle_call(Request, _From, State) ->
    ?WARNING("Got unhandled call: ~p", [Request]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ?WARNING("Got unhandled cast: ~p", [Msg]),
    {noreply, State}.

handle_info(init, #state{config=C}=State) ->
    Dispatch = [{'_', [{[<<"qr">>], herlon_rest_qr, []},
                       {[<<"check">>], herlon_rest_check, []}]}],
    {ok, _HttpPid} = cowboy:start_http(http, 100,
                                       [{port, C#herlon_conf.port},
                                        {ip, C#herlon_conf.ip}],
                                       [{dispatch, Dispatch}]),
    {noreply, State};
handle_info(Info, State) ->
    ?WARNING("Got unhandled info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions

form_key_uri(Secret, Label) ->
    Base32Secret = base32:encode(Secret),
    IOList = io_lib:format("otpauth://totp/~s?secret=~s",
                           [Label, Base32Secret]),
    iolist_to_binary(IOList).
