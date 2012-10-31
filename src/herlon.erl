-module(herlon).
-behaviour(application).
-compile({parse_transform, nakaz_pt}).
-include_lib("nakaz/include/nakaz.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("herlon.hrl").

%% API
-export([start/0,
         get_secret_qr/0, get_secret_qr/1,
         check_code/2,
         add_user/1, check_user/2]).

%% Application callbacks
-export([start/2, stop/1]).

%% Application callbacks
start(_StartType, _StartArgs) ->
    case ?NAKAZ_ENSURE([#herlon_conf{}]) of
        ok ->
            herlon_sup:start_link();
        {error, Msg}=Err ->
            io:format(Msg),
            Err
    end.

stop(_State) ->
    ok.

%% API

start() ->
    ok = application:start(ranch),
    ok = application:start(crypto),
    ok = application:start(cowboy),
    ok = application:start(poolboy),
    ok = application:start(nakaz),
    ok = application:start(herlon).

%% if secret is stored on external server

-spec get_secret_qr() -> {ok, {binary(), binary()}}.
get_secret_qr() ->
    herlon_internal:get_secret_qr([]).

-spec get_secret_qr([{atom(),
                      binary() | integer()}]) -> {ok, {binary(), binary()}}.
get_secret_qr(Params) ->
    herlon_internal:get_secret_qr(Params).

-spec check_code(binary(), pos_integer()) -> boolean().
check_code(Secret, Code) -> % configure time window
    herlon_internal:check_code(Secret, Code).

%% if secret is stored inside herlon

add_user(_UserId) -> % secret is stored internally
    png_binary_or_secret_or_both. % based on content-type-accepted

check_user(_UserId, _Code) ->
    true_or_false_or_signed_request.

%% TODO: try poolboy+jobs+"try later" combo to regulate load
%% -rate + actual (pool of blocking workers) load regulation
%% -identify caller (!!!)
%% TODO: try ot build a scaffold with nakaz

-ifdef(TEST).



-endif.
