
-module(herlon_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% Somewhat reasonable default for poolboy max_overflow
-define(MAX_OVERFLOW(X), X).

%% API functions
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Supervisor callbacks
init([]) ->
    QrPool = poolboy:child_spec(qr_pool,
                                [{name, {local, qr_pool}},
                                 {worker_module, herlon_qr_worker},
                                 {size, 5}, %% get this from config
                                 {max_overflow, ?MAX_OVERFLOW(5)}],
                                []),
    OathPool = poolboy:child_spec(oath_pool,
                                  [{name, {local, oath_pool}},
                                   {worker_module, herlon_oath_worker},
                                   {size, 5}, %% get this from config
                                   {max_overflow, ?MAX_OVERFLOW(5)}],
                                  []),
    {ok, {{one_for_one, 5, 10}, [QrPool, OathPool,
                                 ?CHILD(herlon_internal, worker)]}}.
