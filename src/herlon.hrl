-define(WARNING(STR, ARGS), io:format(STR, ARGS)).

-type filename() :: string().

-record(net_details, {ip   :: inet:ip_address(),
                      port :: 70..90}).

-record(srv_conf, {iface     :: #net_details{},
                   hostname  :: string(),
                   header    :: binary(),
                   conn_type :: http | ssl}).

-record(log_conf, {log :: filename(),
                   severity :: debug | info | error}).

-record(defaults,
        {label     :: binary(),
         margin    :: 0..20,
         tile_size :: 3..50}).

-record(herlon_conf,
        {defaults              :: #defaults{},
         qr_pool_size          :: 1..50,
         oath_pool_size        :: 1..50,
         code_check_rate_limit :: 1..60 % per minute
        }).
