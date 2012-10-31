-module(herlon_rest_qr).

-export([init/3]).
-export([allowed_methods/2,
         malformed_request/2,
         content_types_provided/2]).
-export([resp_json/2]).

-record(state, {params :: [{atom(), any()}]}).

%%% cowboy callbacks

init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.

%%% cowboy_rest callbacks

allowed_methods(Req, State) ->
    {[<<"HEAD">>, <<"GET">>], Req, State}.

malformed_request(Req, State) ->
    {QsVals, Req2} = cowboy_req:qs_vals(Req),
    case check_req(QsVals) of
        {ok, Params} ->
            NewState = #state{params=Params},
            {false, Req2, NewState};
        {error, Errors} ->
            ErrorJson = jiffy:encode({Errors}),
            Req3 = cowboy_req:set_resp_body(ErrorJson, Req2),
            {true, Req3, State}
    end.

content_types_provided(Req, State) ->
	{[
      %% TODO: somehow find a sane way to serve PNG directly
      {{<<"application">>, <<"json">>, []}, resp_json}
     ], Req, State}.

resp_json(Req, #state{params=BinKeys}=State) ->
    Params = [{try binary_to_existing_atom(K, utf8)
               catch error:badarg -> some_key
               end, V} || {K, V} <- BinKeys],
    {ok, {Secret, Qr}} = herlon:get_secret_qr(Params),
	Body = jiffy:encode({[{secret, base64:encode(Secret)},
                          {qr, base64:encode(Qr)}]}),
	{Body, Req, State}.

%%% Internal functions
check_req(QsVals) ->
    Base64Decode = fun(Bin) ->
                           case catch base64:decode(Bin) of
                               B when is_binary(B) -> {ok, B};
                               _                   -> stop
                           end
                   end,

    Rules = [{<<"secret">>, [{{func, Base64Decode},
                              <<"should be a valid base64 string">>}]},
             {<<"label">>, [{{max_size, 32},
                             <<"should be shorter than 32 symbols">>},
                            {{min_size, 4},
                             <<"should be longer than 4 symbols">>}]},
             {<<"tile_size">>, [{{convert, integer},
                                 <<"should be an integer">>},
                                {{'>=', 3},
                                 <<"should be bigger or equal than 3">>},
                                {{'<=', 50},
                                 <<"should be lesser or equal than 50">>}]},
             {<<"margin">>, [{{convert, integer},
                              <<"should be an integer">>},
                             {{'>=', 0},
                              <<"should be bigger or equal than 0">>},
                             {{'<=', 20},
                              <<"should be lesser or equal than 20">>}]}],

    deputy:check_proplist(QsVals, Rules, []).
