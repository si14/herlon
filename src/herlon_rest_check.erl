-module(herlon_rest_check).

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
      {{<<"application">>, <<"json">>, []}, resp_json}
     ], Req, State}.

resp_json(Req, #state{params=Params}=State) ->
    {_, Secret} = lists:keyfind(<<"secret">>, 1, Params),
    {_, Code} = lists:keyfind(<<"code">>, 1, Params),
    Result = herlon:check_code(Secret, Code),
	Body = jiffy:encode({[{result, Result}]}),
	{Body, Req, State}.

%%% Internal functions
check_req(QsVals) ->
    Base64Decode = fun(Bin) ->
                           case catch base64:decode(Bin) of
                               B when is_binary(B) -> {ok, B};
                               _                   -> stop
                           end
                   end,

    Rules = [{<<"secret">>, [{required,
                              <<"should be provided">>},
                             {{func, Base64Decode},
                              <<"should be a valid base64 string">>}]},
             {<<"code">>, [{required,
                            <<"should be provided">>},
                           {{convert, integer},
                            <<"should be an integer">>},
                           {{'>=', 0},
                            <<"should be bigger or equal than 0">>},
                           {{'=<', 999999},
                            <<"should be lesser or equal than 999999">>}]}],

    deputy:check_proplist(QsVals, Rules, []).
