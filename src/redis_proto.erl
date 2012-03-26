-module(redis_proto).
-export([encode/1,decode/1]).
-define(CRLF,<<"\r\n">>).
-define(NIL,nil).


%% redis_proto:encode(["SET","mykey", "myvalue"])
encode(IoList)->
    [<<"*">>,to_iolist(len(IoList)),?CRLF] ++ [[ <<"$">>,to_iolist(len(X)),?CRLF,to_iolist(X),?CRLF] ||X <- IoList]
        .


%% redis_proto:decode(<<"+ok\r\n">>).
%% redis_proto:decode(<<"-ERRRRRRRRR\r\n">>).
%% redis_proto:decode(<<":233\r\n">>).

%% redis_proto:decode([<<"$4\r\n">>, <<"ssss\r\n">>]).
%% redis_proto:decode(<<"$4\r\nssss\r\n">>).

%% redis_proto:decode([<<"*2\r\n">>,<<"$4\r\n">>,<<"cccc\r\n">>,<<"$4\r\n">>,<<"ddd\r\n">>]).
%% redis_proto:decode(<<"*2\r\n$4\r\ncccc\r\n$4\r\nddd\r\n">>).
decode(<<"+",SingleLineRest/binary>>=Bin)when is_binary(Bin)->
    parse_status_reply(SingleLineRest) ;
decode(<<"-",ErrorRest/binary>> =Bin) when is_binary(Bin)->
    parse_error_reply(ErrorRest) ;
decode(<<":",IntegerRest/binary>> = Bin)when is_binary(Bin)->
    decode_integer_reply(IntegerRest);

decode(<<"$-1",_BulkRest/binary>> = Bin) when is_binary(Bin)->
    ?NIL;
decode(<<"$",BulkRest/binary>> = Bin) when is_binary(Bin)->
    decode_bulk_reply(binary:split(BulkRest,?CRLF,[global,trim]));
decode([<<"$-1",_BulkRest/binary>> |_Rest]= List) when is_list(List)->
    ?NIL;
decode([<<"$",_BulkRest/binary>> |_Rest]= List) when is_list(List)->
    decode_bulk_reply(List);

decode(<<"*0\r\n">>)  ->
    [];
decode([<<"*0\r\n">> |_Rest]) ->
    [];
decode(<<"*-1\r\n">> ) ->
    ?NIL;
decode([<<"*-1\r\n">> |_Rest]) ->
    ?NIL;
decode(<<"*",MultiBulkRest/binary>> = Bin ) when is_binary(Bin)->
    %% todo:
    decode_multi_bulk_reply(binary:split(MultiBulkRest,?CRLF,[global,trim]));
decode([<<"*",_MultiBulkRest/binary>> |_Rest]=Bins) ->
    decode_multi_bulk_reply(Bins).

%% redis_proto:decode([<<"$23\r\n">>, <<"ssss\r\n">>]).
%% redis_proto:decode(<<"$4\r\n3333\r\n">>).
decode_bulk_reply(BulkRestBin)when is_binary(BulkRestBin)->
    [_Len,Data]=binary:split(BulkRestBin,?CRLF,[global,trim]),
    Data ;
decode_bulk_reply([BulkRestBin1,BulkRestBin2])when is_binary(BulkRestBin2),is_binary(BulkRestBin1)->
    [Data]=binary:split(BulkRestBin2,?CRLF,[global,trim]),
    Data .

%% redis_proto:decode([<<"*2\r\n">>,<<"$4\r\n">>,<<"cccc\r\n">>,<<"$4\r\n">>,<<"ddd\r\n">>]).
%% redis_proto:decode([<<"*2\r\n">>,<<"$-1\r\n">>,<<"$4\r\n">>,<<"ddd\r\n">>]).
decode_multi_bulk_reply([_CountBin|TailBinList]=_BinList)->
    {Result,_Bool}=lists:foldl(fun(X,{Acc0,Bool})->
                                       case Bool of
                                           true->
                                               {[X|Acc0],false};
                                           false->
                                               case X of
                                                   <<"$-1\r\n">>->
                                                       {[?NIL|Acc0],false};
                                                   _->
                                                       {Acc0,true}
                                               end

                                       end
                               end,{[],false},TailBinList),
    lists:map(fun(X) when is_binary(X)->trim_crlf(X);(X)->X end,lists:reverse(Result))
        .


decode_integer_reply(IntegerRest)when is_binary(IntegerRest)->
    IntBin = trim_crlf(IntegerRest),
    list_to_integer(binary_to_list(IntBin))
        .

%% parse_status_reply(<<"OK\r\n">>) ->
%%     ok;
%% parse_status_reply(<<"QUEUED\r\n">>) ->
%%     queued;
%% parse_status_reply(<<"PONG\r\n">>) ->
%%     pong;
%% parse_status_reply(<<"none\r\n">>) ->
%%     none;
%% parse_status_reply(<<"string\r\n">>) ->
%%     string;
%% parse_status_reply(<<"list\r\n">>) ->
%%     list;
%% parse_status_reply(<<"set\r\n">>) ->
%%     set;
parse_status_reply(Status) ->
    trim_crlf(Status).

parse_error_reply(RestBin)when is_binary(RestBin)->
    {error, trim_crlf(RestBin)}.

%% decode(IoList)->

%%     .

%%-----------------------------------------------------------------------------
%% @doc
%% return the length of a string,a list ,a Bin ,or a int
%% @spec len(L)::iolist(), ) ->
%%       integer()
%% @end
%%-----------------------------------------------------------------------------
-spec len(iolist()) -> integer().

len(L) when is_list(L)->
    length(L);
len(Bin) when is_binary(Bin) ->
    size(Bin);
len(Int) when is_integer(Int) ->
    size(list_to_binary(integer_to_list(3)))
        .

%% 主要对integer 进行特殊处理,转成binary ,其他的不动
to_iolist(Int) when is_integer(Int)->
    list_to_binary(integer_to_list(Int));
to_iolist(IoList)->
    IoList.


trim_crlf(Bin) when is_binary(Bin)->
    L = byte_size(Bin) - 2,
    case Bin of
        <<Bin1:L/binary, "\r\n">> ->
            Bin1;
        _ ->
            Bin
    end
.
