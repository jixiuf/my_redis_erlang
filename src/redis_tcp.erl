-module(redis_tcp).
-export([do_recv/1,start/0]).

-define(Host,"localhost").
-define(Port,6379).
-define(TCP_OPTS,[binary,{active, true},{packet,line}]).

start()->
    case gen_tcp:connect(?Host,?Port,?TCP_OPTS) of
        {ok,Socket}->
            Pid= spawn(?MODULE,do_recv,[Socket]),
            gen_tcp:controlling_process(Socket, Pid),
            %% gen_tcp:recv(Socket,0)
            {ok,Socket}
            %% timer:sleep(infinity)
                ;
        {error,Reason} ->
            {error,Reason}

    end

        .

do_recv(Socket)->
    inet:setopts(Socket, [{active, true}]),
    receive
        {tcp, Socket, Bin}->
            io:format("~p~n",[Bin]),
            do_recv(Socket);
        {tcp_closed, Socket} ->
            gen_tcp:close(Socket),
            exit(normal)
            %% do_recv(Socket)
                ;
        {tcp_error, Socket, Reason} ->
            {error,Reason};
        RandomMsg->
            io:format("random message~p~n",[RandomMsg])
    end

        .
