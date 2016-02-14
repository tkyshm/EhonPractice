-module(koneko_server).
-export([start_link/0, order_cat/4, return_cat/2, close_shop/1]).

-record(neko, {name, color=green, description}).

start_link() ->
    spawn_link(fun init/0).

order_cat(Pid, Name, Color, Description) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, {order, Name, Color, Description}},
    receive
        {Ref, Cat} ->
            erlang:demonitor(Ref, [flush]),
            Cat;
        {'DOWN', Ref, process, Pid, Reason} ->
            erlang:error(Reason)
    after 5000 ->
        erlang:error(timeout)
    end.

%% This call is asynchronous.
return_cat(Pid, Cat = #neko{}) ->
    Pid ! {return, Cat},
    ok.

close_shop(Pid) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, terminate},
    receive
        {Ref, ok} ->
            erlang:demonitor(Ref, [flush]),
            ok;
        {'DOWN', Ref, process, Pid, Reason} ->
            erlang:error(Reason)
    after 5000 ->
        erlang:error(timeout)
    end.

init() -> loop([]).

loop(Cats) ->
    receive
        {Pid, Ref, {order, Name, Color, Description}} ->
            if Cats =:= [] ->
                   Pid ! {Ref, make_cat(Name, Color, Description)},
                   loop(Cats);
               Cats =/= [] ->
                   Pid ! {Ref, hd(Cats)},
                   loop(tl(Cats))
            end;
        {return, Cat = #neko{}} ->
            loop([Cat|Cats]);
        {Pid, Ref, terminate} ->
            Pid ! {Ref, ok},
            terminate(Cats);
        Unknown ->
            io:format("Unknown message: ~p~n", [Unknown]),
            loop(Cats)
    end.

make_cat(Name, Color, Description) ->
    #neko{name=Name, color=Color, description=Description}.

terminate(Cats) ->
    [io:format("~p was set free. ~n", [C#neko.name]) || C <- Cats ],
    ok.
    
