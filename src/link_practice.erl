%%%-------------------------------------------------------------------
%%% @author chuck <chuck@gruvs>
%%% @copyright (C) 2012, chuck
%%% @doc
%%% Process linking/supervision.
%%% @end
%%% Created : 30 May 2012 by chuck <chuck@gruvs>
%%%-------------------------------------------------------------------
-module(link_practice).

%% API
-export([on_exit/1,on_exit_loop/1]).

%%%===================================================================
%%% API
%%%===================================================================
%% Creates a function that wraps a system process and an exit handler.
%% Sample run:
%%   34> Handler = fun(Pid,Why)-> io:format("~p exited because ~p~n",[Pid,Why]) end.
%%   #Fun<erl_eval.12.113037538>
%%   35> Pid1 = spawn(fun()-> receive Other-> void end end).
%%   <0.123.0>
%%   36> Pid2 = spawn(fun()-> receive Other-> void end end).
%%   <0.125.0>
%%   37> Supervise = link_practice:on_exit(Handler).
%%   #Fun<link_practice.0.81973361>
%%   39> Supervise(Pid1).
%%   {link,<0.123.0>}
%%   40> Supervise(Pid2).
%%   {link,<0.125.0>}
%%   41> exit(Pid1,"No Reason").
%%   <0.123.0> exited because "No Reason"
%%   true
%%   42> exit(Pid2,"Some issue").
%%   <0.125.0> exited because "Some issue"
%%   true
on_exit(Handler)->
    Pid = spawn(link_practice,on_exit_loop,[Handler]),
    fun(PidToLink)->
	    Pid!{link,PidToLink}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
on_exit_loop(Handler)->
    receive
	{link,PidToLink}->
	    process_flag(trap_exit,true),
	    link(PidToLink),
	    link_practice:on_exit_loop(Handler);
	{'EXIT',Pid,Why}->
	    Handler(Pid,Why),
	    link_practice:on_exit_loop(Handler);
	Other->
	    io:format("Unknown message:~p~n",[Other]),
	    link_practice:on_exit_loop(Handler)
    end.
