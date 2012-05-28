%%%-------------------------------------------------------------------
%%% @author chuck <chuck@gruvs>
%%% @copyright (C) 2012, chuck
%%% @doc
%%% Simple calculator using an erlang process.
%%% @end
%%% Created : 28 May 2012 by chuck <chuck@gruvs>
%%%-------------------------------------------------------------------
-module(process).

%% API
-export([calculator/0]).

%%%===================================================================
%%% API
%%%===================================================================
%%% A sample run:
%%%    20> Calc=process:calculator().
%%%    #Fun<process.1.99510753>
%%%    21> Calc({a,1,2}).
%%%    3
%%%    22> Calc({s,6,2}).
%%%    4
%%%    23> Calc({m,5,2}).
%%%    10
%%%    24> Calc({d,5,2}).
%%%    2.5
%%%    25> Calc({f,5,2}).
%%%    "Unknown operation"
%%%    26> Calc({exit}).
%%%    "Done"
%%%    27> Calc({m,5,2}).
%%%    timeout
calculator()->
    Pid=spawn(fun loop/0),
    fun(Op)->
	    Pid!{self(),Op},
	    receive
		{Pid,Res}->
		    Res;
		Unknown ->
		    {unknown,Unknown}
	    after 5000->
		    timeout
	    end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
loop()->
    receive
	{From,{a,N1,N2}}->
	    From!{self(),N1+N2},
	    loop();
	{From,{s,N1,N2}}->
	    From!{self(),N1-N2},
	    loop();
	{From,{m,N1,N2}}->
	    From!{self(),N1*N2},
	    loop();
	{From,{d,N1,N2}}->
	    From!{self(),N1/N2},
	    loop();
	{From,{exit}}->
	    From!{self(),"Done"};
	{From,_Other} ->
	    From!{self(),"Unknown operation"},
	    loop()
    end.
