%% @author sitoader
%% @doc @todo Add description to 'Tests'.


-module('tests').

%% ====================================================================
%% API functions
%% ====================================================================

-export([check/0]).

check()->
	task:new(),
	Eager_task = task:eager(p),
	io:fwrite("Calling eager(p). Result: ~p~n", [Eager_task]),
	Lazy_task = task:lazy(p),
	io:fwrite("Calling lazy(p). Result: ~p~n", [Lazy_task]),
	State_task = task:peek(Eager_task),
	io:fwrite("Calling peek(task0). Result: ~p~n", [State_task]),
	task:cancel(Lazy_task),
	Info_tasks = task:state(),
	io:fwrite("Called cancel(task1). Info tasks: ~p~n", [Info_tasks]),
	Value_eager_task = task:retrieve(Eager_task),
	io:fwrite("Calling retrieve for completed task0. Result retrieve(task0): ~p~n", [Value_eager_task]),
	io:fwrite("Called when_done(task0,notifier)."),
	task:when_done(task0,notifier),
	io:fwrite("Called overwrite(Eager_task, overwritten value)."),
	task:overwrite(Eager_task, "overwritten value").