%% @author sitoader
%% @doc @todo Add description to task_demo.


-module(task_demo).

%% ====================================================================
%% API functions
%% ====================================================================
-export([all_api/0]).

all_api()->
	task:new(),
	Eager_task = task:eager(p),
	Lazy_task = task:lazy(p),
	State_task = task:peek(Eager_task),
	task:cancel(Lazy_task),
	Value_eager_task = task:retrieve(Eager_task),
	task:when_done(task0,notifier),
	task:overwrite(Eager_task, "overwritten value"),
    New_task=task:and_then(Eager_task, p),
	Selected_complete_task=task:select([task0,task1]).
	




