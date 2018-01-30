-module(observer_module).

%% ====================================================================
%% API functions
%% ====================================================================
-export([notifier/1]).


notifier({ok,V}) -> io:fwrite("Task is completed with value: ~p~n", [V]);
notifier({cancelled, Kind, ExVal}) -> io:fwrite("task is cancelled").
