
%% @author iulia
%% @doc @todo Add description to famingo.


-module(task).
-behaviour(gen_server).

-export([stop/0, init/1, handle_call/3, handle_cast/2, state/0, get/1, delete/1, put/2]).
-export([new/0, change_state/2]).
-export([eager/1, lazy/1, peek/1, retrieve/1, cancel/1,
        when_done/2, overwrite/2, from_module/2, and_then/2, select/1]).

new() ->  Error_check = gen_server:start_link({local, task}, ?MODULE, [], []),
                case Error_check of
                    {error, Reason} -> {error, Reason};
                    {ok, _} -> {ok, task}
                end.
stop() ->
    gen_server:stop(?MODULE).

init([]) ->
    task:put("current_no_tasks", 0),
    {ok, #{}}.

state() -> gen_server:call(?MODULE, {get_state}).
get(Key) -> gen_server:call(?MODULE, {get, Key}).
delete(Key) -> gen_server:cast(?MODULE, {delete, Key}).
put(Key, Value) -> gen_server:cast(?MODULE, {put, {Key, Value}}).
async_call(Fun, Task_name) -> gen_server:cast(?MODULE, {async_call, {Fun, Task_name}}).
change_state({T,Info}, NewStatus) -> 
%%     Current_task = maps:get(T, State),
    Updated_task = maps:put("state", NewStatus, Info),
    task:put(T, Updated_task),
    Observer_fct = maps:get("observer", Info, undefined),
    if Observer_fct==undefined -> ok;
       true -> if NewStatus == completed -> Val = maps:get("value", Info, undefined),
                                            apply(observer_module, Observer_fct, [ok,Val]);
                             true -> ok
               end
    end,
    ok.

handle_cast({async_call, {Fun, Task_name}}, State) ->
    Value = apply(fun_module, Fun, []),
    Current_task = maps:get(Task_name, State),
    Status_task = maps:get("state", Current_task),
    if  Status_task == overwritten ->  change_state({Task_name,Current_task}, completed);
        Status_task =/= cancelled ->  Updated_task = maps:put("value", Value, Current_task),
%%                                    io:fwrite("After get_a_room server State: ~p~n", [Updated_task]),
%%                                      task:put(Task_name, Updated_task), 
%%                                    io:fwrite("After get_a_room server State: ~p~n", [State]),
%%                                      change_state(Task_name, completed, State),
%%                                    io:fwrite("After get_a_room server State: ~p~n", [State]),
%%             Updated_task_2 = maps:put("state", complete, Updated_task),
            task:put(Task_name, Updated_task),
            change_state({Task_name,Updated_task}, completed),
%%                                    io:fwrite("After get_a_room server State: ~p~n", [New_state]),
%%                                    change_state(Task_name, completed, State),
                                         {noreply, State};
                                true ->  {noreply, State}
    end;
   

handle_cast({put, {Key, Value}}, State) ->
    NewState = maps:put(Key, Value, State),
    {noreply, NewState};

handle_cast({delete, Key}, State) ->
    NewState = maps:remove(Key, State),
    {noreply, NewState}.


handle_call({get, Key}, _, State) ->
    Response = maps:get(Key, State, undefined),
    {reply, Response, State};


handle_call({get_state}, _, State) ->
    Response = {current_state, State},
    {reply, Response, State};


handle_call({eager, Fun}, _, State) ->
    Index_task = maps:get("current_no_tasks", State),
    StrInt = integer_to_list(Index_task),
    New_task = list_to_atom("task" ++ StrInt),
%%  io:fwrite("After get_a_room server State: ~p~n", [Value]),
    task:put(New_task, #{"value"=>"", "type" =>eager, "state"=>running, "fct"=>Fun}),
    task:put("current_no_tasks", Index_task+1),
    async_call(Fun, New_task),
    {reply, New_task, State};

handle_call({lazy, Fun}, _, State) ->
    Index_task = maps:get("current_no_tasks", State),
%%  io:fwrite("After get_a_room server State: ~p~n", [Index_task]),
    StrInt = integer_to_list(Index_task),
    New_task = list_to_atom("task" ++ StrInt),
%%  io:fwrite("After get_a_room server State: ~p~n", [Value]),
    task:put(New_task, #{"value"=>"","type" =>lazy, "state"=>none, "fct"=>Fun}),
    task:put("current_no_tasks", Index_task+1),
    {reply, New_task, State};

handle_call({peek, T}, _, State) ->
    Requested_task = maps:get(T, State),
    Status_task = maps:get("state", Requested_task),
    io:fwrite("Updated_task: ~p~n", [Status_task]),
    if Status_task == none ->  
                           Task_fun = maps:get("fct", Requested_task), 
                           async_call(Task_fun, T), 
                           change_state({T,Requested_task}, running),
                           Status = running;
       Status_task == overwritten -> Status = running;
                   true -> Status = Status_task
    end,
    {reply, Status, State};

handle_call({cancel, T}, _, State) ->
    Requested_task = maps:get(T, State),
    Status_task = maps:get("state", Requested_task),
    if Status_task == running  ->  
                           Task_fun = maps:get("fct", Requested_task), 
%%                         cancel fct exec
%%                         async_call(Task_fun, T), 
%%                            Current_task = maps:get(T, State),
%%                            Updated_task = maps:put("state", cancelled, Current_task),
%%                            task:put(T, Updated_task),
                            change_state({T, Requested_task}, cancelled),
                           Response=cancelled;
                            true -> Response=cancelled
    end,
    {reply, Response, State};

handle_call({overwrite, {T, Val}}, _, State) ->
    Current_task = maps:get(T, State),
    Status_task = maps:get("state", Current_task),
    if Status_task == running  ->  
                            change_state({T,Current_task}, overwritten),
                            Updated_task = maps:put("value", Val, Current_task),
                            task:put(T, Updated_task), 
                           Response=ok;
                            true -> Response=ok
    end,
    {reply, Response, State};

handle_call({when_done, {T, Observer}}, _, State) ->
    Current_task = maps:get(T, State),
    Updated_task = maps:put("observer", Observer, Current_task),
    task:put(T, Updated_task),
    Status_task = maps:get("state", Current_task),
    Val = maps:get("value", Current_task),
    io:fwrite("After get_a_room server State: ~p~n", [Val]),
    if Status_task == completed  ->   apply(observer_module, Observer, [{ok,Val}]);
       Status_task == cancelled  ->  apply(observer_module, Observer, [{error,cancel,cancel}]);
       true ->ok
    end,
    {reply, ok, State}.


eager(Fun) -> gen_server:call(?MODULE, {eager, Fun}).
lazy(Fun) -> gen_server:call(?MODULE, {lazy, Fun}).
peek(T) -> gen_server:call(?MODULE, {peek, T}).

retrieve(_) -> undefined.
cancel(T) -> gen_server:call(?MODULE, {cancel, T}).

%% when_done(_, _) -> undefined.

when_done(T, Observer) -> gen_server:call(?MODULE, {when_done, {T, Observer}}).

overwrite(T, Val) -> gen_server:call(?MODULE, {overwrite, {T, Val}}).

from_module(_, _) -> undefined.

and_then(_, _) -> undefined.

select(_) -> undefined.
