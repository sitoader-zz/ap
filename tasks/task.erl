
%% @author iulia
%% @doc @todo Add description to famingo.


-module(task).
-behaviour(gen_server).


-export([stop/0, init/1, handle_call/3, handle_cast/2, state/0, get/1, delete/1, put/2]).
-export([new/0]).
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

  
handle_call({get, Key}, _, State) ->
    Response = maps:get(Key, State, undefined),
    {reply, Response, State};

% handle_call({request, {task, {Prefix, Params}, From, Ref}}, _, State) ->
%   Pair=maps:get(Prefix, State, undefined),
%   [Action] = maps:keys(Pair),
%   Env = maps:get("Env", State, undefined),
%   Result = Action:action({Prefix, Params}, Env, State),
%   case Result of
%       {no_change, Content} ->   ChangedState = State,    From ! {Ref, {200, Content}}; 
%       {new_state, Content, ChangedState} ->   From ! {Ref, {200, Content}}; 
%      unknown -> ChangedState = State, From ! {Ref, {500, "error"}} %
%   end,
%     Response = {current_state, ChangedState},
%     {reply, Response, ChangedState};

handle_call({eager, Fun}, _, State) ->
    Index_task = maps:get("current_no_tasks", State),
%%  io:fwrite("After get_a_room server State: ~p~n", [Index_task]),
    StrInt = integer_to_list(Index_task),
    New_task = list_to_atom("task" ++ StrInt),
    task:put(New_task, #{}),
    task:put("current_no_tasks", Index_task+1),
    {reply, New_task, State};



handle_call({get_state}, _, State) ->
    Response = {current_state, State},
    {reply, Response, State}.

%% handle_cast({eager, Fun}, State) ->
%% %%   Index_task = maps:get("current_no_tasks") + 1,
%% %%   New_task = list_to_atom("task" ++ Index_task),
%%  New_task = list_to_atom("task" ++ "1"),
%% %%     task:put(New_task, #{}),
%%     {noreply,State};


handle_cast({put, {Key, Value}}, State) ->
    NewState = maps:put(Key, Value, State),
    {noreply, NewState};

handle_cast({delete, Key}, State) ->
    NewState = maps:remove(Key, State),
    {noreply, NewState}.
%% 
%% eager(Fun) -> gen_server:cast(?MODULE, {eager, Fun}). 

eager(Fun) -> gen_server:call(?MODULE, {eager, Fun}).

lazy(_) -> undefined.

peek(_) -> undefined.

retrieve(_) -> undefined.

cancel(_) -> undefined.

when_done(_, _) -> undefined.

overwrite(_, _) -> undefined.

from_module(_, _) -> undefined.

and_then(_, _) -> undefined.

select(_) -> undefined.
