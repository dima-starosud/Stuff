-module(stepper).
-compile(export_all).

% Test

test() ->
    H = create_simple_handler(),
    io:format("Status 0: ~p~n", [dict:to_list(H(status))]),
    H("start:FirstStep"),
    H("start:SecondStep"),
    H("fail:FirstStep SomethingWrong"),
    H("fail:FirstStep SomethingElse"),
    H("fail:NonExistent IAmAlive"),
    H("finish:FirstStep"),
    H("finish:SecondStep"),
    H("finish:SecondStep"),
    io:format("Status 1: ~p~n", [dict:to_list(H(status))]),
    H(exit),
    H(status).

% Process wrapper

receive_alive(Pid) ->
    receive
	{Pid, Response} -> Response
    after 0 ->
	case is_process_alive(Pid) of
	    true -> receive_alive(Pid);
	    false -> process_is_dead
	end
    end.

wrap_pid(Pid, Func) ->
    fun (Request) ->
	    case Func(Request) of
		{sync, Req} ->
		    Pid ! Req,
		    receive_alive(Pid);
		{async, Req} ->
		    Pid ! Req,
		    void;
		{nosend, Result} ->
		    Result
	    end
    end.

% Example: simple handler

create_simple_handler() -> create_handler(fun simple_handlers/1).

simple_handlers(Base) ->
    [{"start:(\\w+)",
      fun (Name) -> Base({start, Name}) end},
     {"fail:(\\w+) (\\w+)",
      fun (Name, Fail) -> Base({fail, Name, Fail}) end},
     {"finish:(\\w+)",
      fun (Name) -> Base({finish, Name}) end}].

% 'Handlers' handler

create_handler(HandlersFun) ->
    Base = create_base(),
    Handlers = compose_handlers(HandlersFun(Base)),
    fun (status) -> Base(status);
	(exit) -> Base(exit);
	(Line) -> [H(Line) || H <- Handlers],
		  void
    end.

% Basic handler

create_base() ->
    Base = spawn(?MODULE, loop_base, [dict:new()]),
    Fun = fun (status) -> {sync, {status, self()}};
	      (R) -> {async, R}
	  end,
    wrap_pid(Base, Fun).

loop_base(exit) -> ok;
loop_base(Dict) ->
    loop_base(
      receive
	  {start, Name} -> step_start(Dict, Name);
	  {fail, Name, Fail} -> step_fail(Dict, Name, Fail);
	  {finish, Name} -> step_finish(Dict, Name);
	  {status, From} -> From ! {self(), Dict}, Dict;
	  exit -> exit
      end).

step_start(Dict, Name) ->
    F = case dict:find(Name, Dict) of
	    {ok, {started, Fails}} -> [started_twice|Fails];
	    {ok, {finished, Fails}} -> [started_after_finish|Fails];
	    error -> []
	end,
    io:format("~p: Started~n", [Name]),
    dict:store(Name, {started, F}, Dict).

step_fail(Dict, Name, Fail) ->
    F = case dict:find(Name, Dict) of
	    {ok, {started, Fails}} -> [Fail|Fails];
	    {ok, {finished, Fails}} -> [failed_after_finish|Fails];
	    error -> [failed_but_not_started]
	end,
    io:format("~p: Failed with ~p ~n", [Name, Fail]),
    dict:store(Name, {started, F}, Dict).

step_finish(Dict, Name) ->
    F = case dict:find(Name, Dict) of
	    {ok, {started, Fails}} -> Fails;
	    {ok, {finished, Fails}} -> [finished_twice|Fails];
	    error -> [finished_but_not_started]
	end,
    io:format("~p: Finished~n", [Name]),
    dict:store(Name, {finished, F}, Dict).

% Handler helpers

compose_handlers(Pairs) -> [compose_handler(Re, Fun) || {Re, Fun} <- Pairs].

compose_handler(Re, Fun) ->
    {ok, Mp} = re:compile(Re),
    fun (Line) ->
	    case re:run(Line, Mp) of
		{match, [_|Data]} ->
		    apply(Fun, [string:substr(Line, Start + 1, End) || {Start, End} <- Data]);
		nomatch ->
		    void
	    end
    end.
