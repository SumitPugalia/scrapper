-module(scrap_data).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    run/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    terminate/2
]).

%%%===================================================================
%%% API
%%%===================================================================

-spec(start_link() -> {ok, pid()} | ignore | {error, term()}).
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec run() -> ok.
run() ->
  gen_server:cast(?MODULE, scrap).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(term()) -> {ok, any()}.
init(_Args) ->
  _ = process_flag(trap_exit, true),
  {ok, #{}}.

-spec handle_call(term(), term(), any()) -> {reply, ok, any()}.
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

-spec handle_cast(term(), any()) -> {noreply, any()}.
handle_cast(scrap, State) ->
  erlang:display(scrapper_working),
  send_request(),
  {noreply, State}.

-spec terminate(term(), term()) -> ok.
terminate(Reason, _State) ->
  lager:error("process terminated ~p", [Reason]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

send_request() ->
  Url = <<"https://www.nseindia.com/archives/nsccl/volt/CMVOLT_20032018.CSV">>,
  {ok, 200, _H, Body} = hackney:get(Url, [], [], [with_body]),
  A = file:write_file(<<"test.csv">>, io_lib:fwrite("~s", [Body])),
  erlang:display(A).