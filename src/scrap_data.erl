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
  {ok, Settings} = application:get_env(scrapper, scrap_urls),
  #{volatiledata := Volatilehost} = Settings,
  Date = get_current_date(),
  VolatileUrl = <<Volatilehost/binary, "CMVOLT_", Date/binary, ".CSV">>,
  send_request(<<"Volatile_", Date/binary>>, VolatileUrl),
  {noreply, State}.

-spec terminate(term(), term()) -> ok.
terminate(Reason, _State) ->
  erlang:display({terminated, Reason}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

send_request(Filename, Url) ->
  case hackney:get(Url, [], [], [with_body]) of
    {ok, 200, _H, Body} ->
      file:write_file(Filename, io_lib:fwrite("~s", [Body]));
    Error ->
      erlang:display(Error)
  end.

%% private
get_current_date() ->
  {YYYY, MM, DD} = erlang:date(),
  CurrentDate = format(DD) ++ format(MM) ++ format(YYYY),
  list_to_binary(CurrentDate).

%% private
format(Number) when Number < 10 ->
  "0" ++ integer_to_list(Number);
format(Number) ->
  integer_to_list(Number).
