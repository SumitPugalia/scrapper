-module(scrap_data).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    run/0,
    save_to_db/1
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
      ok = file:write_file(Filename, io_lib:fwrite("~s", [Body])),
      save_to_db(Filename);
    Error ->
      erlang:display(Error)
  end.

save_to_db(Filename) ->
  {ok, Pid} = mysql:start_link([
      {host, "localhost"},
      {user, "root"},
      {password, "password"},
      {database, "xdb_mysql_test"}
    ]),
  PropList = form_proplist(Filename),
  {Query, Params} = i_all(volatile, PropList),
  erlang:display({Query, Params}),
  mysql:query(Pid, lists:flatten(Query), Params).

% [#{age=>41,first_name=>"Alan",id=>1,is_blocked=>1,last_name=>"Turing"},
%  #{age=>73,first_name=>"Charles",id=>2,last_name=>"Darwin"},
%  #{age=>40,first_name=>"Alan",id=>3,last_name=>"Poe"}] => Proplist
% @private
i_all(Table, PropList) ->
  {Fields, Values, Args} = form_insert_query(Table, PropList, {[], [], []}),
  {
    [
     "INSERT INTO ", escape(Table), " (", string:join(Fields, ", "), ") VALUES ",
     get_args(Args, "")
    ],
    get_values(Values, [])
  }.

form_insert_query(_Table, [], {Fields, Values, Args}) ->
  {Fields, Values, Args};
form_insert_query(Table, [PropList0 | Remaining], {_F0, V0, A0}) ->
  FieldNames = get_field_names(Table),
  PropList = maps:to_list(maps:merge(maps:from_list([{K , null} || K <- FieldNames]), PropList0)),
  {Fields, Values, Args} =
    lists:foldr(fun({K, V}, {Fs, Vs, Args}) ->
      {[escape(K) | Fs], [V | Vs], ["?" | Args]}
    end, {[], [], []}, PropList),
  form_insert_query(Table, Remaining, {Fields, [Values] ++ V0, [Args] ++ A0}).

get_args([], Result) ->
  string:slice(Result, 0, string:length(Result) - 2);
get_args([Arg| Remaining], Result) ->
  Result0 = "(" ++ string:join(Arg, ", ") ++ "), ",
  get_args(Remaining, Result ++ Result0).

%% @private
get_values([], Result) ->
  Result;
get_values([Value | Remaining], Result) ->
  get_values(Remaining, Value ++ Result).

% @private
get_current_date() ->
  {YYYY, MM, DD} = erlang:date(),
  CurrentDate = format(DD) ++ format(MM) ++ format(YYYY),
  list_to_binary(CurrentDate).

% @private
format(Number) when Number < 10 ->
  "0" ++ integer_to_list(Number);
format(Number) ->
  integer_to_list(Number).

get_field_names(volatile) ->
  [date,symbol,ucp,updcp,ulr,pduv,cdudv,uav].

form_proplist(Filename) ->
  {ok, Data} = file:read_file(Filename),
  [_H | Remainings] = binary:split(Data, <<"\n">>, [global]),
  [maps:from_list(form_tuple(Remaining)) ||  Remaining <- Remainings, Remaining =/= <<>>].

form_tuple(Record) ->
  Records = binary:split(Record, <<",">>, [global]),
  lists:zip(get_field_names(volatile), Records).

escape(Field) when is_atom(Field) ->
  escape(atom_to_list(Field));
escape(Field) when is_list(Field) ->
  lists:flatten(["`", Field, "`"]).


  %% CREATE TABLE volatile (id int AUTO_INCREMENT, date varchar(255), symbol varchar(255), ucp varchar(255),
  %% updcp varchar(255), ulr varchar(255), pduv varchar(255), cdudv varchar(255), uav varchar(255), PRIMARY KEY (id) );