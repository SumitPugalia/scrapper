-module(scrapper).

-behaviour(application).

%%% API
-export([start/0, stop/0]).

%%% Callbacks
-export([start/2, stop/1]).

%% @doc Starts the app
-spec start() -> {ok, [atom()]}.
start() ->
  application:ensure_all_started(scrapper).

%% @doc Stops the app
-spec stop() -> ok.
stop() ->
  application:stop(scrapper).

-spec start(application:start_type(), term()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
  scrapper_sup:start_link().

-spec stop([]) -> ok.
stop(_State) ->
  ok.
