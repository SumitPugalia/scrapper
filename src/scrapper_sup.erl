-module(scrapper_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
  Children = [child_spec(scrap_data)],
  {ok, {#{strategy => one_for_one}, Children}}.

child_spec(Name) ->
  #{
    id      => Name,
    start   => {Name, start_link, []},
    restart => permanent
  }.