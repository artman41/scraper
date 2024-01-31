-module(scraper_srv_sup).
-behaviour(supervisor).

-export([start_child/1]).
-export([start_link/0]).
-export([init/1]).

start_child(Url) ->
    supervisor:start_child(?MODULE, [Url]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 1,
        period => 5
    },
    Childs = [
        #{
            id => scraper_srv,
            start => {scraper_srv, start_link, []},
            restart => temporary,
            shutdown => 5000,
            type => worker
        }
    ],

    {ok, {SupFlags, Childs}}.
