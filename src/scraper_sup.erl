-module(scraper_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 1,
        period => 5
    },
    Childs = [
        #{
            id => scraper_srv_sup,
            start => {scraper_srv_sup, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => supervisor
        },
        #{
            id => scraper_registry_srv,
            start => {scraper_registry_srv, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker
        }
    ],

    {ok, {SupFlags, Childs}}.
