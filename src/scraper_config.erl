-module(scraper_config).
-export([
    get_scraper_urls/0
]).

get_scraper_urls() ->
    get_env(scraper_urls, []).

%% Internal Functions

get_env(Key) ->
    application:get_env(scraper, Key).

get_env(Key, Default) ->
    case get_env(Key) of
        undefined ->
            Default;
        {ok, Value} ->
            Value
    end.