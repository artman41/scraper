-module(scraper_srv).
-behaviour(gen_server).

%% API.
-export([start_link/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
    url :: string()
}).

-define(TEN_MINUTES, 10*60*1000).
-define(CHECK, check).

%% API.

-spec start_link(string()) -> {ok, pid()}.
start_link(Url) ->
    gen_server:start_link(?MODULE, [Url], []).

%% gen_server.

init([Url]) ->
    queue_send(),
    {ok, #state{url = Url}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(?CHECK, State = #state{url = Url}) ->
    case ibrowse:send_req(Url, [{"User-Agent", random_user_agent()}], get) of
        {ok, Status, ResponseHeaders, ResponseBody} ->
            logger:info("Url: ~p, Status: ~p, Headers: ~p, Body: ~p~n", [Url, Status, ResponseHeaders, ResponseBody]);
        Err = {error, _} ->
            logger:error("Failed to GET from ~p with error ~p~n", [Url, Err])
    end,
    queue_send(),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

queue_send() ->
    erlang:send_after(?TEN_MINUTES, self(), ?CHECK).

random_user_agent() ->
    random_user_agent_(50).

random_user_agent_(0) ->
    [];
random_user_agent_(I) ->
    [32 + rand:uniform(126+1)-1 | random_user_agent_(I-1)].