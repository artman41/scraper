-module(scraper_registry_srv).
-behaviour(gen_server).

%% API.
-export([
    all/0,
    urls/0,
    pids/0
]).
-export([start_link/0]).

%% gen_server.
-export([init/0]).
-export([
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
}).

-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).

%% API.

all() ->
    ets:tab2list(?TAB).

urls() ->
    ets:select(?TAB, [{{'$1','_'},[],['$1']}]).

pids() ->
    ets:select(?TAB, [{{'_','$1'},[],['$1']}]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    proc_lib:start_link(?MODULE, init, []).

init() ->
    register(?SERVER, self()),
    ets:new(?TAB, [protected, named_table]),
    spawn_scraper_srvs(scraper_config:get_scraper_urls()),
    proc_lib:init_ack({ok, self()}),
    gen_server:enter_loop(?MODULE, [], #state{}).

%% gen_server.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, Reason}, State) ->
    {Url, Pid} = ets:lookup_element(?TAB, 2, Pid),
    logger:error("Scraper for ~p died with reason ~p~n", [Url, Reason]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

spawn_scraper_srvs([]) ->
    ok;
spawn_scraper_srvs([Url|Tail]) ->
    case scraper_srv_sup:start_child(Url) of
        {ok, Pid} ->
            monitor(process, Pid),
            ets:insert(?TAB, {Url, Pid});
        Err = {error, _} ->
            logger:error("Failed to start a scraper for ~p with error ~p~n", [Url, Err])
    end,
    spawn_scraper_srvs(Tail).