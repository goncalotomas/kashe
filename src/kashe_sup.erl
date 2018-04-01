%%%-------------------------------------------------------------------
%% @doc kashe top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(kashe_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    RestartStrategy = #{
        strategy => one_for_all,
        intensity => 10,
        period => 10
    },

    KasheSpec = #{
        id => kashe,
        start => {kashe, start, []},
        restart => permanent,
        type => worker
    },
    
    {ok, {RestartStrategy, [KasheSpec]}}.

%%====================================================================
%% Internal functions
%%====================================================================
