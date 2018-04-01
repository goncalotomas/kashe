%% Implements a cache with support for multiple registered instances, eviction policies, and size configurations.
%% Compatible with OTP - can be plopped into a supervision tree.
-module(kashe).

-behaviour(gen_server).

%% API
-export([
    get/1
    ,put/2
    ,start/0
    ,start/1
    ,status/0
    ,stop/0
]).

%% gen_server exports
-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).

-define(SERVER, ?MODULE).

-record(cache, {
    max_size = 67108864     :: non_neg_integer()    %% default size is 64 MiB
    ,entries = #{}          :: maps:map()
    ,ev_list = []           :: list()               %% stores information about which keys to be evicted
    ,size = 0               :: non_neg_integer()    %% current size in bytes, can never be bigger than max size
}).

start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start(Size) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Size], []).

stop() ->
    gen_server:stop(?SERVER).

get(Key) ->
    gen_server:call(?SERVER, {get, Key}).

put(Key, Value) ->
    gen_server:call(?SERVER, {put, Key, Value}).

status() ->
    gen_server:call(?SERVER, status).

%% gen_server callbacks

init([]) ->
    {ok, #cache{}};

init([Size]) ->
    {ok, #cache{max_size = Size}}.

handle_call({get, Key}, _From, #cache{entries = Es, ev_list = El} = Cache) ->
    Result = maps:get(Key, Es, undefined),
    NEvList = case Result of
        undefined -> El;
        _Val -> mv_to_hd(Key, El)
    end,
    {reply, Result, Cache#cache{ev_list = NEvList}};

handle_call({put, Key, Val}, _From, Cache) ->
    EntrySize = erlang:external_size({Key, Val}),
    try
        #cache{entries = Es, ev_list = El, size = S} = CacheWithRoom = maybe_evict(Cache, Key, EntrySize),
        NewCache = case maps:is_key(Key, Es) of
            true ->
                CacheWithRoom#cache{entries = maps:put(Key, Val, Es), ev_list = mv_to_hd(Key, El), size = S + EntrySize};
            false ->
                CacheWithRoom#cache{entries = maps:put(Key, Val, Es), ev_list = [Key | El], size = S + EntrySize}
        end,
        {reply, ok, NewCache}
    catch
        Error -> {reply, {error, Error}, Cache}
    end;

handle_call(status, _From, #cache{entries = Es, size = S, max_size = Ms} = Cache) ->
    {reply, [{entries, maps:size(Es)}, {size, S}, {max_size, Ms}], Cache}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% helper functions

maybe_evict(#cache{max_size = Ms}, _Key, EntrySize) when EntrySize > Ms ->
    throw(entry_too_large);

maybe_evict(#cache{size = S, max_size = Ms} = Cache, _Key, EntrySize) when S + EntrySize =< Ms ->
    Cache;

maybe_evict(#cache{size = S, max_size = Ms, entries = Es} = Cache, Key, EntrySize) ->
    case maps:is_key(Key, Es) of
        false ->
            %% evict entries until there is enough space
            evict(Cache, Ms - EntrySize);
        true ->
            %% cache contains key, we may not need to evict keys (just replacing the value of key)
            OldEntrySize = erlang:external_size({Key, maps:get(Key, Es)}),
            ReplaceSize = S - OldEntrySize + EntrySize,
            case ReplaceSize > Ms of
                true ->
                    %% not enough space simply replacing the value, need to free Ms - abs(OldEntrySize - EntrySize)
                    evict(Cache, Ms - abs(OldEntrySize - EntrySize));
                false ->
                    %% replacing the value associated with the key is enough, no need to evict keys.
                    Cache
            end
    end.

evict(#cache{size = S} = Cache, UntilBytes) when S =< UntilBytes ->
    Cache;

evict(Cache, UntilBytes) ->
    evict(evict_single(Cache), UntilBytes).

evict_single(#cache{size = S, entries = Es, ev_list = Ev} = Cache) ->
    Evicted = lists:last(Ev),
    EntrySize = erlang:external_size({Evicted, maps:get(Evicted, Es)}),
    Cache#cache{size = S - EntrySize, entries = maps:remove(Evicted, Es), ev_list = lists:delete(Evicted, Ev)}.

mv_to_hd(Key, List) when hd(List) =:= Key ->
    List;

mv_to_hd(Key, List) ->
    [Key | lists:delete(Key, List)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                     Eunit Tests                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

status_test() ->
    Entry1 = {"kashe", "is a simple tool"},
    Entry2 = {"because cashes", "should really not be that hard"},
    {Key1, Val1} = Entry1,
    {Key2, Val2} = Entry2,
    Size1 = erlang:external_size(Entry1),
    Size2 = erlang:external_size(Entry2),
    MaxSize = Size1 + Size2,
    kashe:start(MaxSize),
    %% check that every status parameter was initialized correctly
    Status1 = kashe:status(),
    ?assertEqual({size, 0}, lists:keyfind(size, 1, Status1)),
    ?assertEqual({entries, 0}, lists:keyfind(entries, 1, Status1)),
    ?assertEqual({max_size, MaxSize}, lists:keyfind(max_size, 1, Status1)),
    %% add first key and check status again
    ok = kashe:put(Key1, Val1),
    Status2 = kashe:status(),
    ?assertEqual({size, Size1}, lists:keyfind(size, 1, Status2)),
    ?assertEqual({entries, 1}, lists:keyfind(entries, 1, Status2)),
    ?assertEqual({max_size, MaxSize}, lists:keyfind(max_size, 1, Status2)),
    %% add second key and check final status
    ok = kashe:put(Key2, Val2),
    Status3 = kashe:status(),
    ?assertEqual({size, MaxSize}, lists:keyfind(size, 1, Status3)),
    ?assertEqual({entries, 2}, lists:keyfind(entries, 1, Status3)),
    ?assertEqual({max_size, MaxSize}, lists:keyfind(max_size, 1, Status3)),
    kashe:stop().

size_restriction_test() ->
    {Key1, Val1} = {"kashe is safe to use", "e.g. it's able to respect size restrictions"},
    {Key2, Val2} = {"invalid request key", crypto:strong_rand_bytes(100)},
    MaxSize = erlang:external_size({Key1, Val1}),
    kashe:start(MaxSize),
    ok = kashe:put(Key1, Val1),
    %% this request overflows the max_size by a few bytes
    ?assertEqual({error, entry_too_large}, kashe:put(Key2, Val2)),
    %% this request does the same, but when trying to update an existing key
    ?assertEqual({error, entry_too_large}, kashe:put(Key1, crypto:strong_rand_bytes(70))),
    kashe:stop().

eviction_policy_test() ->
    {Key1, Val1} = {"kashe", "otp app"},
    {Key2, Val2} = {"meaning of life", "42"},
    {Key3, Val3} = {"root of all evil", "premature optimization"},
    %% create kashe with space for only the 2 biggest keys
    MaxSize = erlang:external_size({Key2, Val2}) + erlang:external_size({Key3, Val3}),
    kashe:start(MaxSize),
    %% add the 3 keys
    ok = kashe:put(Key1, Val1),
    ok = kashe:put(Key2, Val2),
    ok = kashe:put(Key3, Val3),
    %% check that the first key was evicted and that the two others are still there
    ?assertEqual(undefined, kashe:get(Key1)),
    ?assertEqual("42", kashe:get(Key2)),
    ?assertEqual("premature optimization", kashe:get(Key3)),
    %% replace Key3 with a bigger value and check that Key2 gets evicted
    NewVal = crypto:strong_rand_bytes(40),
    ok = kashe:put(Key3, NewVal),
    ?assertEqual(undefined, kashe:get(Key1)),
    ?assertEqual(undefined, kashe:get(Key2)),
    ?assertEqual(NewVal, kashe:get(Key3)),
    kashe:stop().

-endif.
