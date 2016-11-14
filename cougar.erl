-module(cougar).

-behaviour(gen_server).

-export([write/1]).

-export([start/1, stop/0]).

-export([init/1, handle_call/3, handle_info/2, terminate/2]).

-record(cougar_state, {
          max_buffer_size = 0,
          flush_timer_timeout = 0,
          data_store,
          buffer,
          timer }).

write(Data) ->
    gen_server:call(?MODULE, {write, Data}).

start(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

stop() -> 
    gen_server:call(?MODULE, stop).
    

%%% Callback Functions %%%

init([DetsTable, MaxBufferSize, FlushTimerTimeout]) ->
    State = #cougar_state{ max_buffer_size = MaxBufferSize,
                           flush_timer_timeout = FlushTimerTimeout,
                           data_store = DetsTable },
    {ok, reset(State)}.


handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call({write, Data}, _From, State = #cougar_state{ max_buffer_size = 0 }) ->
    {Reply, NewState} = increment(Data, State),
    {reply, Reply, NewState};

handle_call({write, Data}, _From, State = #cougar_state{ max_buffer_size = Max }) ->
    {Reply, NewState} =
        case buffer_size(State#cougar_state.buffer) of
            Size when Size < Max -> increment(Data, State);
            _ -> flush(State)
        end,
    {reply, Reply, NewState}.


handle_info(flush, State) ->
    {_, NewState} = flush(State),
    {noreply, NewState}.


terminate(_Reason, State) ->
    flush(State),
    ok.


%%% Internals %%%
reset(State = #cougar_state{ flush_timer_timeout = 0 }) ->
    State#cougar_state{ buffer=buffer_new() };
reset(State = #cougar_state{ flush_timer_timeout = Timeout }) ->
    {ok, TRef} = timer:send_after(Timeout, flush),
    State#cougar_state{ buffer=buffer_new(), timer=TRef }.


flush(State = #cougar_state{ data_store = DetsTable, buffer = Dict }) ->
    io:format("++++++++++++++++++++++++++ flush +++++++++++++++++++++++\n"),
    Reply = db_insert_bulk(DetsTable, buffer_to_list(Dict)),
    {Reply, reset(State)}.


increment(Data, State = #cougar_state{ buffer = Dict }) ->
    NewState = State#cougar_state{ buffer=buffer_update_counter(Data, 1, Dict) },
    {ok, NewState}.

db_insert_bulk(DetsTable, Data) ->
    % Rudimentary 'db' update.
    [ (update_counter(dets))(Key, Incr, DetsTable) 
      || {Key, Incr} <- Data ],
    dets:sync(DetsTable).

update_counter(Type) ->
    fun (Key, Incr, TypeInstance) ->
            % Trying to simulate dict:update_counter/3 behaviour.
            % Upgrading to the latest OTP release would make it nicer, 
            % by providing ets:update_counter/4.
            % Still this is would have to be done for dets though.
            case Type:insert_new(TypeInstance, {Key, Incr}) of
                true -> ok;
                false -> Type:update_counter(TypeInstance, Key, Incr)
            end,
            % Got to return the ets:tab() to conform with dict:update_counter/3 return type.
            TypeInstance
    end.

%%% Generalizing Buffer Type %%%

-ifdef(dict).
buffer_new() -> dict:new().
buffer_size(Buffer) -> dict:size(Buffer).
buffer_update_counter(Key, Incr, Buffer) -> dict:update_counter(Key, Incr, Buffer).
buffer_to_list(Buffer) -> dict:to_list(Buffer).

-else.
buffer_new() -> ets:new(?MODULE, [set, private]).
buffer_size(Buffer) -> ets:info(Buffer, size).
buffer_update_counter(Key, Incr, Buffer) -> EtsUp = update_counter(ets),
                                            EtsUp(Key, Incr, Buffer).
buffer_to_list(Buffer) -> ets:tab2list(Buffer).

-endif.

