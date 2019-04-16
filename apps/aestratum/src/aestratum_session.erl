-module(aestratum_session).

%% TODO: type spec
%% TODO: add functions for setting share_target, share_target_diff_threshold,
%% desired_solve_time, max_solve_time.... - this will work with
%% aestratum_user_register - look up conn pid based on the public key and call

-export([new/0,
         handle_event/2,
         close/1
        ]).

-ifdef(TEST).
-export([state/1]).
-endif.

-include("aestratum_log.hrl").

-export_type([session/0]).

-record(state, {
          phase,
          timer,
          extra_nonce,
          accept_blocks = false,
          share_target,
          max_share_target,
          share_target_diff_threshold,
          desired_solve_time,
          max_solve_time,
          jobs,
          submissions
         }).

-opaque session() :: #state{}.

-define(HOST, application:get_env(aestratum, host, <<"pool.aeternity.com">>)).
-define(PORT, application:get_env(aestratum, port, 9999)).
-define(MSG_TIMEOUT, application:get_env(aestratum, msg_timeout, 30000)).
-define(EXTRA_NONCE_NBYTES, application:get_env(aestratum, extra_nonce_nbytes, 4)).
-define(INITIAL_SHARE_TARGET, application:get_env(aestratum, initial_share_target, 1)).
-define(MAX_SHARE_TARGET, application:get_env(aestratum, max_share_target, aestratum_target:max())).
-define(SHARE_TARGET_DIFF_THRESHOLD, application:get_env(aestratum, share_target_diff_threshold, 5.0)).
-define(DESIRED_SOLVE_TIME, application:get_env(aestratum, desired_solve_time, 30000)).
-define(MAX_SOLVE_TIME, application:get_env(aestratum, max_solve_time, 40000)).
-define(EDGE_BITS, 29).

%% API.

new() ->
    #state{phase = connected}.

handle_event({conn, Event}, State) ->
    handle_conn_event(Event, State);
handle_event({chain, Event}, State) ->
    handle_chain_event(Event, State).

close(#state{} = State) ->
    close_session(State),
    ok.

%% Internal functions.

handle_conn_event(#{event := init}, #state{phase = connected} = State) ->
    {no_send, State#state{timer = set_timer(connected)}};
handle_conn_event(#{event := recv_data, data := RawMsg}, State) ->
    case aestratum_jsonrpc:decode(RawMsg) of
        {ok, Msg}    -> recv_msg(Msg, State);
        {error, Rsn} -> recv_msg_error(Rsn, State)
    end;
%% TODO: {reconnect, Host, Port, WaitTime},...
handle_conn_event(#{event := timeout}, State) ->
    handle_conn_timeout(State);
handle_conn_event(#{event := close}, State) ->
    handle_conn_close(State).

handle_chain_event(#{event := recv_block, block := Block}, State) ->
    handle_chain_recv_block(Block, State);
handle_chain_event(#{event := set_target}, State) ->
    handle_chain_set_target(State);
handle_chain_event(#{event := notify, job_info := JobInfo}, State) ->
    handle_chain_notify(JobInfo, State).

%% Handle received messages from client.

recv_msg(#{type := req, method := configure} = Req,
         #state{phase = connected} = State) ->
    ?INFO("recv_configure_req, req: ~p", [Req]),
    send_configure_rsp(Req, State);
recv_msg(#{type := req, method := subscribe} = Req,
         #state{phase = connected} = State) ->
    ?INFO("recv_subscribe_req, req: ~p", [Req]),
    send_subscribe_rsp(Req, State);
recv_msg(#{type := req, method := subscribe} = Req,
         #state{phase = configured} = State) ->
    ?INFO("recv_subscribe_req, req: ~p", [Req]),
    send_subscribe_rsp(Req, State);
recv_msg(#{type := req, method := authorize} = Req,
         #state{phase = subscribed} = State) ->
    ?INFO("recv_authorize_req, req: ~p", [Req]),
    send_authorize_rsp(Req, State);
%% Submit request is accepted only when the connection is in authorized phase
%% and the share target is set (set_target notification was sent to the client).
recv_msg(#{type := req, method := submit} = Req,
         #state{phase = authorized, share_target = ShareTarget} = State) when
      ShareTarget =/= undefined ->
    ?INFO("recv_submit_req, req: ~p", [Req]),
    send_submit_rsp(Req, State);
recv_msg(#{type := req, method := submit} = Req,
         #state{phase = authorized, share_target = undefined} = State) ->
    ?ERROR("recv_submit_req, req: ~p", [Req]),
    send_unknown_error_rsp(Req, target_not_set, State);
recv_msg(#{type := req, method := submit} = Req,
         #state{phase = subscribed} = State) ->
    ?ERROR("recv_submit_req, reason: ~p, req: ~p", [unauthorized, Req]),
    send_unauthorized_worker_rsp(Req, null, State);
recv_msg(#{type := req, method := Method} = Req,
         #state{phase = Phase} = State) when
      ((Method =:= authorize) or (Method =:= submit)) and
      ((Phase =:= connected) or (Phase =:= configured)) ->
    ?ERROR("recv_req, reason: ~p, req: ~p", [not_subscribed, Req]),
    send_not_subscribed_rsp(Req, null, State);
recv_msg(Msg, State) ->
    ?ERROR("recv_msg, reason: ~p, msg: ~p", [unexpected_msg, Msg]),
    send_unknown_error_rsp(Msg, unexpected_msg, State).

%% JSON-RPC error responses.

recv_msg_error(parse_error = Rsn, State) ->
    ?ERROR("recv_msg_error, reason: ~p", [Rsn]),
    RspMap = #{type => rsp, method => undefined, id => null,
               reason => parse_error, data => null},
    ?INFO("send_error_rsp, rsp: ~p", [RspMap]),
    {send, encode(RspMap), State};
recv_msg_error({invalid_msg = Rsn, MaybeId}, State) ->
    ?ERROR("recv_msg_error, reason: ~p, id: ~p", [Rsn, MaybeId]),
    RspMap = #{type => rsp, method => undefined,
               id => aestratum_jsonrpc:to_id(MaybeId),
               reason => invalid_msg, data => null},
    ?INFO("send_error_rsp, rsp: ~p", [RspMap]),
    {send, encode(RspMap), State};
recv_msg_error({invalid_method = Rsn, MaybeId}, State) ->
    ?ERROR("recv_msg_error, reason: ~p, id: ~p", [Rsn, MaybeId]),
    RspMap = #{type => rsp, method => undefined,
               id => aestratum_jsonrpc:to_id(MaybeId),
               reason => invalid_method, data => null},
    ?INFO("send_error_rsp, rsp: ~p", [RspMap]),
    {send, encode(RspMap), State};
recv_msg_error({invalid_param = Rsn, Param, MaybeId}, State) ->
    ?ERROR("recv_msg_error, reason: ~p, param: ~p, id: ~p",
           [Rsn, Param, MaybeId]),
    RspMap = #{type => rsp, method => undefined,
               id => aestratum_jsonrpc:to_id(MaybeId),
               reason => invalid_param, data => atom_to_binary(Param, utf8)},
    ?INFO("send_error_rsp, rsp: ~p", [RspMap]),
    {send, encode(RspMap), State};
recv_msg_error({internal_error = Rsn, MaybeId}, State) ->
    ?ERROR("recv_msg_error, reason: ~p, id: ~p", [Rsn, MaybeId]),
    RspMap = #{type => rsp, method => undefined,
               id => aestratum_jsonrpc:to_id(MaybeId),
               reason => internal_error, data => null},
    ?INFO("send_error_rsp, rsp: ~p", [RspMap]),
    {send, encode(RspMap), State}.

%% Handle timeout.

handle_conn_timeout(#state{phase = Phase, timer = {_TRef, Phase}} = State) when
      (Phase =:= connected) or (Phase =:= configured) or (Phase =:= subscribed) ->
    %% The timer's phase is the same as the current phase, so the timeout
    %% applies and the connection to the client is closed.
    ?INFO("handle_conn_timeout, phase: ~p", [Phase]),
    {stop, close_session(State)};
handle_conn_timeout(State) ->
    {no_send, State}.

handle_conn_close(State) ->
    {stop, close_session(State)}.

%% Server to client responses.

send_configure_rsp(Req, State) ->
    send_configure_rsp1(validate_configure_req(Req, State), Req, State).

send_subscribe_rsp(Req, State) ->
    send_subscribe_rsp1(validate_subscribe_req(Req, State), Req, State).

send_authorize_rsp(Req, State) ->
    send_authorize_rsp1(validate_authorize_req(Req, State), Req, State).

send_submit_rsp(#{user := User, miner_nonce := MinerNonce, pow := Pow} = Req, State) ->
    %% MinerNonce is guaranteed (by decoder) to be of valid size and hex
    %% encoded. What is not guaranteed here is that the miner nonce bytes +
    %% extra nonce bytes are not 8 together.
    %% TODO: move below to aestratum_nonce module
    MinerNonceNBytes = byte_size(MinerNonce) div 2,
    MinerNonceVal = aestratum_nonce:to_int(miner, MinerNonce, MinerNonceNBytes),
    MinerNonce1 = aestratum_nonce:new(miner, MinerNonceVal, MinerNonceNBytes),
    Share = aestratum_share:new(User, MinerNonce1, Pow),
    Extra = #{miner_nonce => MinerNonce1, share => Share},
    send_submit_rsp1(validate_submit_req(Req, State, Extra), Req, State).

send_configure_rsp1(ok, #{id := Id}, #state{timer = Timer} = State) ->
    %% TODO: there are no configure params currently
    cancel_timer(Timer),
    RspMap = #{type => rsp, method => configure, id => Id, result => []},
    ?INFO("send_configure_rsp, rsp: ~p", [RspMap]),
    {send, encode(RspMap), State#state{phase = configured,
                                       timer = set_timer(configured)}}.
%%send_configure_rsp1({error, Rsn}, ...

send_subscribe_rsp1(ok, #{id := Id} = Req, #state{timer = Timer} = State) ->
    cancel_timer(Timer),
    %% TODO: Session resumption not supported (yet).
    ExtraNonceNBytes = ?EXTRA_NONCE_NBYTES,
    case aestratum_extra_nonce_cache:get(ExtraNonceNBytes) of
        {ok, ExtraNonce} ->
            SessionId1 = null,
            ExtraNonce1 = aestratum_nonce:to_hex(ExtraNonce),
            RspMap = #{type => rsp, method => subscribe, id => Id,
                       result => [SessionId1, ExtraNonce1]},
            ?INFO("send_subscribe_rsp, rsp: ~p", [RspMap]),
            %% Set timer for authorize request.
            {send, encode(RspMap),
             State#state{phase = subscribed, timer = set_timer(subscribed),
                         extra_nonce = ExtraNonce}};
        {error, Rsn} ->
            send_unknown_error_rsp(Req, Rsn, State)
    end;
send_subscribe_rsp1({error, Rsn}, Req, State) ->
    send_unknown_error_rsp(Req, Rsn, State).

send_authorize_rsp1(ok, #{id := Id, user := User},
                    #state{timer = Timer} = State) ->
    cancel_timer(Timer),
    aestratum_user_register:add(User, self()),
    RspMap = #{type => rsp, method => authorize, id => Id, result => true},
    %% After the authorization, the server is supposed to send an initial
    %% target.
    self() ! {chain, #{event => set_target}},
    %% No need to set timer after authorization, there are no further expected
    %% requests within a time period. Submit requests do not require timeout.
    %% Job queue is initialized to make it ready to accept client sumbissions.
    ?INFO("send_authorize_rsp, rsp: ~p", [RspMap]),
    {send, encode(RspMap),
     State#state{phase = authorized, timer = undefined,
                 jobs = aestratum_job_queue:new()}};
send_authorize_rsp1({error, user_and_password}, #{id := Id}, State) ->
    RspMap = #{type => rsp, method => authorize, id => Id,
               result => false},
    %% Timer is not cancelled, the client has a chance to send another
    %% authorize request.
    ?INFO("send_authorize_rsp, rsp: ~p", [RspMap]),
    {send, encode(RspMap), State}.

send_submit_rsp1({ok, Share, Job}, #{id := Id, miner_nonce := MinerNonce, pow := Pow},
                 #state{jobs = Jobs} = State) ->
    JobId = aestratum_job:id(Job),
    Job1 = aestratum_job:add_share(Share, Job),
    Jobs1 = aestratum_job_queue:replace(JobId, Job1, Jobs),
    User = aestratum_share:user(Share),
    ShareTarget = aestratum_job:share_target(Job),
    BlockHash = aestratum_job:block_hash(Job),
    aestratum_reward:submit_share(User, ShareTarget, BlockHash),
    case aestratum_share:validity(Share) of
        valid_block -> aestratum_chain:submit_solution(BlockHash, MinerNonce, Pow);
        valid_share -> ok
    end,
    RspMap = #{type => rsp, method => submit, id => Id, result => true},
    ?INFO("send_submit_rsp, rsp: ~p", [RspMap]),
    {send, encode(RspMap), State#state{jobs = Jobs1}};
send_submit_rsp1({error, Share, Job}, #{id := Id} = Req,
                 #state{jobs = Jobs} = State) when Job =/= undefined ->
    JobId = aestratum_job:id(Job),
    Job1 = aestratum_job:add_error_share(Share, Job),
    Jobs1 = aestratum_job_queue:replace(JobId, Job1, Jobs),
    State1 = State#state{jobs = Jobs1},
    case aestratum_share:validity(Share) of
        user_not_found ->
            send_unauthorized_worker_rsp(Req, null, State1);
        invalid_miner_nonce = Rsn ->
            send_unknown_error_rsp(Req, Rsn, State1);
        duplicate_share ->
            send_duplicate_share_rsp(Req, null, State1);
        invalid_solution = Rsn ->
            send_unknown_error_rsp(Req, Rsn, State1);
        high_target_share ->
            send_low_difficulty_share_rsp(Req, null, State1);
        max_solve_time_exceeded ->
            %% TODO: send unknow_error with data set instead?
            RspMap = #{type => rsp, method => submit, id => Id, result => false},
            ?INFO("send_submit_rsp, rsp: ~p", [RspMap]),
            {send, encode(RspMap), State1}
    end;
send_submit_rsp1({error, Share, undefined}, Req, State) ->
    %% The share is not saved here as there is no job associated with it. This
    %% can be a security issue, we need to check how many of these are
    %% submitted and possibly ban/disconnect the client.
    job_not_found = aestratum_share:validity(Share),
    send_job_not_found_rsp(Req, null, State).

%% Stratum error responses.

send_not_subscribed_rsp(#{id := Id}, Data, State) ->
    RspMap = #{type => rsp, id => Id, reason => not_subscribed,
               data => error_data(Data)},
    ?INFO("send_not_subscribed_rsp, rsp: ~p", [RspMap]),
    {send, encode(RspMap), State}.

send_unauthorized_worker_rsp(#{id := Id}, Data, State) ->
    RspMap = #{type => rsp, id => Id, reason => unauthorized_worker,
               data => error_data(Data)},
    ?INFO("send_unauthorized_worker_rsp, rsp: ~p", [RspMap]),
    {send, encode(RspMap), State}.

send_low_difficulty_share_rsp(#{id := Id}, Data, State) ->
    RspMap = #{type => rsp, id => Id, reason => low_difficulty_share,
               data => error_data(Data)},
    ?INFO("send_low_difficulty_share_rsp, rsp: ~p", [RspMap]),
    {send, encode(RspMap), State}.

send_duplicate_share_rsp(#{id := Id}, Data, State) ->
    RspMap = #{type => rsp, id => Id, reason => duplicate_share,
               data => error_data(Data)},
    ?INFO("send_duplicate_share_rsp, rsp: ~p", [RspMap]),
    {send, encode(RspMap), State}.

send_job_not_found_rsp(#{id := Id}, Data, State) ->
    RspMap = #{type => rsp, id => Id, reason => job_not_found,
               data => error_data(Data)},
    ?INFO("send_job_not_found_rsp, rsp: ~p", [RspMap]),
    {send, encode(RspMap), State}.

send_unknown_error_rsp(#{id := Id}, Data, State) ->
    RspMap = #{type => rsp, id => Id, reason => unknown_error,
               data => error_data(Data)},
    ?INFO("send_unknown_error_rsp, rsp: ~p", [RspMap]),
    {send, encode(RspMap), State}.

error_data(null) ->
    null;
error_data(Data) when is_atom(Data) ->
    atom_to_binary(Data, utf8).

%% TODO: reconnect request.

%% Handle chain related messages.

handle_chain_recv_block(#{block_hash := BlockHash, block_version := BlockVersion,
                          block_target := BlockTarget} = NewBlock,
                       #state{phase = authorized, accept_blocks = true,
                              jobs = Jobs} = State) ->
    JobId = aestratum_job:make_id(BlockHash, BlockVersion, BlockTarget),
    case aestratum_job_queue:member(JobId, Jobs) of
        true  -> {no_send, State};  %% Unlikely to happen?
        false -> handle_chain_recv_block1(NewBlock#{job_id => JobId}, State)
    end;
handle_chain_recv_block(_NewBlock, State) ->
    %% Skip this new block. The session is not ready to handle it.
    %% The phase is not authorized or the accept_blocks is set to false.
    {no_send, State}.

handle_chain_recv_block1(#{block_hash := BlockHash, block_version := BlockVersion,
                           block_target := BlockTarget, job_id := JobId} = NewJobInfo,
                        #state{share_target = ShareTarget,
                               max_share_target = MaxShareTarget,
                               share_target_diff_threshold = ShareTargetDiffThreshold,
                               desired_solve_time = DesiredSolveTime,
                               max_solve_time = MaxSolveTime,
                               jobs = Jobs} = State) ->
    %% We take the newest queue job, and get its share target (or just take
    %% the initial share target if there are no jobs in the queue).
    OldJobShareTarget =
        case aestratum_job_queue:get_rear(Jobs) of
            {ok, OldJob}   -> aestratum_job:share_target(OldJob);
            {error, empty} -> ShareTarget
        end,
    %% If there are enough jobs in the queue, we calculate the new job's
    %% target from them, otherwise we use the initial share target.
    NewJobShareTarget =
        case aestratum_job_queue:share_target(DesiredSolveTime, MaxShareTarget, Jobs) of
            {ok, NewShareTarget}     -> NewShareTarget;
            {error, not_enough_jobs} -> ShareTarget
        end,
    case aestratum_target:diff(NewJobShareTarget, OldJobShareTarget) of
        {_Change, Percent} when Percent > ShareTargetDiffThreshold ->
            %% We stop accepting blocks until the {chain, {notify, ...}}
            %% event is processed. There might be some other new block chain
            %% events in the message queue of this process, these are skipped.
            %% We copy share target, desired solve time and max solve time
            %% as these values were used for the new target computation and can
            %% be changed in the state when processing {chain, {notify, ...}}
            %% event.
            NewJobInfo1 = NewJobInfo#{share_target => NewJobShareTarget,
                                      desired_solve_time => DesiredSolveTime,
                                      max_solve_time => MaxSolveTime},
            self() ! {chain, #{event => notify, job_info => NewJobInfo1}},
            State1 = State#state{accept_blocks = false},
            send_set_target_ntf(NewJobShareTarget, State1);
        _Other ->
            Job = aestratum_job:new(JobId, BlockHash, BlockVersion, BlockTarget,
                                    OldJobShareTarget, DesiredSolveTime, MaxSolveTime),
            %% TODO: compute if the client's job queueu should be cleaned.
            State1 = State#state{jobs = aestratum_job_queue:add(Job, Jobs)},
            send_notify_ntf(JobId, BlockHash, BlockVersion, true, State1)
    end.

handle_chain_set_target(State) ->
    ShareTarget = ?INITIAL_SHARE_TARGET,
    State1 =
        State#state{accept_blocks = true,
                    share_target = ShareTarget,
                    max_share_target = ?MAX_SHARE_TARGET,
                    share_target_diff_threshold = ?SHARE_TARGET_DIFF_THRESHOLD,
                    desired_solve_time = ?DESIRED_SOLVE_TIME,
                    max_solve_time = ?MAX_SOLVE_TIME},
    send_set_target_ntf(ShareTarget, State1).

handle_chain_notify(#{job_id := JobId, block_hash := BlockHash,
                      block_version := BlockVersion, block_target := BlockTarget,
                      share_target := ShareTarget,
                      desired_solve_time := DesiredSolveTime,
                      max_solve_time := MaxSolveTime},
                    #state{accept_blocks = false, jobs = Jobs} = State) ->
    Job = aestratum_job:new(JobId, BlockHash, BlockVersion, BlockTarget,
                            ShareTarget, DesiredSolveTime, MaxSolveTime),
    Jobs1 = aestratum_job_queue:add(Job, Jobs),
    State1 = State#state{accept_blocks = true, jobs = Jobs1},
    send_notify_ntf(JobId, BlockHash, BlockVersion, true, State1).

%% Notifications from server to client.

send_set_target_ntf(ShareTarget, State) ->
    NtfMap = #{type => ntf, method => set_target,
               target => aestratum_target:to_hex(ShareTarget)},
    ?INFO("send_set_target_ntf, ntf: ~p", [NtfMap]),
    {send, encode(NtfMap), State}.

send_notify_ntf(JobId, BlockHash, BlockVersion, EmptyQueue, #state{} = State) ->
    NtfMap = #{type => ntf, method => notify, job_id => JobId,
               block_hash => BlockHash, block_version => BlockVersion,
               empty_queue => EmptyQueue},
    ?INFO("send_notify_ntf, ntf: ~p", [NtfMap]),
    {send, encode(NtfMap), State}.

%% Helper functions.

close_session(#state{phase = Phase, extra_nonce = ExtraNonce,
                     timer = Timer} = State) ->
    maybe_free_extra_nonce(ExtraNonce),
    maybe_cancel_timer(Timer),
    case Phase of
        authorized -> aestratum_user_register:del(self());
        _Other     -> ok
    end,
    ?INFO("close_session", []),
    State#state{phase = disconnected, extra_nonce = undefined,
                timer = undefined}.

maybe_free_extra_nonce(ExtraNonce) when ExtraNonce =/= undefined ->
    aestratum_extra_nonce_cache:free(ExtraNonce),
    ok;
maybe_free_extra_nonce(undefined) ->
    ok.

set_timer(Phase) ->
    TRef = erlang:send_after(?MSG_TIMEOUT, self(), {conn, #{event => timeout}}),
    {TRef, Phase}.

maybe_cancel_timer({_TRef, _Phase} = Timer) ->
    cancel_timer(Timer);
maybe_cancel_timer(undefined) ->
    ok.

cancel_timer({TRef, _Phase}) when is_reference(TRef) ->
    erlang:cancel_timer(TRef).

validate_configure_req(#{params := []}, _State) ->
    ok.

validate_subscribe_req(Req, State) ->
    run([fun check_user_agent/2,
         fun check_session_id/2,
         %%fun check_host/2,  %% TODO: check disabled due to testing
         fun check_port/2], Req, State).

validate_authorize_req(Req, State) ->
    run([fun check_user_and_password/2], Req, State).

validate_submit_req(Req, State, Extra) ->
    run([fun check_job_id/3,
         fun check_user/3,
         fun check_miner_nonce/3,
         fun check_duplicate_share/3,
         fun check_solution/3,
         fun check_target/3,
         fun check_timestamp/3], Req, State, Extra).

check_user_agent(#{user_agent := _UserAgent}, _State) ->
    %% Some user agents may not by supported by the server
    continue.

check_session_id(#{session_id := _SessionId}, _State) ->
    continue.

check_host(#{host := Host}, _State) ->
    check_host1(Host, ?HOST).

check_host1(Host, Host) ->
    continue;
check_host1(_Host, _Host1) ->
    {done, {error, host_mismatch}}.

check_port(#{port := Port}, _State) ->
    check_port1(Port, ?PORT).

check_port1(Port, Port) ->
    continue;
check_port1(_Port, _Port1) ->
    {done, {error, port_mismatch}}.

check_user_and_password(#{user := User, password := null}, _State) ->
    %% TODO: user as "public_key.worker"?
    case aestratum_user_register:member(User) of
        %% The user must not be present already
        false -> continue;
        true  -> {done, {error, user_and_password}}
    end.

check_job_id(#{job_id := JobId}, #state{jobs = Jobs}, #{share := Share}) ->
    case aestratum_job_queue:find(JobId, Jobs) of
         {ok, Job} ->
            {add_extra, #{job => Job}};
         {error, not_found} ->
            Share1 = aestratum_share:set_validity(job_not_found, Share),
            {done, {error, Share1, undefined}}
    end.

check_user(#{user := User}, _State, #{share := Share, job := Job}) ->
    case aestratum_user_register:member(User) of
        true ->
            continue;
        false ->
            Share1 = aestratum_share:set_validity(user_not_found, Share),
            {done, {error, Share1, Job}}
    end.

check_miner_nonce(_Req, #state{extra_nonce = ExtraNonce},
                  #{miner_nonce := MinerNonce, share := Share, job := Job}) ->
    ComplementNBytes = aestratum_nonce:complement_nbytes(ExtraNonce),
    case aestratum_nonce:nbytes(MinerNonce) =:= ComplementNBytes of
        true ->
            continue;
        false ->
            Share1 = aestratum_share:set_validity(invalid_miner_nonce, Share),
            {done, {error, Share1, Job}}
    end.

check_duplicate_share(#{pow := Pow}, _State,
                      #{miner_nonce := MinerNonce, share := Share, job := Job}) ->
    case aestratum_job:is_share_present(MinerNonce, Pow, Job) of
        false ->
            continue;
        true ->
            Share1 = aestratum_share:set_validity(duplicate_share, Share),
            {done, {error, Share1, Job}}
    end.

%% TODO: check_share_timestamp? If a share is submitted long after a job was
%% created.

check_solution(#{pow := Pow}, #state{extra_nonce = ExtraNonce},
               #{miner_nonce := MinerNonce, share := Share, job := Job}) ->
    BlockHash = aestratum_job:block_hash(Job),
    BlockVersion = aestratum_job:block_version(Job),
    Nonce = aestratum_nonce:merge(ExtraNonce, MinerNonce),
    case aestratum_miner:verify_proof(BlockHash, BlockVersion, Nonce,
                                      Pow, ?EDGE_BITS) of
        true ->
            continue;
        false ->
            Share1 = aestratum_share:set_validity(invalid_solution, Share),
            {done, {error, Share1, Job}}
    end.

check_target(#{pow := Pow}, _State, #{share := Share, job := Job}) ->
    BlockTarget = aestratum_job:block_target(Job),
    ShareTarget = aestratum_job:share_target(Job),
    case aestratum_miner:get_target(Pow, ?EDGE_BITS) of
        Target when Target =< BlockTarget ->  %% TODO =< or just < ?
            Share1 = aestratum_share:set_validity(valid_block, Share),
            {add_extra, #{share => Share1}};
        Target when Target =< ShareTarget ->  %% TODO: =< or just < ?
            Share1 = aestratum_share:set_validity(valid_share, Share),
            {add_extra, #{share => Share1}};
        _Other ->
            Share1 = aestratum_share:set_validity(high_target_share, Share),
            {done, {error, Share1, Job}}
    end.

check_timestamp(_Req, #state{max_solve_time = MaxSolveTime},
                #{share := Share, job := Job}) ->
    case aestratum_share:created(Share) - aestratum_job:created(Job) of
        SolveTime when SolveTime =< MaxSolveTime ->
            %% Validity already set in the check_target/3 check.
            {done, {ok, Share, Job}};
        _Other ->
            %% Solve time is greater than the max solve time.
            Share1 = aestratum_share:set_validity(max_solve_time_exceeded, Share),
            {done, {error, Share1, Job}}
    end.

run([Fun | Funs], Req, State) ->
    case Fun(Req, State) of
        continue ->
            run(Funs, Req, State);
        {done, Res} ->
            Res
    end;
run([], _Req, _State) ->
    ok.

run([Fun | Funs], Req, State, Extra) ->
    case Fun(Req, State, Extra) of
        {add_extra, M} when is_map(M) ->
            run(Funs, Req, State, maps:merge(Extra, M));
        continue ->
            run(Funs, Req, State, Extra);
        {done, Res} ->
            Res
    end.

encode(Map) ->
    {ok, RawMsg} = aestratum_jsonrpc:encode(Map),
    RawMsg.

%% Used for testing only.

-ifdef(TEST).
state(#state{phase = Phase, timer = Timer, extra_nonce = ExtraNonce,
             accept_blocks = AcceptBlocks, share_target = ShareTarget,
             max_solve_time = MaxSolveTime}) ->
    #{phase => Phase,
      timer_phase => case Timer of
                         {_, TPhase} -> TPhase;
                         undefined -> undefined
                     end,
      extra_nonce => ExtraNonce,
      accept_blocks => AcceptBlocks,
      share_target => ShareTarget,
      max_solve_time => MaxSolveTime
     }.
-endif.

