-module(aestratum_share).

%% TODO: type spec
%% TODO: eunit

-export([new/3,
         user/1,
         miner_nonce/1,
         pow/1,
         validity/1,
         created/1
        ]).

-export([set_validity/2,
         is_duplicate/3
        ]).

-record(share, {
          user,
          miner_nonce,
          pow,
          validity,
          created
         }).

new(User, MinerNonce, Pow) ->
    #share{user = User,
           miner_nonce = MinerNonce,
           pow = Pow,
           created = aestratum_utils:timestamp()}.

user(#share{user = User}) ->
    User.

miner_nonce(#share{miner_nonce = MinerNonce}) ->
    MinerNonce.

pow(#share{pow = Pow}) ->
    Pow.

validity(#share{validity = Validity}) ->
    Validity.

created(#share{created = Created}) ->
    Created.

set_validity(Validity, #share{} = Share) ->
    Share#share{validity = Validity}.

is_duplicate(MinerNonce, Pow, #share{miner_nonce = MinerNonce, pow = Pow}) ->
    true;
is_duplicate(_MinerNonce, _Pow, _Share) ->
    false.
