%%%-------------------------------------------------------------------
%%% @author Matthias Stellmann and Grzegorz Markiewicz
%%% @copyright (C) 2013, HAW Hamburg
%%% @doc
%%%   This module provides the functionality of the sleeping state.
%%% @private
%%% @end
%%% Created : 03. Nov 2013
%%%-------------------------------------------------------------------
-module(sleeping).
-author("StellmannMarkiewicz").

%% API
-export([wakeup/2]).


%% @doc
%%  This function is called from the sleeping state of the node.
%%  It searches for the akmg and sends the connect msg to the node
%%  on the opposite side of the akmg. Afterwards the node changes into
%%  the sleeping state and waits for a response from the other node.
%%    EdgeOrddict:  Orddict where the key is the EdgeWeight and value
%%                  is a tuple {OtherNodeName,EdgeState}
%%    OwnNodeName:  The system known name of our own node
wakeup(EdgeOrddict, OwnNodeName) ->
  EdgeWeights = orddict:fetch_keys(EdgeOrddict),
  AkmgWeight = lists:min(EdgeWeights),
  % Value for the key (EdgeNumber) that is a Tuple {OtherNodeName, EdgeState}
  EdgeToMark = orddict:fetch(AkmgWeight, EdgeOrddict),
  OtherNodeName = element(1, EdgeToMark),
  MarkedEdge = {OtherNodeName, branch},
  NewEdgeOrddict = orddict:store(AkmgWeight, MarkedEdge, EdgeOrddict),
  nodeUtil:sendMessageTo(OtherNodeName, {connect, 0, {AkmgWeight, OwnNodeName, OtherNodeName}}),
  {ok, NewEdgeOrddict}
.


