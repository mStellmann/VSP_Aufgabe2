%%%-------------------------------------------------------------------
%%% @author Matthias Stellmann and Grzegorz Markiewicz
%%% @copyright (C) 2013, HAW Hamburg
%%% @doc
%%%   TODO writing the doc
%%% @private
%%% @end
%%% Created : 03. Nov 2013
%%%-------------------------------------------------------------------
-module(nodeFunction).
-author("StellmannMarkiewicz").

%% API
-export([test/8, report/5, wakeup/2, changeRoot/3]).

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

%% @doc
%%  This function search for a new edge and sends a test-message if a new basic edge is found.
%%
%%  returns:
%%    {ok, TestEdge, OwnNodeState} if no new edge is found
%%    {ok, NewTestEdge, OwnNodeState} if a test-edge is found
test(OwnEdgeOrddict, OwnLevel, OwnNodeState, OwnFragname, OwnNodeName, FindCount, InBranch, BestWT) ->
  BasicEdgeOrddict = orddict:filter(fun(_, Val) -> element(2, Val) == basic end, OwnEdgeOrddict),
  case BasicEdgeOrddict == [] of
    true ->
      report(nil, FindCount, OwnNodeState, InBranch, BestWT);
    false ->
      EdgeWeights = orddict:fetch_keys(BasicEdgeOrddict),
      TestEdgeWeight = lists:min(EdgeWeights),
      NewTestEdgeFound = orddict:fetch(TestEdgeWeight, BasicEdgeOrddict),
      ReceiveNodeName = element(1, NewTestEdgeFound),
      NewTestEdge = {TestEdgeWeight, OwnNodeName, ReceiveNodeName},
      nodeUtil:sendMessageTo(ReceiveNodeName, {test, OwnLevel, OwnFragname, NewTestEdge}),
      {ok, NewTestEdge, OwnNodeState}
  end
.

%% @doc
%%  This function sends a report-message on the InBranch. It reports the best-weight found so far.
%%
%%  returns:
%%    {ok, TestEdge, OwnNodeState} if nothing is there to report
%%    {ok, TestEdge, found} if a report was send, to switch into the found state
report(TestEdge, FindCount, OwnNodeState, InBranch, BestWT) ->
  case TestEdge == nil andalso FindCount == 0 of
    false ->
      logging:logDebug("preocedure - report: Testedge == nil and Findcount == 0 (false)"),
      {ok, TestEdge, OwnNodeState};
    true ->
      logging:logDebug("preocedure - report: Testedge == nil and Findcount == 0 (true)"),
      ReceiveNodeName = element(2, InBranch),
      NodeWeight = element(1, InBranch),
      OwnNodeName = element(3, InBranch),
      nodeUtil:sendMessageTo(ReceiveNodeName, {report, BestWT, {NodeWeight, OwnNodeName, ReceiveNodeName}}),
      {ok, TestEdge, found}
  end
.

%% @doc
%%  TODO doc
%%
%%  returns:
%%    {ok, EdgeOrdict}
changeRoot(OwnEdgeOrddict, OwnLevel, BestEdge) ->
  EdgeWeight = element(1, BestEdge),
  ReceiveNode = element(2, BestEdge),
  OwnNodeName = element(3, BestEdge),
  SendEdge = {EdgeWeight, OwnNodeName, ReceiveNode},
  {_, EdgeState} = orddict:fetch(EdgeWeight, OwnEdgeOrddict),
  case EdgeState == branch of
    true ->
      nodeUtil:sendMessageTo(ReceiveNode, {changeroot, SendEdge}),
      {ok, OwnEdgeOrddict};
    false ->
      nodeUtil:sendMessageTo(ReceiveNode, {connect, OwnLevel, SendEdge}),
      NewEdgeOrddict = orddict:store(EdgeWeight, {ReceiveNode, branch}, OwnEdgeOrddict),
      {ok, NewEdgeOrddict}
  end
.