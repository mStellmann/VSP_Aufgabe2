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
-export([test/7, report/5]).

%% @doc
%%  This function search for a new edge and sends a test-message if a new basic edge is found.
%%
%%  returns:
%%    {ok, TestEdge, OwnNodeState} if no new edge is found
%%    {ok, NewTestEdge, OwnNodeState} if a test-edge is found
test(OwnEdgeOrddict, OwnLevel, OwnNodeState, OwnFragname, FindCount, InBranch, BestWT) ->
  BasicEdgeOrddict = orddict:filter(fun(_, Val) -> element(2, Val) == basic end, OwnEdgeOrddict),
  case BasicEdgeOrddict == [] of
    true ->
      report(nil, FindCount, OwnNodeState, InBranch, BestWT);
    false ->
      EdgeWeights = orddict:fetch_keys(BasicEdgeOrddict),
      AkmgWeight = lists:min(EdgeWeights),
      NewTestEdge = orddict:fetch(AkmgWeight, BasicEdgeOrddict),
      ReceiveNodeName = element(3, NewTestEdge),
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
      {ok, TestEdge, OwnNodeState};
    true ->
      ReceiveNodeName = element(3, InBranch),
      nodeUtil:sendMessageTo(ReceiveNodeName, {report, BestWT, InBranch}),
      {ok, TestEdge, found}
  end
.
