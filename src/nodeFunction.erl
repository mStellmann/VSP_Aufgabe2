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
-export([test/6, report/4]).

%% @doc
%%  This function search for a new edge and sends a test-message if a new basic edge is found.
%%
%%  returns:
%%    {nothingNew} if no new edge is found
%%    {newTestEdge, NewTestEdge} if a test-edge is found
test(OwnEdgeOrddict, OwnLevel, OwnFragname, FindCount, InBranch, BestWT) ->
  BasicEdgeOrddict = orddict:filter(fun(_, Val) -> element(2, Val) == basic end, OwnEdgeOrddict),
  case BasicEdgeOrddict == [] of
    true ->
      report(nil, FindCount, InBranch, BestWT);
    false ->
      EdgeWeights = orddict:fetch_keys(BasicEdgeOrddict),
      AkmgWeight = lists:min(EdgeWeights),
      NewTestEdge = orddict:fetch(AkmgWeight, BasicEdgeOrddict),
      ReceiveNodeName = element(3, NewTestEdge),
      nodeUtil:sendMessageTo(ReceiveNodeName, {test, OwnLevel, OwnFragname, NewTestEdge}),
      {newTestEdge, NewTestEdge}
  end
.

%% @doc
%%  This function sends a report-message on the InBranch. It reports the best-weight found so far.
%%
%%  returns:
%%    {nothingNew} if nothing is there to report
%%    {newNodeState, found} if a report was send, to switch into the found state
report(TestEdge, FindCount, InBranch, BestWT) ->
  case TestEdge == nil andalso FindCount == 0 of
    false ->
      {nothingNew};
    true ->
      ReceiveNodeName = element(3, InBranch),
      nodeUtil:sendMessageTo(ReceiveNodeName, {report, BestWT, InBranch}),
      {newNodeState, found}
  end
.
