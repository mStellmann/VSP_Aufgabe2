%%%-------------------------------------------------------------------
%%% @author Matthias Stellmann and Grzegorz Markiewicz
%%% @copyright (C) 2013, HAW Hamburg
%%% @doc
%%%   TODO writing the doc
%%% @private
%%% @end
%%% Created : 03. Nov 2013
%%%-------------------------------------------------------------------
-module(find).
-author("StellmannMarkiewicz").

%% API
-export([]).

%% @doc
%%  GlobalVars: {BestEdge, BestWT, TestEdge, InBranch, FindCount}
test(OwnEdgeOrddict, GlobalVars) ->
  BasicEdgeOrddict = orddict:filter(fun(_, Val) -> element(2, Val) == basic end, OwnEdgeOrddict),
  case BasicEdgeOrddict == [] of
    true ->
      report
      false ->
0
end,
EdgeWeights = orddict:fetch_keys(BasicEdgeOrddict),
AkmgWeight = lists:min(EdgeWeights),
% Value for the key (EdgeNumber) that is a Tuple {OtherNodeName, EdgeState}
EdgeToMark = orddict:fetch(AkmgWeight, EdgeOrddict),

0
.

%% @doc
%%  GlobalVars: {BestEdge, BestWT, TestEdge, InBranch, FindCount}
report(GlobalVars) ->
  0
.
