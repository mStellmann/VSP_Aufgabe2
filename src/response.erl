%%%-------------------------------------------------------------------
%%% @author Matthias Stellmann and Grzegorz Markiewicz
%%% @copyright (C) 2013, HAW Hamburg
%%% @doc
%%%   TODO writing the doc
%%% @private
%%% @end
%%% Created : 03. Nov 2013
%%%-------------------------------------------------------------------
-module(response).
-author("StellmannMarkiewicz").

%% API
-export([connect/7, initiate/8, reject/10, test/11, accept/6, report/10]).

%% @doc
%%  This function is in charge of figuring out, if a connect message should be send.
%%  This is done by comparing the own fragment level to the level of the other fragment.
connect(OwnLevel, OwnEdgeOrddict, OwnFragName, OwnNodeState, OtherLevel, Edge, FindCount) ->
  EdgeWeight = element(1, Edge),
  OtherNodeName = element(2, Edge),
  OwnNodeName = element(3, Edge),
  case OtherLevel < OwnLevel of
    true ->
      NewEdgeOrddict = orddict:store(EdgeWeight, {OtherNodeName, branch}, OwnEdgeOrddict),
      logging:logGraph(Edge, OwnFragName, OwnLevel),
      nodeUtil:sendMessageTo(OtherNodeName, {initiate, OwnLevel, OwnFragName, OwnNodeState, {EdgeWeight, OwnNodeName, OtherNodeName}}),
      case OwnNodeState == find of
        true ->
          NewFindCount = FindCount + 1,
          {ok, NewEdgeOrddict, NewFindCount};
        false ->
          {ok, NewEdgeOrddict, FindCount}
      end;
    false ->
      {_, EdgeState} = orddict:fetch(EdgeWeight, OwnEdgeOrddict),
      case EdgeState == basic of
        true ->
          self() ! {connect, OtherLevel, Edge},
          {ok, OwnEdgeOrddict, FindCount};
        false ->
          NewLevel = OwnLevel + 1,
          logging:logGraph(Edge, EdgeWeight, NewLevel),
          nodeUtil:sendMessageTo(OtherNodeName, {initiate, NewLevel, EdgeWeight, find, {EdgeWeight, OwnNodeName, OtherNodeName}}),
          {ok, OwnEdgeOrddict, FindCount}
      end
  end
.

%%  @doc
%% TODO
%%  GlobalVars: {BestEdge, BestWT, TestEdge, InBranch, FindCount}
initiate(OwnNodeName, Level, FragName, NodeState, Edge, EdgeOrddict, TestEdge, FindCount) ->
  InBranch = Edge,
  BestEdge = nil,
  BestWT = infinity,
  FilteredOrddict = orddict:filter(fun(Key, Value) ->
    Key /= element(1, Edge) andalso element(2, Value) == branch end, EdgeOrddict),
  EdgeWeigths = orddict:fetch_keys(FilteredOrddict),
  {ok, NewFindCount} = rekOrddict(FilteredOrddict, EdgeWeigths, Level, FragName, NodeState, FindCount, OwnNodeName),
  case NodeState == find of
    true ->
      {ok, NewTestEdge, NewNodeState} = nodeFunction:test(EdgeOrddict, Level, NodeState, FragName, OwnNodeName, NewFindCount, InBranch, BestWT),
      {ok, InBranch, BestEdge, BestWT, NewFindCount, NewTestEdge, NewNodeState};
    false ->
      {ok, InBranch, BestEdge, BestWT, NewFindCount, TestEdge, NodeState}
  end
.

%% @private
%% @doc
%%  TODO
rekOrddict(FilteredOrddict, EdgeWeigths, Level, FragName, NodeState, FindCount, OwnNodeName) ->
  case EdgeWeigths == [] of
    true ->
      {ok, FindCount};
    false ->
      [Head | Tail] = EdgeWeigths,
      Value = orddict:fetch(Head, FilteredOrddict),
      OtherNodeName = element(1, Value),
      NewFindCount = case NodeState == find of
                       true ->
                         FindCount + 1;
                       false ->
                         FindCount
                     end,
      nodeUtil:sendMessageTo(OtherNodeName, {initiate, Level, FragName, NodeState, {Head, OwnNodeName, OtherNodeName}}),
      rekOrddict(FilteredOrddict, Tail, Level, FragName, NodeState, NewFindCount, OwnNodeName)
  end
.

%% @doc
%%  TODO
reject(Edge, OwnEdgeOrddict, OwnLevel, OwnNodeName, OwnNodeState, OwnFragName, FindCount, InBranch, BestWT, TestEdge) ->
  EdgeWeight = element(1, Edge),
  EdgeValue = orddict:fetch(EdgeWeight, OwnEdgeOrddict),
  EdgeName = element(1, EdgeValue),
  EdgeState = element(2, EdgeValue),
  case EdgeState == basic of
    true ->
      NewEdgeOrddict = orddict:store(EdgeWeight, {EdgeName, rejected}, OwnEdgeOrddict),
      {ok, NewTestEdge, NewNodeState} = nodeFunction:test(OwnEdgeOrddict, OwnLevel, OwnNodeState, OwnFragName, OwnNodeName, FindCount, InBranch, BestWT),
      {ok, NewEdgeOrddict, NewTestEdge, NewNodeState};
    false ->
      {ok, OwnEdgeOrddict, TestEdge, OwnNodeState}
  end
.

%% @doc
%%  TODO
%%    ReportedEdgeWeight: The weigt of the reported best Edge
%%    Edge: Edge the message originates from
report(ReportedEdgeWeight, Edge, OwnNodeState, OwnEdgeOrddict, OwnLevel, FindCount, BestEdge, InBranch, BestWT, TestEdge) ->
  EdgeName = element(1, Edge),
  InBranchName = element(1, InBranch),
  case EdgeName /= InBranchName of
    true ->
      NewFindCount = FindCount - 1,
      case ReportedEdgeWeight < BestWT of
        true ->
          NewBestWT = ReportedEdgeWeight,
          NewBestEdge = Edge,
          {ok, NewTestEdge, NewOwnNodeState} = nodeFunction:report(TestEdge, FindCount, OwnNodeState, InBranch, NewBestWT),
          {ok, NewOwnNodeState, OwnEdgeOrddict, OwnLevel, NewFindCount, NewBestEdge, InBranch, NewBestWT, NewTestEdge};
        false ->
          {ok, NewTestEdge, NewOwnNodeState} = nodeFunction:report(TestEdge, FindCount, OwnNodeState, InBranch, BestWT),
          {ok, NewOwnNodeState, OwnEdgeOrddict, OwnLevel, NewFindCount, BestEdge, InBranch, BestWT, NewTestEdge}
      end;
    false ->
      case OwnNodeState == find of
        true ->
          self() ! {report, ReportedEdgeWeight, Edge},
          {ok, OwnNodeState, OwnEdgeOrddict, OwnLevel, FindCount, BestEdge, InBranch, BestWT, TestEdge};
        false ->
          case ReportedEdgeWeight > BestWT of
            true ->
              {ok, NewEdgeOrddict} = nodeFunction:changeRoot(OwnEdgeOrddict, OwnLevel, BestEdge),
              {ok, OwnNodeState, NewEdgeOrddict, OwnLevel, FindCount, BestEdge, InBranch, BestWT, TestEdge};
            false ->
              case ReportedEdgeWeight == BestWT andalso BestWT == infinity of
                true ->
                  %% in case of more information on exit: {halt, OwnNodeState, OwnEdgeOrddict, OwnLevel, FindCount, BestEdge, InBranch, BestWT, TestEdge};
                  {halt};
                false ->
                  {ok, OwnNodeState, OwnEdgeOrddict, OwnLevel, FindCount, BestEdge, InBranch, BestWT, TestEdge}
              end
          end
      end
  end
.

%% @doc
%%  TODO
%%  returns:
%%    {ok, NewEdgeOrddict, NewTestEdge, NewNodeState}
test(OwnLevel, OwnNodeState, OwnFragName, OwnEdgeOrddict, Level, FragName, Edge, TestEdge, FindCount, InBranch, BestWT) ->
  EdgeWeight = element(1, Edge),
  OtherNodeName = element(2, Edge),
  OwnNodeName = element(3, Edge),
  SendingEdge = {EdgeWeight, OwnNodeName, OtherNodeName},
  case Level > OwnLevel of
    true ->
      % put the message at the back of the queue
      self() ! {test, Level, FragName, Edge},
      {ok, OwnEdgeOrddict, TestEdge, OwnNodeState};
    false ->
      case FragName /= OwnFragName of
        true ->
          nodeUtil:sendMessageTo(OtherNodeName, {accept, SendingEdge}),
          {ok, OwnEdgeOrddict, TestEdge, OwnNodeState};
        false ->
          {_, EdgeState} = orddict:fetch(EdgeWeight, OwnEdgeOrddict),
          case EdgeState == basic of
            true ->
              NewEdgeOrddict = orddict:store(EdgeWeight, {OtherNodeName, rejected}, OwnEdgeOrddict),
              case element(1, SendingEdge) /= element(1, TestEdge) of
                true ->
                  nodeUtil:sendMessageTo(OtherNodeName, {reject, SendingEdge}),
                  {ok, NewEdgeOrddict, TestEdge, OwnNodeState};
                false ->
                  {ok, NewTestEdge, NewNodeState} = nodeFunction:test(NewEdgeOrddict, OwnLevel, OwnNodeState, OwnFragName, OwnNodeName, FindCount, InBranch, BestWT),
                  {ok, NewEdgeOrddict, NewTestEdge, NewNodeState}
              end;
            false ->
              {ok, OwnEdgeOrddict, TestEdge, OwnNodeState}
          end
      end
  end
.

%% @doc
%%  TODO doc
%%  returns:
%%    {ok, NewTestEdge, NewNodeState, NewBestEdge, NewBestWT}
accept(Edge, BestEdge, BestWT, FindCount, OwnNodeState, InBranch) ->
  NewTestEdge = nil,
  EdgeWeight = element(1, Edge),
  case EdgeWeight < BestWT of
    true ->
      NewBestEdge = Edge,
      NewBestWT = EdgeWeight,
      {ok, NewTestEdge2, NewNodeState} = nodeFunction:report(NewTestEdge, FindCount, OwnNodeState, InBranch, NewBestWT),
      {ok, NewTestEdge2, NewNodeState, NewBestEdge, NewBestWT};
    false ->
      {ok, NewTestEdge2, NewNodeState} = nodeFunction:report(NewTestEdge, FindCount, OwnNodeState, InBranch, BestWT),
      {ok, NewTestEdge2, NewNodeState, BestEdge, BestWT}
  end
.