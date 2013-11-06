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
-export([connect/7, initiate/7]).

%% @doc
%%  This function is in charge of figuring out, if a connect message should be send.
%%  This is done by comparing the own fragment level to the level of the other fragment.
%%    OwnLevel:       The level of the fragment of the node which calls this function
%%    OwnEdgeOrddict: The orddict which contains the edge adjacent to this node
%%    OwnFragName:    The name of the fragment containing this node
%%    OwnNodeState:   The state of this node (sleeping,find,found)
%%    Otherlevel:     The level of the fragment of the node this node tries to connect to
%%    Edge:           The edge this node tries to connect trough
%%    FindCount:      TODO
connect(OwnLevel, OwnEdgeOrddict, OwnFragName, OwnNodeState, OtherLevel, Edge, FindCount) ->
  EdgeWeight = element(1, Edge),
  OtherNodeName = element(2, Edge),
  OwnNodeName = element(3, Edge),
  case OtherLevel < OwnLevel of
    true ->
      NewEdgeOrddict = orddict:store(EdgeWeight, {OtherNodeName, branch}, OwnEdgeOrddict),
      nodeUtil:sendMessageTo(OtherNodeName, {initiate, OwnLevel, OwnFragName, OwnNodeState, {EdgeWeight, OwnNodeName, OtherNodeName}}),
      case OwnNodeState == find of
        true ->
          NewFindCount = FindCount + 1,
          {ok, NewEdgeOrddict, NewFindCount};
        false ->
          {ok, NewEdgeOrddict, FindCount}
      end;
    false ->
      {_, EdgeState} = orddict:find(EdgeWeight, OwnEdgeOrddict),
      case EdgeState == basic of
        true ->
          self ! {connect, OtherLevel, Edge},
          {ok, OwnEdgeOrddict, FindCount};
        false ->
          NewLevel = OwnLevel + 1,
          nodeUtil:sendMessageTo(OtherNodeName, {initiate, NewLevel, EdgeWeight, find, {EdgeWeight, OwnNodeName, OtherNodeName}}),
          {ok, OwnEdgeOrddict, FindCount}
      end
  end
.

%%  @doc
%% TODO
%%  GlobalVars: {BestEdge, BestWT, TestEdge, InBranch, FindCount}
initiate(Level, FragName, NodeState, Edge, EdgeOrddict, TestEdge, FindCount) ->
  InBranch = Edge,
  BestEdge = nil,
  BestWT = infinity,
  FilteredOrddict = orddict:filter(fun(Key, Value) ->
    Key /= element(1, Edge) andalso element(2, Value) == branch end, EdgeOrddict),
  EdgeWeigths = orddict:fetch_keys(FilteredOrddict),
  {ok, NewFindCount} = rekOrddict(FilteredOrddict, EdgeWeigths, Level, FragName, NodeState, FindCount),
  case NodeState == find of
    true ->
      {ok, NewTestEdge, NewNodeState} = nodeFunction:test(EdgeOrddict, Level, NodeState, FragName, FindCount, InBranch, BestWT),
      {ok, InBranch, BestEdge, BestWT, NewFindCount, NewTestEdge, NewNodeState};
    false ->
      {ok, InBranch, BestEdge, BestWT, NewFindCount, TestEdge, NodeState}
  end
.

%% @private
%% @doc
%%  TODO
rekOrddict(FilteredOrddict, EdgeWeigths, Level, FragName, NodeState, FindCount) ->
  case EdgeWeigths == [] of
    true ->
      {ok, FindCount};
    false ->
      [Head, Tail] = EdgeWeigths,
      Value = orddict:fetch(Head, FilteredOrddict),
      NodeName = element(1, Value),
      NodeState = element(2, Value),
      case NodeState == find of
        true ->
          FindCount = FindCount + 1;
        false ->
          FindCount
      end,
      nodeUtil:sendMessageTo(NodeName, {initiate, Level, FragName, NodeState}),
      rekOrddict(FilteredOrddict, Tail, Level, FragName, NodeState, FindCount)
  end
.

