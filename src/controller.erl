%%%-------------------------------------------------------------------
%%% @author Matthias Stellmann and Grzegorz Markiewicz
%%% @copyright (C) 2013, HAW Hamburg
%%% @doc
%%%   This module provides the functionality of the main receive loop.
%%%   It delegegates most of the functions to the other modules.
%%% @private
%%% @end
%%% Created : 03. Nov 2013
%%%-------------------------------------------------------------------
-module(controller).
-author("StellmannMarkiewicz").

%% API
-export([main/10]).

%% @doc
%%  This function the the receive loop.
%%
%%  Params: [valid for all sources]
%%    OwnLevel:       The level of the fragment of the node which calls this function
%%    OwnEdgeOrddict: The orddict which contains the edge adjacent to this node
%%    OwnFragName:    The name of the fragment containing this node
%%    OwnNodeState:   The state of this node (sleeping,find,found)
%%    Otherlevel:     The level of the fragment of the node this node tries to connect to
%%    Edge:           The edge this node tries to connect trough
%%    FindCount:      TODO
main(OwnNodeState, OwnLevel, OwnFragName, OwnEdgeOrddict, OwnNodeName, BestEdge, BestWT, TestEdge, InBranch, FindCount) ->
  receive
    wakeup when OwnNodeState == sleeping ->
      logging:logMessage(OwnNodeName, "wakeup received"),
      logging:logStatus(OwnNodeState, OwnLevel, OwnFragName, OwnNodeName, BestEdge, BestWT, TestEdge, InBranch, FindCount),
      wakeup(OwnFragName, OwnEdgeOrddict, OwnNodeName, BestEdge, BestWT, TestEdge, InBranch);

    {initiate, Level, FragName, NodeState, Edge} ->
      logging:logMessage(OwnNodeName, io_lib:format("initiate message received | Level: ~p | FragName: ~p | NodeState: ~p", [Level, FragName, NodeState])),
      logging:logStatus(OwnNodeState, OwnLevel, OwnFragName, OwnNodeName, BestEdge, BestWT, TestEdge, InBranch, FindCount),
      {ok, NewInBranch, NewBestEdge, NewBestWT, NewFindCount, NewTestEdge, NewNodeState} = response:initiate(OwnNodeName, Level, FragName, NodeState, Edge, OwnEdgeOrddict, TestEdge, FindCount),
      main(NewNodeState, Level, FragName, OwnEdgeOrddict, OwnNodeName, NewBestEdge, NewBestWT, NewTestEdge, NewInBranch, NewFindCount);

    {test, Level, FragName, Edge} ->
      logging:logMessage(OwnNodeName, io_lib:format("test message received | Level: ~p | FragName: ~p", [Level, FragName])),
      logging:logStatus(OwnNodeState, OwnLevel, OwnFragName, OwnNodeName, BestEdge, BestWT, TestEdge, InBranch, FindCount),
      case OwnNodeState == sleeping of
        true ->
          {ok, NewEdgeOrddict} = nodeFunction:wakeup(OwnEdgeOrddict, OwnNodeName),
          {ok, NewEdgeOrddict2, NewTestEdge, NewNodeState} = response:test(0, found, OwnFragName, NewEdgeOrddict, Level, FragName, Edge, TestEdge, 0, InBranch, BestWT),
          main(NewNodeState, 0, OwnFragName, NewEdgeOrddict2, OwnNodeName, BestEdge, BestWT, NewTestEdge, InBranch, 0);
        false ->
          {ok, NewEdgeOrddict, NewTestEdge, NewNodeState} = response:test(OwnLevel, OwnNodeState, OwnFragName, OwnEdgeOrddict, Level, FragName, Edge, TestEdge, FindCount, InBranch, BestWT),
          main(NewNodeState, OwnLevel, OwnFragName, NewEdgeOrddict, OwnNodeName, BestEdge, BestWT, NewTestEdge, InBranch, FindCount)
      end;

    {accept, Edge} ->
      logging:logMessage(OwnNodeName, "accept message received"),
      logging:logStatus(OwnNodeState, OwnLevel, OwnFragName, OwnNodeName, BestEdge, BestWT, TestEdge, InBranch, FindCount),
      {ok, NewTestEdge, NewNodeState, NewBestEdge, NewBestWT} = response:accept(Edge, BestEdge, BestWT, FindCount, OwnNodeState, InBranch),
      main(NewNodeState, OwnLevel, OwnFragName, OwnEdgeOrddict, OwnNodeName, NewBestEdge, NewBestWT, NewTestEdge, InBranch, FindCount);

    {reject, Edge} ->
      logging:logMessage(OwnNodeName, "reject message received"),
      logging:logStatus(OwnNodeState, OwnLevel, OwnFragName, OwnNodeName, BestEdge, BestWT, TestEdge, InBranch, FindCount),
      {ok, NewEdgeOrddict, NewTestEdge, NewNodeState} = response:reject(Edge, OwnEdgeOrddict, OwnLevel, OwnNodeName, OwnNodeState, OwnFragName, FindCount, InBranch, BestWT, TestEdge),
      main(NewNodeState, OwnLevel, OwnFragName, NewEdgeOrddict, OwnNodeName, BestEdge, BestWT, NewTestEdge, InBranch, FindCount);

    {report, Weight, Edge} ->
      logging:logMessage(OwnNodeName, io_lib:format("report message received | Weight: ~p", [Weight])),
      logging:logStatus(OwnNodeState, OwnLevel, OwnFragName, OwnNodeName, BestEdge, BestWT, TestEdge, InBranch, FindCount),
      Response = response:report(Weight, Edge, OwnNodeState, OwnEdgeOrddict, OwnLevel, FindCount, BestEdge, InBranch, BestWT, TestEdge),
      case Response of
        {ok, NewOwnNodeState, NewOwnEdgeOrddict, NewOwnLevel, NewFindCount, NewBestEdge, NewInBranch, NewBestWT, NewTestEdge} ->
          main(NewOwnNodeState, NewOwnLevel, OwnFragName, NewOwnEdgeOrddict, OwnNodeName, NewBestEdge, NewBestWT, NewTestEdge, NewInBranch, NewFindCount);
        {halt} ->
          nodeUtil:exitNode(OwnNodeName)
      end;

    {changeroot, Edge} ->
      logging:logMessage(OwnNodeName, "changeroot message received"),
      logging:logStatus(OwnNodeState, OwnLevel, OwnFragName, OwnNodeName, BestEdge, BestWT, TestEdge, InBranch, FindCount),
      {ok, NewEdgeOrddict} = nodeFunction:changeRoot(OwnEdgeOrddict, OwnLevel, BestEdge),
      main(OwnNodeState, OwnLevel, OwnFragName, NewEdgeOrddict, OwnNodeName, BestEdge, BestWT, TestEdge, InBranch, FindCount);

    {connect, Level, Edge} ->
      logging:logMessage(OwnNodeName, io_lib:format("connect message received | Level: ~p", [Level])),
      logging:logStatus(OwnNodeState, OwnLevel, OwnFragName, OwnNodeName, BestEdge, BestWT, TestEdge, InBranch, FindCount),
      case OwnNodeState == sleeping of
        true ->
          {ok, NewEdgeOrddict} = nodeFunction:wakeup(OwnEdgeOrddict, OwnNodeName),
          {ok, NewEdgeOrddict2, NewFindCount} = response:connect(0, NewEdgeOrddict, OwnFragName, found, Level, Edge, 0),
          main(found, 0, OwnFragName, NewEdgeOrddict2, OwnNodeName, BestEdge, BestWT, TestEdge, InBranch, NewFindCount);
        false ->
          {ok, NewEdgeOrddict, NewFindCount} = response:connect(OwnLevel, OwnEdgeOrddict, OwnFragName, OwnNodeState, Level, Edge, FindCount),
          main(OwnNodeState, OwnLevel, OwnFragName, NewEdgeOrddict, OwnNodeName, BestEdge, BestWT, TestEdge, InBranch, NewFindCount)
      end;

    Any ->
      Message = lists:concat(["received an unknown message: ", Any]),
      logging:logMessage(OwnNodeName, Message),
      logging:logStatus(OwnNodeState, OwnLevel, OwnFragName, OwnNodeName, BestEdge, BestWT, TestEdge, InBranch, FindCount),
      main(OwnNodeState, OwnLevel, OwnFragName, OwnEdgeOrddict, OwnNodeName, BestEdge, BestWT, TestEdge, InBranch, FindCount)
  end
.

%% @private
%% @doc
%%  Function deligates to the sleeping module
wakeup(OwnFragName, OwnEdgeOrddict, OwnNodeName, BestEdge, BestWT, TestEdge, InBranch) ->
  {ok, NewEdgeOrddict} = nodeFunction:wakeup(OwnEdgeOrddict, OwnNodeName),
  main(found, 0, OwnFragName, NewEdgeOrddict, OwnNodeName, BestEdge, BestWT, TestEdge, InBranch, 0)
.