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

main(OwnNodeState, OwnLevel, OwnFragName, OwnEdgeOrddict, OwnNodeName, BestEdge, BestWT, TestEdge, InBranch, FindCount) ->
  receive
    wakeup when OwnNodeState == sleeping ->
      wakeup(OwnNodeState, OwnLevel, OwnFragName, OwnEdgeOrddict, OwnNodeName, BestEdge, BestWT, TestEdge, InBranch, FindCount);
    {initiate, Level, FragName, NodeState, Edge} ->
      {ok, NewInBranch, NewBestEdge, NewBestWT, NewFindCount, NewTestEdge, NewNodeState} = response:initiate(OwnNodeName, Level, FragName, NodeState, Edge, OwnEdgeOrddict, TestEdge, FindCount),
      main(NewNodeState, Level, FragName, OwnEdgeOrddict, OwnNodeName, NewBestEdge, NewBestWT, NewTestEdge, NewInBranch, NewFindCount);
    {test, Level, FragName, Edge} ->
      case OwnNodeState == sleeping of
        true ->
          wakeup(OwnNodeState, OwnLevel, OwnFragName, OwnEdgeOrddict, OwnNodeName, BestEdge, BestWT, TestEdge, InBranch, FindCount);
        false ->
          anyThing,    % TODO
          main(OwnNodeState, OwnLevel, OwnFragName, OwnEdgeOrddict, OwnNodeName, BestEdge, BestWT, TestEdge, InBranch, FindCount)
      end;
    {accept, Edge} ->
      doSomething;     %   TODO
    {reject, Edge} ->
      {ok, NewEdgeOrddict, NewTestEdge, NewNodeState} = response:reject(Edge, OwnEdgeOrddict, OwnLevel, OwnNodeName, OwnNodeState, OwnFragName, FindCount, InBranch, BestWT, TestEdge),
      main(NewNodeState, OwnLevel, OwnFragName, NewEdgeOrddict, OwnNodeName, BestEdge, BestWT, NewTestEdge, InBranch, FindCount);
    {report, Weight, Edge} ->
      response:report(Weight, Edge, OwnNodeState, OwnEdgeOrddict, OwnLevel, FindCount, BestEdge, InBranch, BestWT, TestEdge);        %   TODO
    {changeroot, Edge} ->
      {ok, NewEdgeOrddict} = nodeFunction:changeRoot(OwnEdgeOrddict, OwnLevel, BestEdge),
      main(OwnNodeState, OwnLevel, OwnFragName, NewEdgeOrddict, OwnNodeName, BestEdge, BestWT, TestEdge, InBranch, FindCount);
    {connect, Level, Edge} ->
      case OwnNodeState == sleeping of
        true ->
          wakeup(OwnNodeState, OwnLevel, OwnFragName, OwnEdgeOrddict, OwnNodeName, BestEdge, BestWT, TestEdge, InBranch, FindCount);
        false ->
          {ok, NewEdgeOrddict, NewFindCount} = response:connect(OwnLevel, OwnEdgeOrddict, OwnFragName, OwnNodeState, Level, Edge, FindCount),
          main(OwnNodeState, OwnLevel, OwnFragName, NewEdgeOrddict, OwnNodeName, BestEdge, BestWT, TestEdge, InBranch, NewFindCount)
      end;
    Any ->
      anythingReceived % TODO logging
  end
.

%% @private
%% @doc
%%  Function deligates to the sleeping module
wakeup(OwnNodeState, OwnLevel, OwnFragName, OwnEdgeOrddict, OwnNodeName, BestEdge, BestWT, TestEdge, InBranch, FindCount) ->
  {ok, NewEdgeOrddict} = nodeFunction:wakeup(OwnEdgeOrddict, OwnNodeName),
  main(OwnNodeState, OwnLevel, OwnFragName, NewEdgeOrddict, OwnNodeName, BestEdge, BestWT, TestEdge, InBranch, FindCount)
.
