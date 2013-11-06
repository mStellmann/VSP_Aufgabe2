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
      {ok, NewInBranch, NewBestEdge, NewBestWT, NewFindCount, NewTestEdge, NewNodeState} = response:initiate(Level, FragName, NodeState, Edge, OwnEdgeOrddict, TestEdge, FindCount),
      main(NewNodeState, Level, FragName, OwnEdgeOrddict, OwnNodeName, NewBestEdge, NewBestWT, NewTestEdge, NewInBranch, NewFindCount);
    {test, Level, FragName, Edge} ->
      doSomething;     %   TODO
    {accept, Edge} ->
      doSomething;     %   TODO
    {reject, Edge} ->
      doSomething;      %   TODO
    {report, Weight, Edge} ->
      doSomething;        %   TODO
    {changeroot, Edge} ->
      doSomething;      %   TODO
    {connect, Level, Edge} ->
      case OwnNodeState == sleeping of
        true ->
          wakeup(OwnNodeState, OwnLevel, OwnFragName, OwnEdgeOrddict, OwnNodeName, BestEdge, BestWT, TestEdge, InBranch, FindCount);
        false ->
          {ok, NewEdgeOrddict, NewFindCount} = response:connect(OwnLevel, OwnEdgeOrddict, OwnFragName, OwnNodeState, Level, Edge, FindCount),
          main(OwnNodeState, OwnLevel, OwnFragName, NewEdgeOrddict, OwnNodeName, BestEdge, BestWT, TestEdge, InBranch, NewFindCount)
      end
  end
.

%% @private
%% @doc
%%  Function deligates to the sleeping module
wakeup(OwnNodeState, OwnLevel, OwnFragName, OwnEdgeOrddict, OwnNodeName, BestEdge, BestWT, TestEdge, InBranch, FindCount) ->
  {ok, NewEdgeOrddict} = sleeping:wakeup(OwnEdgeOrddict, OwnNodeName),
  main(OwnNodeState, OwnLevel, OwnFragName, NewEdgeOrddict, OwnNodeName, BestEdge, BestWT, TestEdge, InBranch, FindCount)
.
