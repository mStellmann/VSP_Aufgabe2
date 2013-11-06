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
-export([main/6]).

main(OwnNodeState, OwnLevel, OwnFragName, OwnEdgeOrddict, OwnNodeName, FindCount) ->
  receive
    wakeup when OwnNodeState == sleeping ->
      wakeup(OwnLevel, OwnFragName, OwnEdgeOrddict, OwnNodeName, FindCount);
    {initiate, Level, FragName, NodeState, Edge} ->
      nodeUtil:initiate(Level, FragName, NodeState, Edge, OwnEdgeOrddict);       %   TODO
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
          wakeup(OwnLevel, OwnFragName, OwnEdgeOrddict, OwnNodeName, FindCount);
        false ->
          {ok, NewEdgeOrddict, NewFindCount} = nodeUtil:connect(OwnLevel, OwnEdgeOrddict, OwnFragName, OwnNodeState, Level, Edge, FindCount),
          main(OwnNodeState, OwnLevel, OwnFragName, NewEdgeOrddict, OwnNodeName, NewFindCount)
      end
  end
.

%% @private
%% @doc
%%  Function deligates to the sleeping module
wakeup(OwnLevel, OwnFragName, OwnEdgeOrddict, OwnNodeName, FindCount) ->
  {ok, NewEdgeOrddict} = sleeping:wakeup(OwnEdgeOrddict, OwnNodeName),
  main(found, OwnLevel, OwnFragName, NewEdgeOrddict, OwnNodeName, FindCount)
.
