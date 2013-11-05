%%%-------------------------------------------------------------------
%%% @author Matthias Stellmann and Grzegorz Markiewicz
%%% @copyright (C) 2013, HAW Hamburg
%%% @doc
%%%   TODO writing the doc
%%% @private
%%% @end
%%% Created : 03. Nov 2013
%%%-------------------------------------------------------------------
-module(nodeUtil).
-author("StellmannMarkiewicz").

%% API
-export([connect/7, sendMessageTo/2]).

%% @doc
%% TODO
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
          sendMessageTo(OtherNodeName, {initiate, NewLevel, EdgeWeight, find, {EdgeWeight, OwnNodeName, OtherNodeName}}),
          {ok, OwnEdgeOrddict, FindCount}
      end
  end
.

%% @doc
%% TODO
sendMessageTo(OtherNodeName, Message) ->
  OtherNodePID = global:whereis_name(OtherNodeName),
  OtherNodePID ! Message
.