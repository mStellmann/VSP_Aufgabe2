%%%-------------------------------------------------------------------
%%% @author Matthias Stellmann and Grzegorz Markiewicz
%%% @copyright (C) 2013, HAW Hamburg
%%% @doc
%%%   This module contains some utility functions.
%%% @private
%%% @end
%%% Created : 03. Nov 2013
%%%-------------------------------------------------------------------
-module(nodeUtil).
-author("StellmannMarkiewicz").

%% API
-export([sendMessageTo/2, exitNode/1, findGlobalHosts/0]).

findGlobalHosts() ->
  {ok, Hostlist} = file:consult("config/hosts.cfg"),
  net_adm:world_list(Hostlist)
.

%% @doc
%%  Finds the PID of the Node through her  global known name and sends the message
%%  to the node
%%    OtherNodeName:  The global name of the node
%%    Message:        Message that will be send
sendMessageTo(OtherNodeName, Message) ->
  OtherNodePID = global:whereis_name(OtherNodeName),
  OtherNodePID ! Message
.

%% @doc
%%  This Function exits the current node and log it.
exitNode(OwnNodeName) ->
  logging:logMessage(OwnNodeName, "Shutting down!"),
  erlang:exit("EndOfLife")
.