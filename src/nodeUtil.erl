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
-export([sendMessageTo/2]).

%% @doc
%%  Finds the PID of the Node through her  global known name and sends the message
%%  to the node
%%    OtherNodeName:  The global name of the node
%%    Message:        Message that will be send
sendMessageTo(OtherNodeName, Message) ->
  OtherNodePID = global:whereis_name(OtherNodeName),
  OtherNodePID ! Message
.