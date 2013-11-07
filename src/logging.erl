%%%-------------------------------------------------------------------
%%% @author Matthias Stellmann and Grzegorz Markiewicz
%%% @copyright (C) 2013, HAW Hamburg
%%% @doc
%%%  This module provides the functionality of logging messages on the console and into a .log-file.
%%% @private
%%% @end
%%% Created : 03. Nov 2013
%%%-------------------------------------------------------------------
-module(logging).
-author("StellmannMarkiewicz").

%% API
-export([logMessage/2, logStatus/10]).

%% @private
%% @doc
%%  The Function returns the hostname of the current system.
getHostname() ->
  {ok, Hostname} = inet:gethostname(),
  Hostname
.

%% @doc
%%  The Function logs the given Message into a .log-File and writes it on the console.
logMessage(NodeName, Message) ->
  Filename = lists:concat(["logs/", NodeName, "@", getHostname(), ".log"]),
  werkzeug:logging(Filename, lists:concat(["[", werkzeug:timeMilliSecond(), "] ", NodeName, " - ", Message, "\n"]))
.

logStatus(NodeState, Level, FragName, EdgeOrddict, NodeName, BestEdge, BestWT, TestEdge, InBranch, FindCount) ->
  Message1 = lists:concat(["[", werkzeug:timeMilliSecond(), "] STATUS: ", NodeName, " NodeState: ", NodeState, " FragmentLevel: ", Level, " FragName: ", FragName, " EdgeOrddict:", EdgeOrddict]),
  Message2 = lists:concat([" BestEdge: ", BestEdge, " BestWT: ", BestWT, " TestEdge: ", TestEdge, " InBanch: ", InBranch, " FindCount: ", FindCount, "\n"]),
  Message = string:concat(Message1, Message2),
  Filename = lists:concat(["logs/", NodeName, "@", getHostname(), ".log"]),
  werkzeug:logging(Filename, Message)
.
