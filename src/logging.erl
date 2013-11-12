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
-export([logMessage/2, logStatus/9, logDebug/1]).

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
  Filename = lists:concat(["logs/log@", getHostname(), ".log"]),
  werkzeug:logging(Filename, lists:concat(["[", werkzeug:timeMilliSecond(), "] ", NodeName, " - ", Message, "\n"]))
.

logStatus(NodeState, Level, FragName, NodeName, BestEdge, BestWT, TestEdge, InBranch, FindCount) ->
  Message = io_lib:format("[STATUS] - ~p | NodeState: ~p | FragmentLevel: ~p | FragName: ~p | BestEdge: ~p | BestWT: ~p | TestEdge: ~p | InBranch: ~p | FindCount: ~p~n", [NodeName, NodeState, Level, FragName, BestEdge, BestWT, TestEdge, InBranch, FindCount]),
  Filename = lists:concat(["logs/log@", getHostname(), ".log"]),
  werkzeug:logging(Filename, Message)
.

logDebug(Message) ->
  Filename = lists:concat(["logs/", "DEBUG: ", "@", getHostname(), ".log"]),
  werkzeug:logging(Filename, lists:concat(["[", werkzeug:timeMilliSecond(), "] ", "DEBUG: ", " - ", Message, "\n"]))
.
