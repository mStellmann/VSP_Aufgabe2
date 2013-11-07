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
-export([logMessage/2]).

%% @private
%% @doc
%%  The Function returns the hostname of the current system.
getHostname() ->
  {ok, Hostname} = inet:gethostname(),
  Hostname
.

%% @doc
%%  The Function logs the given Message into a .log-File and writes it on the console.
logMessage(Nodename, Message) ->
  Filename = lists:concat(["logs/", Nodename, "@", getHostname(), ".log"]),
  werkzeug:logging(Filename, lists:concat(["[", werkzeug:timeMilliSecond(), "] ", Nodename, " - ", Message, "\n"]))
.
