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
-module(scenarioStarter).
-author("StellmannMarkiewicz").

%% API
-export([scenario1/0]).

%%
%% @doc
%%  Starts the first scenario with 5 nodes
scenario1() ->
  mstNode:start(node1),
  mstNode:start(node2),
  mstNode:start(node3),
  mstNode:start(node4),
  mstNode:start(node5)
.
