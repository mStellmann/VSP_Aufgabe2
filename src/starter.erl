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
-module(starter).
-author("StellmannMarkiewicz").

%% API
-export([scenario1/0, scenario2/0, scenario3/0]).


%% @doc
%%  Starts the first scenario with 5 nodes
scenario1() ->
  mstNode:start(node1, 1),
  mstNode:start(node2, 1),
  mstNode:start(node3, 1),
  mstNode:start(node4, 1),
  mstNode:start(node5, 1)
.

%% @doc
%%  Starts the second scenario with 7 nodes (Aufgabenblatt)
scenario2() ->
  mstNode:start(node1, 2),
  mstNode:start(node2, 2),
  mstNode:start(node3, 2),
  mstNode:start(node4, 2),
  mstNode:start(node5, 2),
  mstNode:start(node6, 2),
  mstNode:start(node0, 2)
.

%% @doc
%%  Starts the second scenario with 4 nodes (Undirected Graph V4)
scenario3() ->
  mstNode:start(node1, 3),
  mstNode:start(node2, 3),
  mstNode:start(node3, 3),
  mstNode:start(node0, 3)
.