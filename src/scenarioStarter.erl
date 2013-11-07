%%%-------------------------------------------------------------------
%%% @author Matthias
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Nov 2013 15:42
%%%-------------------------------------------------------------------
-module(scenarioStarter).
-author("Matthias").

%% API
-export([scenario1/0]).

scenario1() ->
  mstNode:start(node1),
  mstNode:start(node2),
  mstNode:start(node3),
  mstNode:start(node4),
  mstNode:start(node5)
.
