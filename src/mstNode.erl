%%%-------------------------------------------------------------------
%%% @author Matthias Stellmann and Grzegorz Markiewicz
%%% @copyright (C) 2013, HAW Hamburg
%%% @doc
%%%   Public starting module of a node.
%%% @end
%%% Created : 03. Nov 2013
%%%-------------------------------------------------------------------
-module(mstNode).
-author("StellmannMarkiewicz").

%% API
-export([start/1]).

%% @doc
%%  This function starts a node in the sleeping state.
%%  Nodename:   Name of the node as a String
start(Nodename) ->
  Filepath = lists:concat(["nodeconfigs/", Nodename, ".cfg"]),
  {ok, EdgeList} = file:consult(Filepath),
  EdgeOrddict = createEdgeOrddict(EdgeList, orddict:new()),
  NodePID = erlang:spawn(fun() -> controller:main(sleeping, 0, 0, EdgeOrddict, Nodename, 0) end),
  register(Nodename, NodePID),
  {NodePID, node(NodePID)}
.

%% @private
%% @doc
%%  This function creates an orddict for the edges known by this node.
%%  EdgeList:     Input - List created of node.cfg
%%  EdgeOrddict:  To be created: Returnvalue of this Function
%%                Key = Weight(Edge)
%%                Value = {Nodename, Nodestate}
createEdgeOrddict(EdgeList, EdgeOrddict) ->
  case EdgeList == [] of
    true ->
      EdgeOrddict;
    _ ->
      [Head|Tail] = EdgeList,
      HeadElem1 = element(1, Head),
      HeadElem2 = element(2, Head),
      NewEdgeOrddict = orddict:store(HeadElem1, {HeadElem2, basic}, EdgeOrddict),
      createEdgeOrddict(Tail, NewEdgeOrddict)
  end
.
