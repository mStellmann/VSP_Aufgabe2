%%%-------------------------------------------------------------------
%%% @author Matthias Stellmann and Grzegorz Markiewicz
%%% @copyright (C) 2013, HAW Hamburg
%%% @doc
%%%   TODO writing the doc
%%% @private
%%% @end
%%% Created : 03. Nov 2013
%%%-------------------------------------------------------------------
-module(sleeping).
-author("StellmannMarkiewicz").

%% API
-export([]).


%% @doc
%% This function is called from the sleeping state of the node.
%% It searches for the akmg and sends the connect msg to the node
%% on the opposite side of the akmg. Afterwards the node changes into
%% the sleeping state and waits for a response from the other node.
%%    EdgeList:     Orddict where the key is the EdgeWeight and value
%%                  is a tuple {OtherNodeName,EdgeState}
%%    OwnNodeName:  The system known name of our own node
wakeup(EdgeList, OwnNodeName)->
  EdgeWeights = orddict:fetch_keys(EdgeList),
    Akmg = lists:min(EdgeWeights),
  % Value for the key (EdgeNumber) that is a Tuple {OtherNodeName, EdgeState}
   EdgeToMark = orddict:fetch(Akmg, EdgeList),
   OtherNodeName = element(1,EdgeToMark),
   MarkedEdge = {OtherNodeName,branch},
   NewEdgeList = orddict:store(Akmg,MarkedEdge,EdgeList),
   OtherNodeName ! {connect, 0, {Akmg,OwnNodeName,OtherNodeName}},
   ok
.


