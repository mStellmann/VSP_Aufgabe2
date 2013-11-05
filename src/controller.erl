%%%-------------------------------------------------------------------
%%% @author Matthias Stellmann and Grzegorz Markiewicz
%%% @copyright (C) 2013, HAW Hamburg
%%% @doc
%%%   TODO writing the doc
%%% @private
%%% @end
%%% Created : 03. Nov 2013
%%%-------------------------------------------------------------------
-module(controller).
-author("StellmannMarkiewicz").


main(OwnNodeState, OwnLevel, OwnFragName, OwnEdgeList, OwnNodeName) ->
  receive
    wakeup when OwnNodeState == sleeping -> sleeping:wakeup(OwnEdgeList, OwnNodeName),
      main(found, OwnLevel, OwnFragName, OwnEdgeList, OwnNodeName);

    {initiate, Level, FragName, NodeState, Edge} -> doSomething;
    {test, Level, FragName, Edge} -> doSomething;
    {accept, Edge} -> doSomething;
    {reject, Edge} -> doSomething;
    {report, Weight, Edge} -> doSomething;
    {changeroot, Edge} -> doSomething;
    {connect, Level, Edge} -> doSomething
  end

.

%% API
-export([]).
