%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(release).    
    
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%-include("log.hrl").

%%---------------------------------------------------------------------
%% Records for test
%%


%% --------------------------------------------------------------------
%-compile(export_all).

-export([
	 start/1
	]).


%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

-define(Path,"https://github.com/joq62/git_test.git").
-define(Id,"git_test").

start([Id,ReleaseFile])->
    RelId=Id,
    Path="https://github.com/joq62/"++Id++".git",
    File=ReleaseFile,
    {ok,I1}=file:consult(File),
    L1=[{XId,TagList,XPath}||{XId,TagList,XPath}<-I1,
					  XId=:=RelId],
    TagString=os:cmd("git tag"),
    Tags1=string:tokens(TagString,"\n"), 
    case L1 of
	[]->
	    I2=lists:sort([{RelId,Tags1,Path}|I1]),
	    unconsult(File,I2);
	[{RelId,TagsX,_Path1}]->
	    case lists:sort(TagsX)=:=lists:sort(Tags1) of
		true->
		    ok;
		false->
		    I2=lists:sort(lists:keyreplace(RelId,1,I1,{RelId,Tags1,Path})),
		    unconsult(File,I2)
	    end
    end,
    io:format("file:consult(File) ~p~n",[{file:consult(File),?FUNCTION_NAME,?MODULE,?LINE}]),    
    ok.
    
   
unconsult(File,L)->
    {ok,S}=file:open(File,write),
    lists:foreach(fun(X)->
			  io:format(S,"~p.~n",[X]) end,L),
    file:close(S).
