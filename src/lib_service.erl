%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(lib_service).   
    
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%-include("log.hrl").
%-include("configs.hrl").
%%---------------------------------------------------------------------
%% Records for test
%%
%-define(ScheduleInterval,20*1000).
%-define(ConfigsGitPath,"https://github.com/joq62/configs.git").
%-define(ConfigsDir,filename:join(?ApplMgrConfigDir,"configs")).
%-define(ApplicationsDir,filename:join(?ConfigsDir,"applications")).
%-define(ApplMgrConfigDir,"appl_mgr.dir").

%% --------------------------------------------------------------------
%-compile(export_all).

-export([
	 load/4,
	 start/2,
	 stop/2,
	 unload/3,
	 
	 start_template/2
	
	]).


%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
load(AppId,Vsn,GitPath,ServiceDir)->
    AppDir=filename:join(ServiceDir,AppId++"_"++Vsn),
    os:cmd("rm -rf "++AppDir),
    ok=file:make_dir(AppDir),
    TempDir="temp.dir",
    os:cmd("rm -rf "++TempDir),
    ok=file:make_dir(TempDir),
    os:cmd("git clone "++GitPath++" "++TempDir),
    os:cmd("mv  "++TempDir++"/*"++" "++AppDir),
    os:cmd("rm -rf "++TempDir),
    Ebin=filename:join(AppDir,"ebin"),
    Reply=case filelib:is_dir(Ebin) of
	      true->
		  case code:add_patha(Ebin) of
		      true->
			  {ok,AppDir};
		      Err ->
			  {error,[Err]}
		  end;
	      false ->
		  {error,[no_dir_created,?MODULE,?LINE]}
	  end,
    Reply.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
unload(AppId,_Vsn,AppDir)->
    App=list_to_atom(AppId),
    application:unload(App),
    os:cmd("rm -rf "++AppDir),
    ok.
%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
start(AppId,_Vsn)->
    App=list_to_atom(AppId),
    application:start(App).

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
stop(AppId,_Vsn)->
    App=list_to_atom(AppId),
    application:stop(App).

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
start_template(Template,LoaderVm)->
    start_template(Template,LoaderVm,[]).

start_template([],_LoaderVm,StartRes)->
    case [{error,Reason}||{error,Reason}<-StartRes] of
	[]->
	    {ok,StartRes};
	ErrorList ->
	    {error,ErrorList}
    end;
start_template([{App,Vsn}|T],LoaderVm,Acc)->
    Result=case rpc:call(LoaderVm,loader,load_appl,[App,node()],30*1000) of
	       {error,Reason}->
		   {error,[Reason,App,Vsn]};
	       ok->
		   case rpc:call(LoaderVm,loader,start_appl,[App,node()],30*1000) of
		        {error,Reason}->
			   {error,[Reason,App,Vsn]};
		       ok->
			   case rpc:call(node(),App,ping,[],5*1000) of
			       pong->
				   {ok,[App,Vsn]};
			       {badrpc,Reason}->
				   {error,[{badrpc,Reason},App,Vsn]};
			       {error,Reason}->
				   {error,[Reason,App,Vsn]}
			   end
		   end
	   end,
    start_template(T,LoaderVm,[Result|Acc]).
				   



%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
