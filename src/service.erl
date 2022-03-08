%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(service). 

-behaviour(gen_server). 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
-define(SERVER,?MODULE).


%% External exports
-export([
	 load/3,
	 start/2,
	 stop/2,
	 unload/2,
	 
	 id/0,
	 template/0,

	 read_state/0,
	 ping/0
	]).


-export([
	 start/0,
	 stop/0
	]).


-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
		loaded,
		started,
		service_dir,
		id,
		template
		
	       }).

%% ====================================================================
%% External functions
%% ====================================================================


%% ====================================================================
%% Server functions
%% ====================================================================
%% Gen server functions

start()-> gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).
stop()-> gen_server:call(?SERVER, {stop},infinity).

%% ====================================================================
%% Application handling
%% ====================================================================

%%---------------------------------------------------------------
%% Function:load(AppId,Vsn,GitPath)
%% @doc:git clone AppId    
%% @param: AppId,Vsn,GitPath
%% @returns:ok|{error,Reason}
%%
%%---------------------------------------------------------------
-spec load(string(),string(),string())-> atom()|{string(),string()}.
load(AppId,Vsn,GitPath)->
    gen_server:call(?SERVER,{load,AppId,Vsn,GitPath},infinity).

%%---------------------------------------------------------------
%% Function:start(AppId,Vsn)
%% @doc:start loaded application    
%% @param: AppId,Vsn
%% @returns:ok|{error,Reason}
%%
%%---------------------------------------------------------------
-spec start(string(),string())-> atom()|{string(),string()}.
start(AppId,Vsn)->
    gen_server:call(?SERVER,{start,AppId,Vsn},infinity).

%%---------------------------------------------------------------
%% Function:stop(AppId,Vsn)
%% @doc:start loaded application    
%% @param: AppId,Vsn
%% @returns:ok|{error,Reason}
%%
%%---------------------------------------------------------------
-spec stop(string(),string())-> atom()|{string(),string()}.
stop(AppId,Vsn)->
    gen_server:call(?SERVER,{stop,AppId,Vsn},infinity).

%%---------------------------------------------------------------
%% Function:unload(AppId,Vsn)
%% @doc:start loaded application    
%% @param: AppId,Vsn
%% @returns:ok|{error,Reason}
%%
%%---------------------------------------------------------------
-spec unload(string(),string())-> atom()|{string(),string()}.
unload(AppId,Vsn)->
    gen_server:call(?SERVER,{unload,AppId,Vsn},infinity).


%%---------------------------------------------------------------
%% Function:id()
%% @doc: service id == service spec        
%% @param: non
%% @returns:{Name,Vsn}
%%
%%---------------------------------------------------------------
-spec id()-> {string(),string()}.
id()->
    gen_server:call(?SERVER, {id},infinity).

%%---------------------------------------------------------------
%% Function:template()
%% @doc: service spec template  list of {app,vsn} to run      
%% @param: 
%% @returns:[{app,vsn}]
%%
%%---------------------------------------------------------------
-spec template()-> [{atom(),string()}].
template()->
    gen_server:call(?SERVER, {template},infinity).


%% ====================================================================
%% Support functions
%% ====================================================================
%%---------------------------------------------------------------
%% Function:read_state()
%% @doc: read theServer State variable      
%% @param: non 
%% @returns:State
%%
%%---------------------------------------------------------------
-spec read_state()-> term().
read_state()->
    gen_server:call(?SERVER, {read_state},infinity).
%% 
%% @doc:check if service is running
%% @param: non
%% @returns:{pong,node,module}|{badrpc,Reason}
%%
-spec ping()-> {atom(),node(),module()}|{atom(),term()}.
ping()-> 
    gen_server:call(?SERVER, {ping},infinity).

%% ====================================================================
%% Gen Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    {ok,HostName}=net:gethostname(),
    ServiceDir=HostName++".service_dir",  
    os:cmd("rm -rf "++ServiceDir),
    ok=file:make_dir(ServiceDir),
    {ok, #state{service_dir=ServiceDir,
		loaded=[],
		started=[]}
    }.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_call({start,AppId,Vsn},_From, State) ->
    Reply=case lib_service:start(AppId,Vsn) of
	      ok->
		  NewState=State#state{started=[{AppId,Vsn}|State#state.started]},	  
		  ok;
	      Err->
		  NewState=State,
		  {error,[Err]}
	  end,
    {reply, Reply, NewState};

handle_call({stop,AppId,Vsn},_From, State) ->
    Reply=case lists:member({AppId,Vsn},State#state.started) of
	      true->
		  case lib_service:stop(AppId,Vsn) of
		      ok->
			  NewState=State#state{started=lists:delete({AppId,Vsn},State#state.started)},
			  ok;
		      {error,Reason}->
			  NewState=State,
			   {error,Reason}
		  end;
	      false->
		  NewState=State,
		  {error,[not_started]}
	  end,
    {reply, Reply, NewState};


handle_call({load,AppId,Vsn,GitPath},_From, State) ->
    Reply=case lists:keymember({AppId,Vsn},1,State#state.loaded) of
	       false->
		   case lib_service:load(AppId,Vsn,GitPath,State#state.service_dir) of
		       {ok,AppDir}->
			   NewState=State#state{loaded=[{{AppId,Vsn},GitPath,AppDir}|State#state.loaded]},
			   ok;
		       {error,Reason}->
			   NewState=State,
			   {error,Reason}
		   end;
	       true->
		   NewState=State,
		   {error,[already_loaded]}
	   end,
    {reply, Reply, NewState};


handle_call({unload,AppId,Vsn},_From, State) ->
    Reply=case lists:keyfind({AppId,Vsn},1,State#state.loaded) of
	      {{AppId,Vsn},_GitPath,AppDir}->
		  case lib_service:unload(AppId,Vsn,AppDir) of
		      ok->
			  NewState=State#state{loaded=lists:keydelete({AppId,Vsn},1,State#state.loaded)},
			  ok;
		       {error,Reason}->
			   NewState=State,
			   {error,Reason}
		   end;
	       true->
		   NewState=State,
		   {error,[already_loaded]}
	   end,
    {reply, Reply, NewState};


handle_call({id},_From, State) ->
    Reply=State#state.id,
    {reply, Reply, State};

handle_call({template},_From, State) ->
    Reply=State#state.template,
    {reply, Reply, State};

handle_call({read_state},_From, State) ->
    Reply=State,
    {reply, Reply, State};
handle_call({ping},_From, State) ->
    Reply=pong,
    {reply, Reply, State};

handle_call({stopped},_From, State) ->
    Reply=ok,
    {reply, Reply, State};




handle_call({not_implemented},_From, State) ->
    Reply=not_implemented,
    {reply, Reply, State};

handle_call({stop}, _From, State) ->
    os:cmd("rm -rf "++State#state.service_dir),
    timer:sleep(3000),
    {stop, normal, shutdown_ok, State};

handle_call(Request, From, State) ->
    %rpc:cast(node(),log,log,[?Log_ticket("unmatched call",[Request, From])]),
    Reply = {ticket,"unmatched call",Request, From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------


handle_cast(_Msg, State) ->
    %rpc:cast(node(),log,log,[?Log_ticket("unmatched cast",[Msg])]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    %rpc:cast(node(),log,log,[?Log_ticket("unmatched info",[Info])]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

		  
