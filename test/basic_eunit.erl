%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Node end point  
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(basic_eunit).   
 
-export([start/0]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
    ok=application:start(service_app),
    pong=service:ping(),

    ok=load_test(),
    ok=start_test(),
    pong=mydivi:ping(),
    
    
    {error,[{error,{already_started,divi_app}}]}=start_test(),
    {error,[already_loaded]}=load_test(),
    ok=stop_test(),
    {error,[not_started]}=stop_test(),
    ok=unload_test(),

    ok=load_test(),
    ok=start_test(),
    pong=mydivi:ping(),
    
    ok=stop_test(),
    ok=unload_test(),

    service:stop(),
    timer:sleep(1000),
 %   ok=load_start_test(),
 
    init:stop(),
    ok.


setup_test()->
    ok.

stop_test()->
    AppId="divi_app",
    Vsn="1.0.0",
    service:stop(AppId,Vsn).
unload_test()->
    AppId="divi_app",
    Vsn="1.0.0",
    service:unload(AppId,Vsn).
   

load_start_test()->
    
    ok.

start_test()->
    AppId="divi_app",
    Vsn="1.0.0",
    service:start(AppId,Vsn).

load_test()->
    AppId="divi_app",
    Vsn="1.0.0",
    GitPath="https://github.com/joq62/divi_app.git",
    service:load(AppId,Vsn,GitPath).



%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------

setup()->
  
    % Simulate host
    R=rpc:call(node(),test_nodes,start_nodes,[],2000),
%    [Vm1|_]=test_nodes:get_nodes(),

%    Ebin="ebin",
 %   true=rpc:call(Vm1,code,add_path,[Ebin],5000),
 
    R.
