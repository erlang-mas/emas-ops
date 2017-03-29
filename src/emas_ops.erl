%%%-----------------------------------------------------------------------------
%%% @doc EMAS runner script.
%%% @end
%%%-----------------------------------------------------------------------------

-module(emas_ops).

-include("emas_ops.hrl").

%%% API
-export([main/1, start/1]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

main([]) ->
    usage();
main(Args) ->
    OptSpecList = option_spec_list(),
    case getopt:parse(OptSpecList, Args) of
        {ok, {Opts, _NonOptArgs}} ->
            start(Opts);
        {error, {Reason, Data}} ->
            io:format("Error: ~s ~p~n~n", [Reason, Data]),
            usage()
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
start(Opts) ->
    setup_distribution(),
    application:load(mas),
    application:load(emas),
    application:set_env(mas, population_mod, emas_population),
    application:set_env(mas, simulation_mod, emas_simulation),
    setup_app_env(mas, Opts),
    setup_app_env(emas, Opts),
    SP = emas_config:fetch_all(),
    Time = get_opt(time, Opts),
    mas:start(),
    mas:start_simulation(SP, Time),
    handle_result().

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
setup_distribution() ->
    {ok, _} = net_kernel:start([generate_node_name(), shortnames]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
generate_node_name() ->
    Name = emas_utils:format("emas-~3..0B", [rand:uniform(100)]),
    list_to_atom(Name).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
setup_app_env(App, Opts) ->
    Props = [element(1, Spec) || Spec <- option_spec_list(App)],
    [set_app_prop(App, proplists:lookup(Prop, Opts)) || Prop <- Props].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
set_app_prop(App, {Key, Value}) ->
    application:set_env(App, Key, Value);
set_app_prop(_App, none) -> none.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_opt(Key, Opts) ->
    proplists:get_value(Key, Opts).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_result() ->
    receive
        {result, Result} ->
            io:format("Simulation result: ~p~n", [Result]);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason]);
        _ -> io:format("Unknown simulation result~n", [])
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
usage() ->
    getopt:usage(option_spec_list(), escript:script_name()).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
option_spec_list() ->
    Spec = option_spec_list(mas) ++ option_spec_list(emas),
    lists:usort(Spec).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
option_spec_list(mas) ->
    [
     {population_count,             $i,         "population-count",             integer,    "Number of populations (islands)"},
     {population_size,              undefined,  "population-size",              integer,    "Size of single population"},
     {migration_probability,        undefined,  "migration-probability",        float,      "Migration probability"},
     {node_migration_probability,   undefined,  "node-migration-probability",   float,      "Node migration probability"},
     {topology,                     $t,         "topology",                     atom,       "Topology of connections between populations"},
     {nodes_topology,               undefined,  "nodes-topology",               atom,       "Topology of connections between nodes"},
     {logs_dir,                     $o,         "output",                       string,     "Logs output directory"}
    ];
option_spec_list(emas) ->
    [
     {time,         $t, "time",         integer,    "Duration of the simulation"},
     {problem_size, $s, "problem-size", integer,    "Problem size"}
    ].
