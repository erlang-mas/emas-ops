%%%-----------------------------------------------------------------------------
%%% @doc Genetic operators for Rastrigin function optimization problem.
%%% @end
%%%-----------------------------------------------------------------------------

-module(emas_ops_rastrigin).

-include("emas_ops.hrl").

-behaviour(emas_genetic_ops).

%%% API
-export ([evaluation/2,
          mutation/2,
          recombination/3,
          solution/1,
          config/0]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Generates a random solution, as a vector of numbers in range [-50, 50].
%% @end
%%------------------------------------------------------------------------------
solution(SP) ->
    S = [-50 + rand:uniform() * 100 || _ <- lists:seq(1, SP#sim_params.problem_size)],
    erlang:term_to_binary(S).

%%------------------------------------------------------------------------------
%% @doc Evaluates given solution by computing the Rastrigin function.
%% @end
%%------------------------------------------------------------------------------
evaluation(B, _SP) ->
    S = erlang:binary_to_term(B),
    - lists:foldl(fun(X, Sum) -> Sum + 10 + X*X - 10*math:cos(2*math:pi()*X) end, 0.0, S).

%%------------------------------------------------------------------------------
%% @doc Continuously recombines every pair of features for the given pair of
%%      solutions.
%% @end
%%------------------------------------------------------------------------------
recombination(B1, B2, _SP) ->
    S1 = erlang:binary_to_term(B1),
    S2 = erlang:binary_to_term(B2),
    {S3, S4} = lists:unzip([recombination_features(F1, F2) || {F1, F2} <- lists:zip(S1,S2)]),
    {erlang:term_to_binary(S3), erlang:term_to_binary(S4)}.

%%------------------------------------------------------------------------------
%% @doc Mutates the features at random indices
%% @end
%%------------------------------------------------------------------------------
mutation(B, SP) ->
    S = erlang:binary_to_term(B),
    NrGenesMutated = emas_utils:average_number(SP#sim_params.mutation_rate, S),
    Indexes = [rand:uniform(length(S)) || _ <- lists:seq(1, NrGenesMutated)], % indices may be duplicated
    Mut = mutate_genes(S, lists:usort(Indexes), 1, [], SP), % usort removes duplicates
    erlang:term_to_binary(Mut).

%%------------------------------------------------------------------------------
%% @doc Operators additional config.
%% @end
%%------------------------------------------------------------------------------
config() ->
    undefined.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
recombination_features(F1, F2) ->
    A = erlang:min(F1, F2),
    B = (erlang:max(F1, F2) - erlang:min(F1, F2)),
    {A + rand:uniform() * B,A + rand:uniform() * B}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
mutate_genes(RestOfSolution, [], _, Acc, _SP) ->
    lists:reverse(Acc, RestOfSolution);
mutate_genes([], [_|_], _, _, _) ->
    erlang:error(tooManyIndexes);
mutate_genes([Gene|Solution], [I|Indexes], I, Acc, SP) ->
    mutate_genes(Solution, Indexes, I+1, [mutate_feature(Gene, SP)|Acc], SP);
mutate_genes([Gene|Solution], [I|Indexes], Inc, Acc, SP) ->
    mutate_genes(Solution, [I|Indexes], Inc+1, [Gene|Acc], SP).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
mutate_feature(F, SP) ->
    Range = SP#sim_params.mutation_range * case rand:uniform() of
                                         X when X < 0.2 -> 5.0;
                                         X when X < 0.4 -> 0.2;
                                         _ -> 1.0
                                     end,
    F + Range * math:tan(math:pi()*(rand:uniform() - 0.5)).
