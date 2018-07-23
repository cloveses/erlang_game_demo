-module(transform_tool).

%% ====================================================================
%% Include files  
%% ====================================================================


%% ====================================================================
%% API functions
%% ====================================================================
-export([to_list/1,
         to_atom/1]).

to_list(X) when is_list(X) ->
    X;
to_list(X) when is_atom(X) ->
    atom_to_list(X);
to_list(X) when is_binary(X) ->
    binary_to_list(X);
to_list(X) when is_integer(X) ->
    integer_to_list(X);
to_list(X) when is_float(X) ->
    float_to_list(X);
to_list(X) when is_tuple(X) ->
    tuple_to_list(X);
to_list(_) ->
    throw(to_list_error).


to_atom(X) when is_atom(X) ->
    X;
to_atom(X) when is_binary(X) ->
    to_atom(binary_to_list(X));
to_atom(X) when is_list(X) ->
    case catch(list_to_existing_atom(X)) of
        {'EXIT', _} -> 
            list_to_atom(X);
        Atom -> 
            Atom
    end;
to_atom(_) ->
    throw(to_atom_error).

%% ====================================================================
%% Internal functions
%% ====================================================================


