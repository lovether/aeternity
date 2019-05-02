-module(aeeqc_props).
-export([prop_names/1]).

prop_names(Mod) ->
    lists:map(
      fun({Name, 0}) when is_atom(Name) -> Name end,
      lists:filter(
        fun is_property/1,
        Mod:module_info(exports))).

is_property({Name, Arity}) when is_integer(Arity), Arity > 0,
                                is_atom(Name) ->
    false;
is_property({Name, 0}) ->
    case atom_to_list(Name) of
        "prop_" ++ _ ->
            true;
        _ ->
            false
    end.
