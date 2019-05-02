-module(aeeqc_eunit).
-export([props_mod_test_repr/1]).

-define(REV_SUFFIX, "stset_").

props_mod_test_repr(TestsModS) ->
    PropsMod = list_to_atom(props_mod(TestsModS)),
    {setup,
     fun() -> eqc:start() end,
     fun(_) -> eqc:stop() end,
     lists:map(
       fun(PropName) -> prop_test_repr(PropsMod, PropName) end,
       aeeqc_props:prop_names(PropsMod))
     }.

props_mod(TestsModS) ->
    ?REV_SUFFIX = lists:reverse("_tests"),
    ?REV_SUFFIX ++ RevPropsModS = lists:reverse(TestsModS),
    lists:reverse(RevPropsModS).

prop_test_repr(Mod, Name) ->
    prop_test_repr(Mod, Name, 500).

prop_test_repr(Mod, Name, Ms) ->
    {atom_to_list(Name),
     {timeout, (Ms * 3) / 1000,
      fun() -> true = eqc:quickcheck(eqc:testing_time(Ms / 1000, Mod:Name())) end
     }}.
