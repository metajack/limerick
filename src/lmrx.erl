-module(lmrx).
-author("Jack Moffitt <jack@metajack.im>").

-include_lib("xmerl/include/xmerl.hrl").

-export([get_aval/2, get_aval_int/2, get_child/2]).

get_aval(Name, #xmlElement{attributes=Attrs}) ->
    get_aval(Name, Attrs);
get_aval(Name, Attrs) ->
    Vals = [Attr#xmlAttribute.value || 
               Attr = #xmlAttribute{name=N} <- Attrs, N =:= Name],
    case Vals of
        [Val] ->
            Val;
        [] ->
            undefined
    end.

get_aval_int(Name, Elem) ->
    case get_aval(Name, Elem) of
        undefined ->
            undefined;
        L ->
            list_to_integer(L)
    end.

get_child(Name, #xmlElement{content=Content}) ->
    Children = [E || E = #xmlElement{name=N} <- Content, N =:= Name],
    case Children of
        [] ->
            undefined;
        [First | _] ->
            First
    end.
