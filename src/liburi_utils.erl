-module(liburi_utils).

-export([
    quote/2,
    unquote/1,
    escape/1,
    to_binary/1
]).

quote(Str, Part) ->
    binary_foldl(fun (C, Acc) -> Escaped = escape_for_part(C, Part), <<Acc/binary, Escaped/binary>> end, <<>>, Str).

unquote(Str) ->
    unquote(Str, <<>>).

escape(C) ->
    erlang:iolist_to_binary(io_lib:format("%~2.16.0B", [C])).

to_binary(V) when is_binary(V) ->
    V;
to_binary(V) when is_list(V) ->
    iolist_to_binary(V);
to_binary(V) when is_atom(V) ->
    atom_to_binary(V, utf8);
to_binary(V) when is_integer(V) ->
    integer_to_binary(V);
to_binary(V) when is_float(V) ->
    float_to_binary(V, [{decimals, 8}, compact]).

% internals

unquote(<<>>, Acc) ->
    Acc;
unquote(<<$+, Str/binary>>, Acc) ->
    unquote(Str, <<Acc/binary, $\s>>);
unquote(<<$\%, A, B, Str/binary>>, Acc) ->
    Char = erlang:list_to_integer([A, B], 16),
    unquote(Str, <<Acc/binary, Char>>);
unquote(<<C, Str/binary>>, Acc) ->
    unquote(Str, <<Acc/binary, C/integer>>).

binary_foldl(_Fun, Acc0, <<>>) ->
    Acc0;
binary_foldl(Fun, Acc0, <<H, T/binary>>) ->
    Acc1 = Fun(H, Acc0),
    binary_foldl(Fun, Acc1, T).

escape_for_part(C, Part) ->
    IsReserved = case Part of
        any ->
            is_unreserved(C);
        userinfo ->
            is_userinfo(C);
        path ->
            is_pchar(C) orelse C == $; orelse C == $/;
        segment ->
            is_pchar(C) orelse C == $;;
        segment_param ->
            is_pchar(C);
        query_ ->
            is_unreserved(C);
        q ->
            is_unreserved(C);
        fragment ->
            is_unreserved(C);
        frag ->
            is_unreserved(C)
    end,
    case IsReserved of
        true ->
            <<C>>;
        false ->
            escape(C)
    end.

is_unreserved(C) ->
    is_alphanum(C) orelse is_mark(C).
is_alphanum(C) ->
    is_alpha(C) orelse is_digit(C).
is_alpha(C) ->
    is_lowalpha(C) orelse is_upalpha(C).
is_lowalpha(C) ->
    $a =< C andalso C =< $z.
is_upalpha(C) ->
    $A =< C andalso C =< $Z.
is_digit(C) ->
    $0 =< C andalso C =< $9.

is_pchar($:) ->
    true;
is_pchar($@) ->
    true;
is_pchar($&) ->
    true;
is_pchar($=) ->
    true;
is_pchar($+) ->
    true;
is_pchar($$) ->
    true;
is_pchar($,) ->
    true;
is_pchar(C)  ->
    is_unreserved(C).

is_userinfo($;) ->
    true;
is_userinfo($:) ->
    true;
is_userinfo($&) ->
    true;
is_userinfo($=) ->
    true;
is_userinfo($+) ->
    true;
is_userinfo($$) ->
    true;
is_userinfo($,) ->
    true;
is_userinfo(C)  ->
    is_unreserved(C).

is_mark($-) ->
    true;
is_mark($_) ->
    true;
is_mark($.) ->
    true;
is_mark($!) ->
    true;
is_mark($~) ->
    true;
is_mark($*) ->
    true;
is_mark($\') ->
    true;
is_mark($() ->
    true;
is_mark($)) ->
    true;
is_mark(_) ->
    false.
