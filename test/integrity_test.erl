-module(integrity_test).

-include_lib("eunit/include/eunit.hrl").

new_test() ->
    ?assertMatch(<<"http://myhost.com:8080/my/path?color=red#Section%205">>,
        liburi:to_string(liburi:new(<<"http">>, <<>>, <<"myhost.com">>, 8080, <<"my/path">>, <<"color=red">>, <<"Section 5">>))).

append_path_test() ->
    T0 = liburi:new(<<"http">>, <<"">>, <<"myhost.com">>, 8080, <<"/my/path">>, <<"color=red">>, <<"Section 5">>),
    T1 = liburi:append_path(T0, <<"additional/path">>),
    ?assertMatch(<<"http://myhost.com:8080/my/path/additional/path?color=red#Section%205">>, liburi:to_string(T1)).

parse_scheme_test() ->
    ?assertMatch({<<"http">>, <<"//test.com/">>}, liburi_parser:parse_scheme(<<"http://test.com/">>)),
    ?assertMatch({<<>>, <<"/test">>}, liburi_parser:parse_scheme(<<"/test">>)),
    ?assertMatch({<<"mailto">>, <<"x@test.com">>}, liburi_parser:parse_scheme(<<"mailto:x@test.com">>)).

parse_authority_test() ->
    ?assertMatch({<<"test.com">>, <<"/here">>}, liburi_parser:parse_authority(<<"//test.com/here">>)),
    ?assertMatch({<<"test.com">>, <<"">>}, liburi_parser:parse_authority(<<"//test.com">>)),
    ?assertMatch({<<>>, <<"/test">>}, liburi_parser:parse_authority(<<"/test">>)).

parse_user_info_test() ->
    ?assertMatch({<<"user">>, <<"test.com">>}, liburi_parser:parse_user_info(<<"user@test.com">>)),
    ?assertMatch({<<"">>, <<"user.test.com">>}, liburi_parser:parse_user_info(<<"user.test.com">>)).

parse_host_port_test() ->
    ?assertMatch({<<"test.com">>, 8080}, liburi_parser:parse_host_port(<<"test.com:8080">>)),
    ?assertMatch({<<"test.com">>, undefined}, liburi_parser:parse_host_port(<<"test.com">>)).

parse_path_test() ->
    ?assertMatch({<<"/a/b/c">>, <<"">>}, liburi_parser:parse_path(<<"/a/b/c">>)),
    ?assertMatch({<<"/a/b/c">>, <<"?n=5">>}, liburi_parser:parse_path(<<"/a/b/c?n=5">>)),
    ?assertMatch({<<"/a/b/c">>, <<"#anchor">>}, liburi_parser:parse_path(<<"/a/b/c#anchor">>)),
    ?assertMatch({<<"">>, <<"">>}, liburi_parser:parse_path(<<"">>)).

parse_query_test() ->
    ?assertMatch({<<"a=b">>, <<"">>}, liburi_parser:parse_query(<<"?a=b">>)),
    ?assertMatch({<<"a=b">>, <<"#anchor">>}, liburi_parser:parse_query(<<"?a=b#anchor">>)),
    ?assertMatch({<<"">>, <<"#anchor">>}, liburi_parser:parse_query(<<"#anchor">>)),
    ?assertMatch({<<"">>, <<"">>}, liburi_parser:parse_query(<<"">>)).

query_to_proplist_test() ->
    ?assertMatch([], liburi:query_to_proplist(<<>>)),
    ?assertMatch([{<<"a">>, <<"b">>}], liburi:query_to_proplist(<<"a=b&">>)),
    ?assertMatch([{<<"a">>, <<>>}], liburi:query_to_proplist(<<"a=">>)),
    ?assertMatch([{<<"a">>, null}, {<<"b">>, <<"c">>}], liburi:query_to_proplist(<<"a&b=c">>)),
    ?assertMatch([{<<"a&b">>, <<"!t=f">>}], liburi:query_to_proplist(<<"a%26b=!t%3Df">>)).

to_query_test() ->
    ?assertMatch(<<"one&two=2&three=two%20%2B%20one">>, liburi:to_query([one, {<<"two">>, 2}, {<<"three">>, <<"two + one">>}])).

proplist_query_test() ->
    QueryPropList = [{<<"foo">>, <<"bar">>}, {<<"baz">>, <<"back">>}],
    Uri0 = liburi:from_string(<<"http://myhost.com:8080/my/path?color=red#Section%205">>),
    Uri1 = liburi:q(Uri0, QueryPropList),
    ?assertMatch(<<"http://myhost.com:8080/my/path?foo=bar&baz=back#Section%205">>, liburi:to_string(Uri1)).

unquote_test() ->
    ?assertMatch(<<"ab">>, liburi:unquote(<<"ab">>)),
    ?assertMatch(<<"a b">>, liburi:unquote(<<"a+b">>)),
    ?assertMatch(<<"a b">>, liburi:unquote(<<"a%20b">>)).

quote_test() ->
    ?assertMatch(<<"abc123">>, liburi:quote(<<"abc123">>)),
    ?assertMatch(<<"abc%20123">>, liburi:quote(<<"abc 123">>)).

escape_test() ->
    ?assertMatch(<<"%20">>, liburi_utils:escape($\s)).
