%% -*- erlang-indent-level: 4; indent-tabs-mode: nil; fill-column: 80 -*-
%%% Copyright Erlware, LLC. All Rights Reserved.
%%%
%%% This file is provided to you under the Apache License,
%%% Version 2.0 (the "License"); you may not use this file
%%% except in compliance with the License.  You may obtain
%%% a copy of the License at
%%%
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing,
%%% software distributed under the License is distributed on an
%%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%%% KIND, either express or implied.  See the License for the
%%% specific language governing permissions and limitations
%%% under the License.
%%%-------------------------------------------------------------------
%%% @author Scott Parish <srp@srparish.net>
%%% @copyright Erlware, LLC.
%%% @doc A module for generating, parsing, encoding, and decoding uris.
%%%
%%% At the moment this module isn't very sympathetic to non-http
%%% uri's, but that could/should change in the future.

-module(liburi).

-export([
    new/7,
    from_string/1,
    from_http_1_1/3,
    to_string/1,
    query_foldl/3,
    query_to_proplist/1,
    to_query/1,
    to_query/2,
    quote/1,
    quote/2,
    unquote/1,

    scheme/1,
    scheme/2,
    user_info/1,
    user_info/2,
    host/1,
    host/2,
    port/1,
    port/2,
    path/1,
    path/2,
    append_path/2,
    raw_query/1,
    raw_query/2,
    q/1,
    q/2,
    frag/1,
    frag/2,
    raw/1
]).

-record(uri, {
    scheme::binary(),               % <<"http">>, <<"ftp">>
    user_info= <<"">>::binary(),    % <<>> | <<"srp">>
    host= <<"">>::binary(),         % <<"somewhere.net">>
    port= undefined:: non_neg_integer() | undefined, % undefined | 80 | 8080
    path= <<"">>::binary(),         % <<"/here/there/everytwhere">>
    q=[]::proplists:proplist(),     % The q as a dict
    frag= <<"">>::binary(),         % <<"some anchor">>
    raw= <<"">>::binary()           % original raw uri
}).

% types

-type uri() :: #uri{}.
-type bin_string() :: binary() | list().
-type qs() :: proplists:proplist() | binary().

-export_type([
    uri/0
]).

%%  This is a record that represents the different parts of a uri,
%%  as defined by rfc-2396. It has the following fields:
%%  <dl>
%%   <dt>scheme::binary()</dt>
%%   <dd>`"http"', `"https"', `"ftp"', etc</dd>
%%
%%   <dt>user_info::binary()</dt>
%%   <dd>This will be `"parish:secret"' for the uri
%%       `"http://parish:secret@somehost.com/index.html"'</dd>
%%
%%   <dt>host::binary()</dt>
%%   <dd>This will be `"somehost.com"' for the uri
%%       `"http://somehost.com/index.html"'.</dd>
%%
%%   <dt>port::integer() | undefined</dt>
%%   <dd>This will be `8080' for the uri
%%       `"http://somehost.com:8080/index.html"', and `[]' for
%%       uri `"http://somehost.com/index.html"'.</dd>
%%
%%   <dt>path::binary()</dt>
%%   <dd>This will be `"/index.html"' for the uri
%%       `"http://somehost.com/index.html?startId=50"'. This will
%%       be unquoted, so `"http://somehost.com/name+with%20spaces"'
%%       will be `"/name with spaces"'</dd>
%%
%%   <dt>q::dict()</dt>
%%   <dd> This is a dict of name value pairs from the query. If no query
%%        was found then it is left empty </dd>
%%
%%   <dt>frag::binary()</dt>
%%   <dd>The fragment part of the url, unquoted. This will be
%%       `"Section 5"' for the uri
%%       `"http://somehost.com/index.html#Section+5"'. It will be
%%       The empty string if no fragment is found.</dd>
%%
%%   <dt>raw::binary()</dt>
%%   <dd>This is the original uri that the above fields were populated
%%       from. Everything will still be in their original quoted form.
%%       Note that this may be a best guess as to the uri a user had
%%       in their browser, as this will most likely be formed by
%%       concatenating the `Host' header with the `request' line uri.</dd>
%%  </dl>
%%

%% @doc Populate a new uri record by parsing the string `Uri'

-spec from_string(bin_string()) ->
    uri().

from_string(Uri0) when is_binary(Uri0) ->

    Uri = case Uri0 of
        <<"www.", _R/binary>> ->
            <<"http://", Uri0/binary>>;
        _ ->
            Uri0
    end,

    {Scheme, Uri1} = liburi_parser:parse_scheme(Uri),
    {Authority, Uri2} = liburi_parser:parse_authority(Uri1),
    {UserInfo, HostPort} = liburi_parser:parse_user_info(Authority),
    {Host, Port} = liburi_parser:parse_host_port(HostPort),
    {Path, Uri3} = liburi_parser:parse_path(Uri2),
    {Query, Uri4} = liburi_parser:parse_query(Uri3),
    Frag = liburi_parser:parse_frag(Uri4),
    new(Scheme, UserInfo, Host, Port, Path, Query, Frag);
from_string(Uri) when is_list(Uri) ->
    from_string(iolist_to_binary(Uri)).

%% @doc Return the string this uri represents. (Same as the `raw' field)

-spec to_string(uri()) ->
    binary().

to_string(#uri{raw = Raw}) ->
    Raw.

%% @doc Populate a new #uri record by using `Scheme' and parsing `HostPort' string `Uri'

-spec from_http_1_1(binary(), binary(), binary()) ->
    uri().

from_http_1_1(Scheme, HostPort, Uri) ->
    {Host, Port} = liburi_parser:parse_host_port(HostPort),
    {Path, Uri1} = liburi_parser:parse_path(Uri),
    {Query, Uri2} = liburi_parser:parse_query(Uri1),
    Frag = liburi_parser:parse_frag(Uri2),
    new(Scheme, <<"">>, Host, Port, Path, Query, Frag).

%% @doc Return a uri record with the given fields. Use `""' for any field that isn't used.
%% You probably want {@link raw/7} unless you've parsed a uri yourself.

-spec new(binary(), binary(), binary(), non_neg_integer()|undefined, binary(), qs(), binary()) ->
    uri().

new(Scheme, UserInfo, Host, Port, Path, Query, Frag) when is_binary(Query) ->
    new(Scheme, UserInfo, Host, Port, Path, query_to_proplist(Query), Frag);
new(Scheme, UserInfo, Host, Port, Path, Query, Frag) when is_list(Query) ->
    update_raw(#uri{
        scheme = Scheme,
        user_info = liburi_utils:unquote(UserInfo),
        host = Host,
        port = Port,
        path = liburi_utils:unquote(Path),
        q = Query,
        frag = liburi_utils:unquote(Frag)
    }).

%% @doc Convert the string or the `raw_query' portion of {@link uri()} into a {@link proplists:proplist()}, where the keys
%% are binaries, the values are binaries, and for valueless keys, the atom `null' is used as the value.
%%
%% For example, `"range=5-50&printable"' would result in the following proplist entries:
%% <table>
%%     <tr><th>Key</th><th>Value</th></tr>
%%     <tr><td>"range"</td><td>"5-50"</td></tr>
%%     <tr><td>"printable"</td><td>null</td></tr>
%% </table>
%%
%% The string needent have to be from a uri, this method is also useful for decoding the `Post' body of an HTTP form submission.

-spec query_to_proplist(binary()) ->
    proplists:proplist().

query_to_proplist(Query) ->
    lists:reverse(query_foldl(fun (KV, Acc) -> [KV | Acc] end, [], Query)).

%% @doc Fold over each element of a query. For instance with the query `"range=5-50&printable"', `F' will be called as
%% `F("range", "5-50", Acc)' and `F("printable", null, Acc)'.
%%  Both `Key' and `Value' are already unquoted when `F' is called. @see query_to_dict/1

-spec query_foldl(fun((proplists:property(), Acc::term()) -> term()), Acc::term(), qs() | uri()) ->
    Acc::term().

query_foldl(F, Init, #uri{q = Query}) ->
    query_foldl(F, Init, Query);
query_foldl(F, Init, Query) when is_binary(Query) ->
    lists:foldl(fun (Part, Acc) ->
        case binary:split(Part, <<"=">>) of
            [Key, Value] ->
                F({unquote(Key), unquote(Value)}, Acc);
            [<<>>] ->
                Acc;
            [Key] ->
                F({unquote(Key), null}, Acc)
        end
    end, Init, binary:split(erlang:iolist_to_binary(Query), <<"&">>, [global]));
query_foldl(F, Init, Query) when is_list(Query) ->
    lists:foldl(F, Init, Query).

%% @doc Convert a dictionary or proplist to an iolist representing the query part of a uri. Keys and values can
%% be binaries, lists, atoms, integers or floats, and will be automatically converted to a string and quoted.

-spec to_query(proplist:proplist()) ->
    binary().

to_query(List) when is_list(List) ->
    to_query(fun lists:foldl/3, List).

%% @doc Return an binary representing the query part of a uri by folding over `Ds' by calling the provided `FoldF',
%% which should take three arguments: a function, an initial accumulator value, and  the data structure to fold over.
%% @see to_query/1

-spec to_query(function(), binary() | proplist:proplist()) ->
    binary().

to_query(FoldF, Ds) ->
    FoldF(fun
        ({K, V}, <<>>) ->
            KB = liburi_utils:quote(liburi_utils:to_binary(K), q),
            VB = liburi_utils:quote(liburi_utils:to_binary(V), q),
            <<KB/binary, <<"=">>/binary, VB/binary>>;
        ({K, V}, Acc) ->
            KB = liburi_utils:quote(liburi_utils:to_binary(K), q),
            VB = liburi_utils:quote(liburi_utils:to_binary(V), q),
            <<Acc/binary, $&, KB/binary, <<"=">>/binary, VB/binary>>;
        (K, <<>>) ->
            KB = liburi_utils:quote(liburi_utils:to_binary(K), q),
            KB;
        (K, Acc) ->
            KB = liburi_utils:quote(liburi_utils:to_binary(K), q),
            <<Acc/binary, $&, KB/binary>>
    end, <<>>, Ds).

%% @doc Return `Str' with all `+' replaced with space, and `%NN' replaced with the decoded byte.

-spec unquote(binary()) ->
    binary().

unquote(Str) ->
    liburi_utils:unquote(Str).

%% @doc Return `Str' with all reserved or uri-unsafe characters in quoted form. For instance `"A Space"'
%% becomes `"A%20Space"'. This is the same as calling `quote(Str, any)'.
%% @see quote/2

-spec quote(binary()) ->
    binary().

quote(Str) ->
    liburi_utils:quote(Str, any).

%% @doc Return `Str' with all reserved or uri-unsafe characters in quoted form. Since rfc-2396 has different
%% reserved characters for different parts of the uri, you can specify `Part' to obtain a minimally quoted uri for that `Part'.
%%
%% `Part' can be one of the following values:
%%    <dl>
%%      <dt>any</dt>
%%      <dd>Quote any character that is reserved in any potential part.</dd>
%%
%%      <dt>userinfo</dt>
%%      <dd>Quote for the userinfo part of a uri.</dd>
%%
%%      <dt>path</dt>
%%      <dd>Quote for the path part of a uri.</dd>
%%
%%      <dt>segment</dt>
%%      <dd>Quote for a path's segment. This is much like `path', but will also quote the `/' character.</dd>
%%
%%      <dt>segment_param</dt>
%%      <dd>Quote for path segment's parameters (a fairly obscure part of uris). This is like `path' but will also quote the characters `/' and `;'.</dd>
%%
%%      <dt>query_ | q</dt>
%%      <dd>Quote for query parts. `query' is an erlang keyword so you can either use `` q '' or `query_' to specify this part. This
%%       will quote characters such as `&' and `=', so it needs to be called on the individual key/value parts of a query. See
%%       {@link to_query/1} and {@link to_query/1}.</dd>
%%
%%      <dt>frag</dt>
%%      <dd>Quote for the fragment part of a uri</dd>
%%   </dl>

-spec quote(binary(), atom()) ->
    binary().

quote(Str, Part) ->
    liburi_utils:quote(Str, Part).

%% @doc Return the scheme field of {@link uri()}.

-spec scheme(uri()) -> binary().
scheme(#uri{scheme = Scheme}) ->
    Scheme.

%% @doc Set the scheme field of {@link uri()}.

-spec scheme(uri(), binary()) -> uri().
scheme(Uri, NewScheme) ->
    update_raw(Uri#uri{scheme = NewScheme}).

%% @doc Return the user_info field of {@link uri()}.

-spec user_info(uri()) -> binary().
user_info(#uri{user_info = UserInfo}) ->
    UserInfo.

%% @doc Set the user_info field of {@link uri()}.

-spec user_info(uri(), binary()) -> uri().
user_info(Uri, NewUserInfo) ->
    update_raw(Uri#uri{user_info = NewUserInfo}).

%% @doc Return the host field of {@link uri()}.

-spec host(uri()) -> binary().
host(#uri{host = Host}) ->
    Host.

%% @doc Set the host field of {@link uri()}.

-spec host(uri(), binary()) -> uri().
host(Uri, NewHost) ->
    update_raw(Uri#uri{host = NewHost}).

%% @doc Return the port field of {@link uri()}.

-spec port(uri()) -> integer().
port(#uri{port = Port}) ->
    Port.

%% @doc Set the port field of {@link uri()}.

-spec port(uri(), integer()) -> uri().
port(Uri, NewPort) ->
    update_raw(Uri#uri{port = NewPort}).

%% @doc Return the path field of {@link uri()}.

-spec path(uri()) -> binary().
path(#uri{path = Path}) ->
    Path.

%% @doc Set the path field of {@link uri()}.

-spec path(uri(), binary()) -> uri().
path(Uri, NewPath) ->
    update_raw(Uri#uri{path = NewPath}).

%% @doc Append a path to the existing path of the system

-spec append_path(uri(), binary()) -> uri().
append_path(Uri=#uri{path=Path}, NewPath) ->
    path(Uri, <<Path/binary, <<"/">>/binary, NewPath/binary>>).

%% @doc Return the raw_query field of {@link uri()}.

-spec raw_query(uri()) -> binary().
raw_query(#uri{q = Query}) ->
    to_query(Query).

%% @doc Set the raw_query field of {@link uri()}.

-spec raw_query(uri(), binary()) -> uri().
raw_query(Uri, NewRawQuery) ->
    update_raw(Uri#uri{q = query_to_proplist(NewRawQuery)}).

%% @doc Return the query field of {@link uri()}.

-spec q(uri()) -> proplists:proplist().
q(#uri{q = Query}) ->
    Query.

%% @doc Set the query field of {@link uri()}.

-spec q(uri(), proplists:proplist()) -> uri().
q(Uri, Query) when is_list(Query) ->
    update_raw(Uri#uri{q = Query}).

%% @doc Return the frag field of {@link uri()}.

-spec frag(uri()) -> binary().
frag(#uri{frag = Frag}) ->
    Frag.

%% @doc Set the frag field of {@link uri()}.

-spec frag(uri(), binary()) -> uri().
frag(Uri, NewFrag) ->
    update_raw(Uri#uri{frag = NewFrag}).

%% @doc Return the raw field of {@link uri()}.

-spec raw(uri()) -> binary().
raw(#uri{raw = Raw}) ->
    Raw.

%% internals

user_info_to_string(#uri{user_info = UserInfo}) ->
    case byte_size(UserInfo) of
        0 ->
            UserInfo;
        _ ->
            <<UserInfo/binary, $@>>
    end.

port_to_string(#uri{port = Port}) ->
    case Port of
        undefined ->
            <<"">>;
        _ ->
            <<$:, (integer_to_binary(Port))/binary>>
    end.

path_to_string(#uri{path = <<>>}) ->
    $/;
path_to_string(#uri{path = Path}) ->
    case liburi_utils:quote(Path, path) of
        <<$/, _/binary>> ->
            Path;
        QuotedPath ->
            <<$/, QuotedPath/binary>>
    end.

query_to_string(#uri{q = []}) ->
    <<"">>;
query_to_string(#uri{q = Query}) ->
    <<$?, (to_query(Query))/binary>>.

frag_to_string(#uri{frag = <<>>}) ->
    <<>>;
frag_to_string(#uri{frag = Frag}) ->
    <<$#, (liburi_utils:quote(Frag, frag))/binary>>.

update_raw(Uri) ->
    Uri#uri{raw = erlang:iolist_to_binary(to_iolist(Uri))}.

to_iolist(Uri) ->
    [Uri#uri.scheme, <<"://">>, user_info_to_string(Uri), Uri#uri.host, port_to_string(Uri), path_to_string(Uri), query_to_string(Uri), frag_to_string(Uri)].
