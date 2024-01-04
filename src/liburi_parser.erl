-module(liburi_parser).

-export([
    parse_scheme/1,
    parse_authority/1,
    parse_user_info/1,
    parse_host_port/1,
    parse_path/1,
    parse_query/1,
    parse_frag/1
]).

parse_scheme(Uri) ->
    parse_scheme(Uri, <<>>).

parse_scheme(<<$:, Uri/binary>>, Acc) ->
    {Acc, Uri};
parse_scheme(<<>>, Acc) ->
    {<<>>, Acc};
parse_scheme(<<C, Rest/binary>>, Acc) ->
    parse_scheme(Rest, <<Acc/binary, C>>).

parse_authority(<<$/, $/, Uri/binary>>) ->
    parse_authority(Uri, <<"">>);
parse_authority(Uri) ->
    {<<>>, Uri}.

parse_authority(<<$/, Rest/binary>>, Acc) ->
    {Acc, <<$/, Rest/binary>>};
parse_authority(<<>>, Acc) ->
    {Acc, <<>>};
parse_authority(<<C,  Rest/binary>>, Acc) ->
    parse_authority(Rest, <<Acc/binary, C>>).

parse_user_info(Authority) ->
    parse_user_info(Authority, <<>>).

parse_user_info(<<$@, HostPort/binary>>, Acc) ->
    {Acc, HostPort};
parse_user_info(<<>>, Acc) ->
    {<<>>, Acc};
parse_user_info(<<C, HostPort/binary>>, Acc) ->
    parse_user_info(HostPort, <<Acc/binary, C>>).

parse_host_port(HostPort) ->
    case binary:split(HostPort, <<":">>) of
        [Host] ->
            {Host, undefined};
        [Host, <<>>] ->
            {Host, undefined};
        [Host, Port] ->
            {Host, list_to_integer(binary_to_list(Port))};
        _ ->
            throw({uri_error, {invalid_host_port, HostPort}})
    end.

parse_path(Uri) ->
    parse_path(Uri, <<>>).

parse_path(<<C, Uri/binary>>, Acc)
    when C == $?; C == $# ->
    {Acc, <<C, Uri/binary>>};
parse_path(<<>>, Acc) ->
    {Acc, <<"">>};
parse_path(<<C, Uri/binary>>, Acc) ->
    parse_path(Uri, <<Acc/binary, C>>).

parse_query(<<$?, Uri/binary>>) ->
    parse_query(Uri, <<>>);
parse_query(Uri) ->
    {<<>>, Uri}.

parse_query(<<$#, Uri/binary>>, Acc) ->
    {Acc, <<$#, Uri/binary>>};
parse_query(<<>>, Acc) ->
    {Acc, <<"">>};
parse_query(<<C, Rest/binary>>, Acc) ->
    parse_query(Rest, <<Acc/binary, C>>).

parse_frag(<<$#, Frag/binary>>) ->
    liburi_utils:unquote(Frag);
parse_frag(<<>>) ->
    <<>>;
parse_frag(Data) ->
    throw({uri_error, {data_left_after_parsing, Data}}).
