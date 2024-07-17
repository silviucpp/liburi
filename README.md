# liburi

[![Build Status](https://app.travis-ci.com/silviucpp/liburi.svg?branch=master)](https://travis-ci.com/github/silviucpp/liburi)
[![GitHub](https://img.shields.io/github/license/silviucpp/liburi)](https://github.com/silviucpp/liburi/blob/main/LICENSE)
[![Hex.pm](https://img.shields.io/hexpm/v/liburi)](https://hex.pm/packages/liburi)

A module for generating, parsing, encoding, and decoding uris. 

## Quick Start

**Compile:**

```sh
rebar3 compile
```

Simple usage:

```erlang
Uri = liburi:from_string(<<"https://example.mockable.io">>),
liburi:host(Uri).
<<"demo6905838.mockable.io">>
```

## TODO

- support uri creation
- support [ipv6 addresses in names][1]


## Tests

In order to run the integrity tests run `rebar3 eunit` from project root.

[1]: http://en.wikipedia.org/wiki/IPv6#Literal_IPv6_addresses_in_URLs

