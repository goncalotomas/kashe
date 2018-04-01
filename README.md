# kashe (/kæʃ/) - Simple LRU cache
[![Build Status](https://travis-ci.org/ClicaAi/kashe.svg?branch=master)](https://travis-ci.org/ClicaAi/kashe)
[![hex version](https://img.shields.io/hexpm/v/kashe.svg)](https://hex.pm/packages/kashe)  

A very simple fixed-size cache implementation with a minimal `get` and `put` interface. Distributed as an OTP application and it can be used as a dependency.

## Build

    $ rebar3 compile

Testing it out
-----

    $ rebar3 shell
    1> kashe:get(1).
    undefined
    2> kashe:put(<<"meaning of life">>, 42).
    ok
    3> kashe:get(<<"meaning of life">>).
    42
