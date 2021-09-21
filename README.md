# JJSON

This is a simple Erlang library for reading and writing JSON.

It aims to be small and simple, something that wouldn't be heavy to read the source for.

This library has only seen use in my personal projects; use it at your own risk.

It is not complete.

In particular, unicode escape sequences with `\\u` are not functioning yet.

- [x] integers
- [x] floats
- [x] scientific notation
- [x] booleans
- [x] strings
- [x] lists
- [x] dictionaries
- [x] common escape sequences
- [ ] unicode escape sequences with \\u
- [x] light testing
- [ ] heavy testing

## Usage

    1> jjson:serialize(#{person => "bob", values => [1,2,3]}).
    "{\"person\": \"bob\", \"values\": [1, 2, 3]}"
    2> jjson:deserialize("{\"key\": [1,2,3]}").
    #{key => [1,2,3]}

The library does also implement a server behavior, if you prefer.

    1> {ok, Jjson} = jjson:start(state).
    {ok,<0.62.0>}
    2> jjson:serialize(Jjson, [1,2,3,4]).
    "[1, 2, 3, 4]"
    3> jjson:deserialize(Jjson, "[1,2,3,4]").
    [1,2,3,4]

## Building

    cd JJSON
    make compile

This will generate a jjson.beam file for you to use.

## Testing

    make test

## License

This is licensed with the MIT license.
See the `LICENSE` file for details.
