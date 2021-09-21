-module(jjson).
-behavior(gen_server).
-export([
  init/1,
  handle_call/3
  % handle_cast/2
]).

-export([
  start_link/1,
  start/1,
  serialize/1,
  serialize/2,
  deserialize/1,
  deserialize/2,
  jtokenize/1,
  jdeserialize_tokens/1
]).

jserializeList(L) ->
  lists:append([
    "[",
    lists:append(
      lists:join(", ",
        lists:map(fun (E) -> jserialize(E) end, L))),
    "]"
  ]).
jserialize(S) when is_list(S) ->
  case io_lib:printable_list(S) of
    true -> io_lib:format("\"~s\"", [S]);
    false -> jserializeList(S)
  end;
jserialize(N) when is_number(N) -> io_lib:format("~p", [N]);
jserialize(true) -> "true";
jserialize(false) -> "false";
jserialize(null) -> "null";
jserialize(A) when is_atom(A) ->
      lists:append(["\"", atom_to_list(A), "\""]);
jserialize(M) when is_map(M) ->
  lists:append([
    "{",
    lists:append(
      lists:join(
        ", ",
        lists:map(
          fun ({Key, Value}) ->
            io_lib:format("~s: ~s", [jserialize(Key), jserialize(Value)])
          end,
          maps:to_list(M)))),
    "}"
  ]).
jdeserialize_dict_pair([Key, ':' | Rest]) when is_list(Key) ->
  case io_lib:printable_list(Key) of
    true ->
      {NextAstNode, AfterNode} = jdeserialize_tokens(Rest),
      {{list_to_atom(Key), NextAstNode}, AfterNode}
  end.
jdeserialize_dict(Pairs, ['}' | Rest]) -> {maps:from_list(Pairs), Rest};
jdeserialize_dict(Pairs, Tokens) ->
  {Pair, TokensAfter} = jdeserialize_dict_pair(Tokens),
  case TokensAfter of
    [',' | Rest] -> jdeserialize_dict([Pair | Pairs], Rest);
    ['}' | Rest] -> jdeserialize_dict([Pair | Pairs], [ '}' | Rest])
  end.
jdeserialize_list(Elements, [']' | Rest]) ->
  {lists:reverse(Elements), Rest};
jdeserialize_list(Elements, Tokens) ->
  {Element, Rest} = jdeserialize_tokens(Tokens),
  case Rest of
    [',' | NewRest] -> jdeserialize_list([Element | Elements], NewRest);
    [']' | _NewRest] -> jdeserialize_list([Element | Elements], Rest)
  end.
jdeserialize_tokens([true | Rest]) -> {true, Rest};
jdeserialize_tokens([false | Rest]) -> {false, Rest};
jdeserialize_tokens([null | Rest]) -> {null, Rest};
jdeserialize_tokens(['{' | Rest]) -> jdeserialize_dict([], Rest);
jdeserialize_tokens([S | Rest]) when is_list(S) ->
  case io_lib:printable_list(S) of
    true -> {S, Rest}
  end;
jdeserialize_tokens([N | Rest]) when is_number(N) -> {N, Rest};
jdeserialize_tokens(['[' | Rest]) -> jdeserialize_list([], Rest).
deserialize(S) ->
  {Object, []} = jdeserialize(S),
  Object.
jdeserialize_tokens_top([]) -> {ok, []};
jdeserialize_tokens_top(Tokens) -> jdeserialize_tokens(Tokens).
jdeserialize(S) -> jdeserialize_tokens_top(jtokenize(S)).
is_hex(D) -> lists:member(D, "0123456789ABCDEFabcdef").
jconsumeString(String, <<$":8, Rest/binary>>) ->
  {lists:reverse(String), Rest};
jconsumeString(String, <<"\\\"", Rest/binary>>) ->
  jconsumeString([$\" | String], Rest);
jconsumeString(String, <<"\\\\", Rest/binary>>) ->
  jconsumeString([$\\ | String], Rest);
jconsumeString(String, <<"\\/", Rest/binary>>) ->
  jconsumeString([$/ | String], Rest);
jconsumeString(String, <<"\\b", Rest/binary>>) ->
  jconsumeString([$\b | String], Rest);
jconsumeString(String, <<"\\f", Rest/binary>>) ->
  jconsumeString([$\f | String], Rest);
jconsumeString(String, <<"\\n", Rest/binary>>) ->
  jconsumeString([$\n | String], Rest);
jconsumeString(String, <<"\\r", Rest/binary>>) ->
  jconsumeString([$\r | String], Rest);
jconsumeString(String, <<"\\t", Rest/binary>>) ->
  jconsumeString([$\t | String], Rest);
jconsumeString(String, <<"\\u", Ds:32, Rest/binary>>) ->
  case lists:all(fun (D) -> is_hex(D) end,
                 binary_to_list(<<Ds:32>>)) of
    true ->
      CodePoint = binary_to_integer(<<Ds:32>>, 16),
      Utf8 = binary_to_list(<<CodePoint:32>>),
      RevUtf8 = lists:reverse(Utf8),
      jconsumeString(lists:append([RevUtf8, String]), Rest)
  end;
jconsumeString(String, <<C:8, Rest/binary>>) ->
  jconsumeString([C | String], Rest).
list_to_number(String) ->
  case lists:member($., String) of
    true -> list_to_float(String);
    false -> list_to_integer(String)
  end.
is_digit(C) -> lists:member(C, "1234567890.").
jconsumeDigits(Chars, <<>>) -> {lists:reverse(Chars), <<>>};
jconsumeDigits(Chars, <<Digit/utf8, Rest/binary>>) ->
  case is_digit(Digit) of
    true -> jconsumeDigits([Digit | Chars], Rest);
    false -> {lists:reverse(Chars), <<Digit/utf8, Rest/binary>>}
  end.
jconsumeDigits(Binary) -> jconsumeDigits([], Binary).
jconsumeMaybeDecimal(Number, <<".", Rest/binary>>) ->
  {Decimal, AfterDecimal} = jconsumeDigits(Rest),
  {Number#{decimal := lists:append([".", Decimal])}, AfterDecimal};
jconsumeMaybeDecimal(Number, Binary) -> {Number, Binary}.
jconsumeMaybeExponentSign(Number, <<"-", Rest/binary>>) ->
  {Number#{exponent_sign := "-"}, Rest};
jconsumeMaybeExponentSign(Number, Binary) -> {Number, Binary}.
jconsumeMaybeExponent(Number, <<"e", Rest/binary>>) ->
  {Number1, AfterSign} = jconsumeMaybeExponentSign(Number, Rest),
  {Exponent, AfterExponent} = jconsumeDigits(AfterSign),
  {Number1#{exponent := Exponent}, AfterExponent};
jconsumeMaybeExponent(Number, Binary) -> {Number, Binary}.
pow(_, 0) -> 1;
pow(N, 1) -> N;
pow(N, P) when P < 0 -> 1 / pow(N, -P);
pow(N, P) ->
  case P rem 2 of
    0 -> X = pow(N, P div 2), X * X;
    1 -> N * pow(N, P - 1)
  end.
number_state_to_string(#{number_sign := Sign,
                         number := NumberText,
                         decimal := Decimal,
                         exponent_sign := ExponentSign,
                         exponent := ExponentText}) ->
   Number = list_to_number(lists:append([Sign, NumberText, Decimal])),
   Exponent = list_to_integer(lists:append([ExponentSign, ExponentText])),
   Number * pow(10, Exponent).
numberStartState() ->
  #{number_sign => "",
    number => "0",
    decimal => "",
    exponent_sign => "",
    exponent => "0"}.
jconsumeAfterSign(State, Rest) ->
  {Number, AfterNumber}  = jconsumeDigits(Rest),
  State1 = State#{number := Number},
  {State2, AfterDecimal} = jconsumeMaybeDecimal(State1, AfterNumber),
  {State3, AfterExponent} = jconsumeMaybeExponent(State2, AfterDecimal),
  {number_state_to_string(State3), AfterExponent}.
jconsumeToken(<<"true", Rest/binary>>) -> {true, Rest};
jconsumeToken(<<"false", Rest/binary>>) -> {false, Rest};
jconsumeToken(<<"null", Rest/binary>>) -> {null, Rest};
jconsumeToken(<<"{", Rest/binary>>) -> {'{', Rest};
jconsumeToken(<<":", Rest/binary>>) -> {':', Rest};
jconsumeToken(<<",", Rest/binary>>) -> {',', Rest};
jconsumeToken(<<"}", Rest/binary>>) -> {'}', Rest};
jconsumeToken(<<"[", Rest/binary>>) -> {'[', Rest};
jconsumeToken(<<"]", Rest/binary>>) -> {']', Rest};
jconsumeToken(<<"\"", Rest/binary>>) -> jconsumeString("", Rest);
jconsumeToken(<<"-", AfterSign/binary>>) ->
  jconsumeAfterSign((numberStartState())#{number_sign := "-"}, AfterSign);
jconsumeToken(<<Digit/utf8, Rest/binary>>) ->
  case is_digit(Digit) of
    true -> jconsumeAfterSign(numberStartState(),
                              <<Digit/utf8, Rest/binary>>)
  end.
skip_whitespace(<<" ", Rest/binary>>) -> skip_whitespace(Rest);
skip_whitespace(<<"\t", Rest/binary>>) -> skip_whitespace(Rest);
skip_whitespace(<<"\n", Rest/binary>>) -> skip_whitespace(Rest);
skip_whitespace(<<Given/binary>>) -> Given.
jtokenize(Tokens, <<>>) -> lists:reverse(Tokens);
jtokenize(Tokens, String) ->
  {Token, Rest} = jconsumeToken(skip_whitespace(String)),
  jtokenize([Token | Tokens], skip_whitespace(Rest)).
jtokenize(String) -> jtokenize([], list_to_binary(String)).
init(_Input) -> {ok, _Input}.
serialize(Object) -> lists:flatten(jserialize(Object)).
handle_call({serialize, Object}, _From, State) -> 
  {reply, lists:flatten(jserialize(Object)), State};
handle_call({deserialize, String}, _From, State) ->
  {Object, []} = jdeserialize(String),
  {reply, Object, State}.
start(_Input) -> gen_server:start(jjson, {}, []).
start_link(_Input) -> gen_server:start_link(jjson, {}, []).
serialize(Self, Object) -> gen_server:call(Self, {serialize, Object}).
deserialize(Self, String) ->
  gen_server:call(Self, {deserialize, String}).
