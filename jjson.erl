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
jdeserialize(S) -> jdeserialize_tokens(jtokenize(S)).
jconsumeCharacterToken([C | Rest]) -> {list_to_atom([C]), Rest}.
jconsumeString(String, [$\" | Rest]) -> {lists:reverse(String), Rest};
jconsumeString(String, [C | Rest]) -> jconsumeString([C | String], Rest).
is_digit(C) -> lists:member(C, "1234567890.").
list_to_number(String) ->
  case lists:member($., String) of
    true -> list_to_float(String);
    false -> list_to_integer(String)
  end.
jconsumeNumber(Number, []) ->
  {list_to_number(lists:reverse(Number)), []};
jconsumeNumber(Number, [C | Rest]) ->
  case is_digit(C) of
    true -> jconsumeNumber([C | Number], Rest);
    false -> {list_to_number(lists:reverse(Number)), [C | Rest]}
  end.
starts_with([], _) -> true;
starts_with([P | Rest], [P | SRest]) -> starts_with(Rest, SRest);
starts_with(_, _) -> false.
jconsumeToken(String) ->
  [C | Rest] = String,
  case starts_with("true", String) of
    true -> {'true', string:sub_string(String, 5)};
    false ->
      case starts_with("false", String) of
        true -> {'false', string:sub_string(String, 6)};
        false ->
          case starts_with("null", String) of
            true -> {'null', string:sub_string(String, 5)};
            false ->
              case lists:member(C, "{:,}[]") of
                true -> jconsumeCharacterToken(String);
                false ->
                  case lists:member(C, "-1234567890.") of
                    true -> jconsumeNumber("", String);
                    false ->
                      case C of
                        $\" -> jconsumeString("", Rest);
                        $\ -> jconsumeToken(Rest);
                        $\t -> jconsumeToken(Rest);
                        $\n -> jconsumeToken(Rest)
                      end
                  end
              end
          end
      end
  end.
jtokenize(Tokens, "") -> lists:reverse(Tokens);
jtokenize(Tokens, String) ->
  {Token, Rest} = jconsumeToken(String),
  % io:format("~p", [{Token, Rest}]),
  case Rest of
    [] -> lists:reverse([Token | Tokens]);
    _ -> jtokenize([Token | Tokens], Rest)
  end.
jtokenize(String) -> jtokenize([], String).
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
