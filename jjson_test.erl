-module(jjson_test).
-include_lib("eunit/include/eunit.hrl").

reverse_test() -> lists:reverse([1,2,3]).
reverse_test_again_test() -> lists:reverse([1,2,3]).

deserialize_number_test() -> 4 = jjson:deserialize("4").
deserialize_big_number_test() -> 123456789 = jjson:deserialize("123456789").
deserialize_float_test() -> 3.14159 = jjson:deserialize("3.14159").
deserialize_list_test() -> [1,2,3] = jjson:deserialize("[1,2,3]").
deserialize_string_test() -> "this" = jjson:deserialize("\"this\"").
deserialize_dict_test() ->
  #{hello := 1, blah := "abc"} = jjson:deserialize("{\"hello\":1, \"blah\": \"abc\"}").
deserialize_dict2_test() ->
  #{hello := [1,2,3], blah := "abc"} = jjson:deserialize("{\"hello\": [1, 2,3], \"blah\": \"abc\"}").
deserialize_scientific_notation_test() -> 1000 = jjson:deserialize("1e3").
deserialize_scientific_notation2_test() -> 1234.0 = jjson:deserialize("1.234e3").
deserialize_special_chars_test() ->
  "\"\\/\b\f\n\r\t" = jjson:deserialize("\"\\\"\\\\\\/\\b\\f\\n\\r\\t\"").
round_trip(Item) -> Item = jjson:deserialize(jjson:serialize(Item)).
round_trip_dict_test() -> round_trip(#{a => 12, b => "this"}).
round_trip_number_test() -> round_trip(4).
round_trip_negative_number_test() -> round_trip(-4).
rount_trip_big_number_test() -> round_trip(123456789).
round_trip_float_test() -> round_trip(3.14159).
