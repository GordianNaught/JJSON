all: compile compile_test

compile: jjson.erl
	erlc jjson.erl
compile_test: jjson_test.erl
	erlc jjson_test.erl
test: compile compile_test jjson.beam
	erl -eval "jjson_test:test()."
