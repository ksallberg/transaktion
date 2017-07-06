build:
	rebar3 compile

start: build
	erl -boot start_sasl -pa _build/default/lib/*/ebin \
            -pa priv -eval "application:start(transaktion)"

# Start a node, but with a certain sname
# TR_TEST_ID=1 make start_node
start_node: build
	erl -sname tr_test$(TR_TEST_ID) -setcookie dough \
            -pa _build/default/lib/*/ebin \
            -pa priv -eval "application:start(transaktion)"

.PHONY: start start_node

clean:
	rm -rf _build
	rm -f ebin/*.beam
	rm -f priv/*.beam
	rm -rf lux_logs
	rm -rf log/*.log
.PHONY: clean

# if no plt file:
# dialyzer --build_plt --apps mnesia
# dialyzer --add_to_plt ./_build/default/lib/brunhilde/
dialyzer:
	dialyzer --src src/
.PHONY: dialyzer

test: build
	lux test/
.PHONY: test
