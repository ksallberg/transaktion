# transaktion
Transaction engine

## hierarchy

### table

Data formatted as KV-pairs need to be put in a table. The table
is like a namespace.

#### key value pair
The key value pair, {k, v}, is any erlang term.

## to test (requires lux, rebar3 and erlang/otp r18+)

### all tests (lux and proper)
```
make test
```

### only proper
```
make proper
```

or one of

```
rebar3 proper
rebar3 proper -n 10000
rebar3 proper -p prop_test
rebar3 proper -n 1000 -p prop_test
```