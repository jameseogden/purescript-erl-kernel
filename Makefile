.PHONY: ps erl all test

.DEFAULT_GOAL := ps

all: test

ps:
	@spago build

test: 
	@spago -x test.dhall test
