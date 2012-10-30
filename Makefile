DIALYZER = dialyzer
REBAR = rebar

all: app

# Application.

deps:
	@$(REBAR) get-deps

app: deps tags
	@$(REBAR) compile

app-nodeps:
	@$(REBAR) compile skip_deps=true

run: app-nodeps
	./start-dev.sh

clean:
	@$(REBAR) clean
	rm -f test/*.beam
	rm -f test/*.xml
	rm -f erl_crash.dump
	rm -rf test/ct/
	rm -rf test/tests/

# Dialyzer

build-plt:
	@$(DIALYZER) --build_plt --output_plt .$(PROJECT).plt \
		--apps kernel stdlib sasl inets crypto public_key ssl

plt-add-deps:
	@$(DIALYZER) --add_to_plt --plt .$(PROJECT).plt \
		--output_plt .$(PROJECT).plt -r deps/

plt-remove-deps:
	@$(DIALYZER) --remove_from_plt --plt .$(PROJECT).plt \
		--output_plt .$(PROJECT).plt -r deps/

plt-readd-deps: plt-remove-deps plt-add-deps

dialyze: app-nodeps
	@$(DIALYZER) --plt .$(PROJECT).plt -r apps/ \
		-Werror_handling -Wrace_conditions -Wunmatched_returns # -Wunderspecs

.PHONY: all deps app clean app-nodeps test-deps test-app test run tags build-plt plt-add-deps plt-remove-deps plt-readd-deps dialyze
