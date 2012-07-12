.PHONY: test

all:
	@./rebar compile
	@./rebar xref skip_deps=true
	@./rebar eunit skip_deps=true
compile:
	@./rebar compile skip_deps=true
xref:
	@./rebar xref skip_deps=true
eunit:
	@./rebar eunit skip_deps=true
clean:
	@./rebar clean skip_deps=true
distclean:
	@./rebar clean
qc:
	@./rebar qc skip_deps=true

