get-deps:
	@rebar get-deps

compile: get-deps
	@rebar compile

compile-nodeps:
	@rebar compile skip_deps=true

release: compile
	@rebar generate -f

release-nodeps: compile-nodeps
	@rebar generate -f

quickdev: release-nodeps
	@./rel/dws/bin/dws console

all: release

clean: devclean
	@rebar clean


## Development environment generator
.PHONY : devrel
DEVNODES ?= 3
REBAR ?= rebar
SCREENRCTPL = rel/screenrc
SCREENRC = dev/.screenrc

# 'seq' is not available on all *BSD, so using an alternate in awk
SEQ = $(shell awk 'BEGIN { for (i = 1; i < '$(DEVNODES)'; i++) printf("%i ", i); print i ;exit(0);}')

$(eval devrel : $(foreach n,$(SEQ),dev$(n)))

rundevrel: devrel
	@cp $(SCREENRCTPL) $(SCREENRC)
	@awk 'BEGIN { for (i = 0; i < '$(DEVNODES)'; i++) \
		printf("screen -t DWS%d %d ./dev/dev%d/bin/dws console\n", i+1, i, i+1); }' \
		>> $(SCREENRC)
	@echo "select 0" >> $(SCREENRC)
	@screen -c $(SCREENRC)

dev% : release-nodeps
	@mkdir -p dev
	@rel/gen_dev $@ rel/vars/dev_vars.config.src rel/vars/$@_vars.config
	@(cd rel && $(REBAR) generate target_dir=../dev/$@ overlay_vars=vars/$@_vars.config)

devclean:
	@rm -rf dev rel/vars/dev*_vars.config

