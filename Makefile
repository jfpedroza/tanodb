BASEDIR = $(shell pwd)
REBAR = rebar3
RELPATH = _build/default/rel/tanodb
PRODRELPATH = _build/prod/rel/tanodb
DEV1RELPATH = _build/dev1/rel/tanodb
DEV2RELPATH = _build/dev2/rel/tanodb
DEV3RELPATH = _build/dev3/rel/tanodb
APPNAME = tanodb
SHELL = /bin/bash

release:
	$(REBAR) release
	mkdir -p $(RELPATH)/../tanodb_config
	[ -f $(RELPATH)/../tanodb_config/tanodb.conf ] || cp $(RELPATH)/etc/tanodb.conf  $(RELPATH)/../tanodb_config/tanodb.conf
	[ -f $(RELPATH)/../tanodb_config/advanced.config ] || cp $(RELPATH)/etc/advanced.config  $(RELPATH)/../tanodb_config/advanced.config

console:
	cd $(RELPATH) && ./bin/tanodb console

prod-release:
	$(REBAR) as prod release
	mkdir -p $(PRODRELPATH)/../tanodb_config
	[ -f $(PRODRELPATH)/../tanodb_config/tanodb.conf ] || cp $(PRODRELPATH)/etc/tanodb.conf  $(PRODRELPATH)/../tanodb_config/tanodb.conf
	[ -f $(PRODRELPATH)/../tanodb_config/advanced.config ] || cp $(PRODRELPATH)/etc/advanced.config  $(PRODRELPATH)/../tanodb_config/advanced.config

prod-console:
	cd $(PRODRELPATH) && ./bin/tanodb console

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

test:
	$(REBAR) ct

devrel1:
	$(REBAR) as dev1 release
	mkdir -p $(DEV1RELPATH)/../tanodb_config
	[ -f $(DEV1RELPATH)/../tanodb_config/tanodb.conf ] || cp $(DEV1RELPATH)/etc/tanodb.conf  $(DEV1RELPATH)/../tanodb_config/tanodb.conf
	[ -f $(DEV1RELPATH)/../tanodb_config/advanced.config ] || cp $(DEV1RELPATH)/etc/advanced.config  $(DEV1RELPATH)/../tanodb_config/advanced.config

devrel2:
	$(REBAR) as dev2 release
	mkdir -p $(DEV2RELPATH)/../tanodb_config
	[ -f $(DEV2RELPATH)/../tanodb_config/tanodb.conf ] || cp $(DEV2RELPATH)/etc/tanodb.conf  $(DEV2RELPATH)/../tanodb_config/tanodb.conf
	[ -f $(DEV2RELPATH)/../tanodb_config/advanced.config ] || cp $(DEV2RELPATH)/etc/advanced.config  $(DEV2RELPATH)/../tanodb_config/advanced.config

devrel3:
	$(REBAR) as dev3 release
	mkdir -p $(DEV3RELPATH)/../tanodb_config
	[ -f $(DEV3RELPATH)/../tanodb_config/tanodb.conf ] || cp $(DEV3RELPATH)/etc/tanodb.conf  $(DEV3RELPATH)/../tanodb_config/tanodb.conf
	[ -f $(DEV3RELPATH)/../tanodb_config/advanced.config ] || cp $(DEV3RELPATH)/etc/advanced.config  $(DEV3RELPATH)/../tanodb_config/advanced.config

devrel: devrel1 devrel2 devrel3

dev1-attach:
	$(BASEDIR)/_build/dev1/rel/tanodb/bin/$(APPNAME) attach

dev2-attach:
	$(BASEDIR)/_build/dev2/rel/tanodb/bin/$(APPNAME) attach

dev3-attach:
	$(BASEDIR)/_build/dev3/rel/tanodb/bin/$(APPNAME) attach

dev1-console:
	$(BASEDIR)/_build/dev1/rel/tanodb/bin/$(APPNAME) console

dev2-console:
	$(BASEDIR)/_build/dev2/rel/tanodb/bin/$(APPNAME) console

dev3-console:
	$(BASEDIR)/_build/dev3/rel/tanodb/bin/$(APPNAME) console

devrel-clean:
	rm -rf _build/dev*/rel

devrel-start:
	for d in $(BASEDIR)/_build/dev*; do $$d/rel/tanodb/bin/$(APPNAME) start; done

devrel-join:
	for d in $(BASEDIR)/_build/dev{2,3}; do $$d/rel/tanodb/bin/$(APPNAME)-admin cluster join tanodb1@127.0.0.1; done

devrel-cluster-plan:
	$(BASEDIR)/_build/dev1/rel/tanodb/bin/$(APPNAME)-admin cluster plan

devrel-cluster-commit:
	$(BASEDIR)/_build/dev1/rel/tanodb/bin/$(APPNAME)-admin cluster commit

devrel-status:
	$(BASEDIR)/_build/dev1/rel/tanodb/bin/$(APPNAME)-admin member-status

devrel-ping:
	for d in $(BASEDIR)/_build/dev*; do $$d/rel/tanodb/bin/$(APPNAME) ping; true; done

devrel-stop:
	for d in $(BASEDIR)/_build/dev*; do $$d/rel/tanodb/bin/$(APPNAME) stop; true; done

start:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) start

stop:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) stop

attach:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) attach

