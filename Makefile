emacs ?= emacs
SHELL = /bin/sh
BASEDIR := $(shell pwd)

profile:
	$(emacs) -- --ig-profile=profiler

debug:
	$(emacs) --debug-init

up: IS_GIT_WT := $(shell git rev-parse --is-inside-work-tree > /dev/null 2>&1 && git config --get remote.origin.url; echo $$?)
up:
	cd $(BASEDIR)
	if [ "z$(IS_GIT_WT)" = "z0" ]; then \
		git pull 2>&1; \
	else \
		echo 'Not inside git repository'; \
	fi
	$(emacs) -batch -Q -l init.el -f ig-update-all-autoloads -- --ig-profile-minimal 2>&1
	$(emacs) -batch -Q -l init.el -- --ig-profile=update 2>&1

run:
	$(emacs)

min:
	$(emacs) -- --ig-profile=minimal

.PHONY: debug up run min profile

