emacs ?= emacs
SHELL = /bin/sh
BASEDIR := $(shell pwd)

profile:
	$(emacs) -- --ig-profile=profiler

debug:
	$(emacs) --debug-init

up: IS_GIT_WT := $(shell git rev-parse --is-inside-work-tree > /dev/null 2>&1 && git config --get remote.origin.url > /dev/null 2>&1; echo $$?)
up:
	cd $(BASEDIR)
	 #"minus" to continue, even in case of error
	-if [ "z$(IS_GIT_WT)" = "z0" ]; then \
		git pull --rebase 2>&1; \
	else \
		echo 'Not inside git repository'; \
	fi
	$(emacs) -batch -Q -l init.el -f ig-update-all-autoloads -- --ig-profile=minimal 2>&1
	-$(emacs) -batch -Q -l init.el -- --ig-profile=update 2>&1
#	$(emacs) -batch -Q -l init.el --eval="(progn(byte-compile-file \"init.el\" t)(byte-recompile-directory \"lisp\" 0))"

run:
	$(emacs)

min:
	$(emacs) -- --ig-profile=minimal

.PHONY: debug up run min profile
