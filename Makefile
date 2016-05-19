PRODUCT = whistle

Q = @
ECHO = /bin/echo

MKFILE_PATH := $(abspath $(lastword $(MAKEFILE_LIST)))
CURRENT_DIR := $(notdir $(patsubst %/,%,$(dir $(MKFILE_PATH))))

APPDIR = ${shell pwd}

CFLAGS =
CC = $(APPDIR)/rebar3
ERL = erl

all: $(PRODUCT)

release: all
	$(Q)$(ECHO) "Building release"
	$(Q)$(CC) release

console:
	$(CURRENT_DIR)/$(PRODUCT)/bin/whistle console

$(PRODUCT):
	$(Q)$(ECHO) "$(PRODUCT) build started."
	$(Q)$(CC) compile

generate: $(PRODUCT)
	$(Q)$(CC) generate

clean:
	$(Q)$(ECHO) "Cleanining up."
	$(Q)$(CC) clean

shell:
	$(Q)$(ECHO) "Starting shell in dev environment."
	$(Q)$(CC) shell

deps-upgrade:
	$(Q)$(ECHO) "Upgrading deps."
	$(Q)$(CC) upgrade
