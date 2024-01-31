PROJECT = scraper
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

SP = 4

BUILD_DEPS += relx

DEPS += ibrowse
DEPS += jsx

SHELL_OPTS += -config config/sys.config
SHELL_OPTS += -args_file config/vm.args
SHELL_OPTS += -eval 'application:ensure_all_started($(PROJECT)).'

ERLC_OPTS += +debug_info

include erlang.mk