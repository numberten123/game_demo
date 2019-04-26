###########################################################################
## 使用make xls 需要安装xlrd, 语句：easy_install xlrd
## 使用make mod=module编译热更单文件 ,需要配置 tool/single_c/hot_swap.sh文件
###########################################################################
mod=""
all:
ifeq (${mod},"")
	@rm -rf _build/default/lib/game_api
	@./tool/proto/generate.sh
	@rebar3 release
else
	@./tool/single_c/make.sh ${mod}
endif
compile:
	@rm -rf _build/default/lib/game_api
	@./tool/proto/generate.sh
	@rebar3 release
prod:
	@rebar3 as prod tar
start:
	@./_build/default/rel/game_api/bin/game_api console
start-d:
	@./_build/default/rel/game_api/bin/game_api start
remote:
	@./_build/default/rel/game_api/bin/game_api remote_console
stop:
	@./tool/single_c/stop.sh
	@./_build/default/rel/game_api/bin/game_api stop
xls:
ifeq (${mod},all)
	@./tool/xls/make_all_erl.sh
else
	@./tool/xls/make_one_erl.sh ${mod}
endif

proto:
	@./tool/proto/generate.sh
%:
	@printf	"%-10s\n" "=================================================================================================="
	@printf "%-30s %-10s\n" "make"					"to compile all mod"
	@printf "%-30s %-10s\n" "make mod=module"		"to compile and update one file, eg: make mod=pp_login"
	@printf "%-30s %-10s\n" "make compile"			"to compile all mod"
	@printf "%-30s %-10s\n" "make prod"				"build tag"
	@printf "%-30s %-10s\n" "make start"			"start the project"
	@printf "%-30s %-10s\n" "make start-d"			"start the project by detached"
	@printf "%-30s %-10s\n" "make remote"			"remote the project"
	@printf "%-30s %-10s\n" "make stop"				"stop the project"
	@printf "%-30s %-10s\n" "make xls mod=name"		"make one xls file, eg: make xls mod=data_upgrade.xlsx"
	@printf "%-30s %-10s\n" "make xls mod=all"		"make all xls file"
	@printf "%-30s %-10s\n" "make proto"			"make all proto"
	@printf "%-30s %-10s\n" "make game_api mod=n"		"start another node"
	@printf	"%-10s\n" "=================================================================================================="
