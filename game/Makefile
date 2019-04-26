###########################################################################
## 使用make xls 需要安装xlrd, 语句：easy_install xlrd
## 使用make mod=module编译热更单文件 ,需要配置 tool/single_c/hot_swap.sh文件
###########################################################################
mod=""
all:
ifeq (${mod},"")
	@rm -rf _build/default/lib/game
	@./tool/proto/generate.sh
	@rebar3 release
else
	@./tool/single_c/make.sh ${mod}
endif
compile:
	@rm -rf _build/default/lib/game
	@rebar3 release
prod:
	@rebar3 as prod tar
start:
	@./_build/default/rel/game/bin/game console
start-d:
	@./_build/default/rel/game/bin/game start
remote:
	@./_build/default/rel/game/bin/game remote_console
stop:
	@./tool/single_c/stop.sh
	@./_build/default/rel/game/bin/game stop
xls:
ifeq (${mod},all)
	@./tool/xls/make_all_erl.sh
else
	@./tool/xls/make_one_erl.sh ${mod}
endif
proto:
	@./tool/proto/generate.sh
game:
ifeq (${mod},"")
	@./_build/default/rel/game/bin/game console
else
	@rm -rf game_n/${mod} &&mkdir game_n/${mod}
	@cp -rf game_n/config game_n/${mod}
	@sed -i 's/{node_name}/game${mod}/g' game_n/${mod}/config/vm.args
	@sed -i 's/{web_port}/501${mod}/g' game_n/${mod}/config/sys.config
	@sed -i 's/{login_port}/502${mod}/g' game_n/${mod}/config/sys.config
	@sed -i 's/{game_port}/503${mod}/g' game_n/${mod}/config/sys.config
	@sed -i 's/{server_num}/${mod}/g' game_n/${mod}/config/sys.config
	@sed -i 's/{server_ip}/"192.168.1.52"/g' game_n/${mod}/config/sys.config
	@cp -rf _build/default/rel/* game_n/${mod}/
	@cp -rf game_n/${mod}/config/* game_n/${mod}/game/releases/0.1.0/
	@./game_n/${mod}/game/bin/game console
endif

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
	@printf "%-30s %-10s\n" "make game mod=n"		"start another node"
	@printf	"%-10s\n" "=================================================================================================="
