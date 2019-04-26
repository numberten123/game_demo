%%% !!! DO NOT EDIT !!!
%%% auto generated from Excel files
-module(cfg_data_upgrade).
-include("config.hrl").
-export([get_all/0]).
-export([get_single/1, get_rows/0, get_columns/0]).

get_rows() -> 2.
get_columns() -> 3.
get_all() ->
	[1,2].

get_single(1) ->
	#cfg_data_upgrade{
		id = 1,
		upgrade_db = pool_game,
		upgrade_params = "CREATE TABLE if not exists `data_upgrade` (\n  `id` int(10) NOT NULL,\n  `status` tinyint(3) DEFAULT '0',\n  PRIMARY KEY (`id`)\n) ENGINE=InnoDB DEFAULT CHARSET=utf8;"
	};
get_single(2) ->
	#cfg_data_upgrade{
		id = 2,
		upgrade_db = pool_game,
		upgrade_params = "ALTER TABLE `data_upgrade` ADD INDEX `status` (`status`) ;"
	};
get_single(Arg) -> throw({badarg, Arg, cfg_data_upgrade}).
