%%use record on shell
-module(user_default).
-include("common.hrl").
-export([get_meta/0]).

get_meta() -> 
	user_default:module_info().