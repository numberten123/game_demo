
-define(DATA_DETS_KV, dets_kv).
-record(dets_kv,{
    key,
    value
}).

-define(DATA_MNESIA_KV, mnesia_kv).
-record(mnesia_kv,{
    key,
    value
}).

%%联通节点信息
-define(DATA_NODE_INFO, mnesia_node_info).
-record(mnesia_node_info,{
    type,
   	nodes=#{}
}).

%%结点详情
-record(node_info,{
    node,
   	ip,
   	port,
   	num
}).