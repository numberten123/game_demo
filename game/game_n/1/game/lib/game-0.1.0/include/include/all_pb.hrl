-ifndef(M_1101_TOC_PB_H).
-define(M_1101_TOC_PB_H, true).
-record(m_1101_toc, {
    cmd = erlang:error({required, cmd}),
    error_code = erlang:error({required, error_code}),
    description
}).
-endif.

-ifndef(M_1001_TOS_PB_H).
-define(M_1001_TOS_PB_H, true).
-record(m_1001_tos, {
    
}).
-endif.

-ifndef(M_1001_TOC_PB_H).
-define(M_1001_TOC_PB_H, true).
-record(m_1001_toc, {
    time = erlang:error({required, time})
}).
-endif.

-ifndef(M_1002_TOS_PB_H).
-define(M_1002_TOS_PB_H, true).
-record(m_1002_tos, {
    
}).
-endif.

-ifndef(M_1002_TOC_PB_H).
-define(M_1002_TOC_PB_H, true).
-record(m_1002_toc, {
    public_key = erlang:error({required, public_key})
}).
-endif.

-ifndef(M_1003_TOS_PB_H).
-define(M_1003_TOS_PB_H, true).
-record(m_1003_tos, {
    username = erlang:error({required, username}),
    password = erlang:error({required, password})
}).
-endif.

-ifndef(M_1003_TOC_PB_H).
-define(M_1003_TOC_PB_H, true).
-record(m_1003_toc, {
    ip = erlang:error({required, ip}),
    port = erlang:error({required, port}),
    role_id = erlang:error({required, role_id}),
    auth_key = erlang:error({required, auth_key})
}).
-endif.

-ifndef(M_1090_TOS_PB_H).
-define(M_1090_TOS_PB_H, true).
-record(m_1090_tos, {
    role_id = erlang:error({required, role_id}),
    auth_key = erlang:error({required, auth_key})
}).
-endif.

-ifndef(M_1090_TOC_PB_H).
-define(M_1090_TOC_PB_H, true).
-record(m_1090_toc, {
    
}).
-endif.

