-module(beam_server).

-export([start/0, start/1]).

-export([init/1, loop/1, handle_connection/1]).


start() ->
    start(8000).


start(Port) ->
    spawn(?MODULE, init, [Port]).


init(Port) ->
    {ok, Socket} =
        gen_tcp:listen(
          Port,
          [binary,
           {packet, http_bin},
           {active, false},
           {reuseaddr, true}]),
    ?MODULE:loop(Socket).


loop(Socket) ->
    {ok, Conn} = gen_tcp:accept(Socket),
    Pid = spawn(?MODULE, handle_connection, [Conn]),
    ok = gen_tcp:controlling_process(Conn, Pid),
    ?MODULE:loop(Socket).


recv_headers(Conn) ->
    case gen_tcp:recv(Conn, 0) of
        {ok, {http_header, _, Field, _, Value}} ->
            [{Field, Value}|recv_headers(Conn)];
        {ok, http_eoh} ->
            []
    end.


handle_connection(Conn) ->
    {ok, {http_request, Method, {abs_path, Path}, Version}} = gen_tcp:recv(Conn, 0),

    Headers = recv_headers(Conn),
    ok = inet:setopts(Conn, [{packet, raw}]),

    try
        handle_request(Conn, Method, Path, Version, Headers)
    after
        ok = gen_tcp:close(Conn)
    end.


response(Conn, Code, ContentType, Headers, Body) ->
    ok =
        gen_tcp:send(
          Conn,
          [<<"HTTP/1.1 ">>, integer_to_list(Code), <<" ">>, httpd_util:reason_phrase(Code), <<"\r\n">>,
           <<"Connection: close\r\n">>,
           <<"Content-Type: ">>, ContentType, <<"\r\n">>,
           <<"Content-Length: ">>, integer_to_list(iolist_size(Body)), <<"\r\n">>,
           <<"Access-Control-Allow-Origin: *\r\n">>,
           Headers,
           <<"\r\n">>, Body]).


handle_request(Conn, 'GET', <<"/", Path/binary>>, _Version, _Headers) ->
    Name = filename:basename(Path, ".beam"),
    Module = binary_to_atom(Name, latin1),
    case code:get_object_code(Module) of
        {Module, Bin, _} ->
            response(Conn, 200, <<"application/octet-stream">>, [], Bin);
        error ->
            response(Conn, 404, <<"text/plain">>, [], <<"Not Found">>)
    end;
handle_request(Conn, Method, Path, _Version, _Headers) ->
    io:format("request: ~p ~p~n", [Method, Path]),
    response(Conn, 500, <<"text/plain">>, [], <<"Internal Server Error">>).
