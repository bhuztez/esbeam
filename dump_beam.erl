-module(dump_beam).

-export([dump/0, dump/1]).


find_chunks(_Offset, <<>>) ->
    [];
find_chunks(Offset, <<Name:4/binary, Length:32, Bin/binary>>) ->
    Size =
        case Length rem 4 of
            0 ->
                Length;
            X ->
                Length + 4 - X
        end,
    <<_:Size/binary, Bin1/binary>> = Bin,

    Chunks = find_chunks(Offset+8+Size, Bin1),
    [{Name,Offset+8,Length}|Chunks].


find_chunks(<<"FOR1", Length:32, Bin:Length/binary>>) ->
    <<"BEAM", Bin1/binary>> = Bin,
    find_chunks(12, Bin1).


read_chunk(Chunk, Bin, Chunks) ->
    {Chunk, Offset, Length} = proplists:lookup(Chunk, Chunks),
    binary:part(Bin, Offset, Length).

parse_chunk(Chunk, Bin, Chunks) ->
    parse_chunk(Chunk, read_chunk(Chunk, Bin, Chunks)).

parse_chunk(<<"Atom">>, <<Numbers:32, Bin/binary>>) ->
    parse_atoms(Numbers, Bin);
parse_chunk(<<"Code">>, <<_SS:32, _IS:32, _OM:32, _Ls:32, _Fs:32, Bin/binary>>) ->
    parse_code(Bin);
parse_chunk(<<"ImpT">>, <<N:32, Bin/binary>>) ->
    parse_imports(N, Bin);
parse_chunk(<<"ExpT">>, <<N:32, Bin/binary>>) ->
    parse_exports(N, Bin);
parse_chunk(<<"LocT">>, <<N:32, Bin/binary>>) ->
    parse_exports(N, Bin);
parse_chunk(<<"LitT">>, <<Size:32, Bin/binary>>) ->
    <<Bin1:Size/binary>> = zlib:uncompress(Bin),
    <<N:32, Bin2/binary>> = Bin1,
    parse_literals(N, Bin2);
parse_chunk(<<"Abst">>, <<>>) ->
    none;
parse_chunk(<<"Abst">>, Bin) ->
    binary_to_term(Bin);
parse_chunk(<<"CInf">>, Bin) ->
    term:from_binary(Bin);
parse_chunk(<<"Attr">>, Bin) ->
    term:from_binary(Bin);
parse_chunk(<<"Line">>, <<_Ver:32, _Bits:32, _NumLineInsts:32, NumLines:32, NumFnames:32, Bin/binary>>) ->
    {Lines, Bin1} = parse_lines(NumLines, Bin),
    Fnames = parse_fnames(NumFnames, Bin1),
    {Lines, Fnames};
parse_chunk(Chunk, Bin) ->
    io:format("Unknown chunk ~s: ~p~n", [Chunk, Bin]).


parse_atoms(0, <<>>) ->
    [];
parse_atoms(N, <<Size, Atom:Size/binary, Bin/binary>>) ->
    [Atom|parse_atoms(N-1, Bin)].

parse_code(<<>>) ->
    [];
parse_code(<<C:8, Bin/binary>>) ->
    {Op, Arity} = beam_opcodes:opname(C),
    {Args, Bin1} = parse_n_args(Arity, Bin),
    [{Op,Arity,Args}|parse_code(Bin1)].

decode_tag(0) -> u;
decode_tag(1) -> i;
decode_tag(2) -> a;
decode_tag(3) -> x;
decode_tag(4) -> y;
decode_tag(5) -> f;
decode_tag(6) -> h;
decode_tag(7) -> z.

parse_arg(<<0:4, 0:1, 7:3, Float:64/float, Bin/binary>>) ->
    {{float, Float}, Bin};
parse_arg(<<1:4, 0:1, 7:3, Bin/binary>>) ->
    {{u,N}, Bin1} = parse_arg(Bin),
    parse_n_args(N, Bin1);
parse_arg(<<2:4, 0:1, 7:3, Bin/binary>>) ->
    {{u, Fr}, Bin1} = parse_arg(Bin),
    {{fr, Fr}, Bin1};
parse_arg(<<3:4, 0:1, 7:3, Bin/binary>>) ->
    {{u, N}, Bin1} = parse_arg(Bin),
    {List, Bin2} = parse_alloc_list(N, Bin1),
    {{alloc, List}, Bin2};
parse_arg(<<4:4, 0:1, 7:3, Bin/binary>>) ->
    {{u, N}, Bin1} = parse_arg(Bin),
    {{literal, N}, Bin1};
parse_arg(<<_:5, 7:3, _Bin/binary>>) ->
    throw(bad);
parse_arg(<<N:4,0:1,Tag:3, Bin/binary>>) ->
    {{decode_tag(Tag), N}, Bin};
parse_arg(<<N1:3,1:2,Tag:3, N2:8, Bin/binary>>) ->
    <<N:11>> = <<N1:3, N2:8>>,
    {{decode_tag(Tag), N}, Bin};
parse_arg(<<N:3, 3:2,Tag:3, Bin/binary>>) ->
    case N of
        7 ->
            {{u,L}, Bin1} = parse_arg(Bin),
            Length = L + 9;
        L ->
            Length = L + 2,
            Bin1 = Bin
    end,
    Bits = Length * 8,
    case Tag of
        1 ->
            <<Int:Bits/signed, Bin2/binary>> = Bin1;
        _ ->
            <<Int:Bits, Bin2/binary>> = Bin1
    end,
    {{decode_tag(Tag), Int}, Bin2}.

parse_n_args(0, Bin) ->
    {[], Bin};
parse_n_args(N, Bin) ->
    {H, Bin1} = parse_arg(Bin),
    {T, Bin2} = parse_n_args(N-1, Bin1),
    {[H|T], Bin2}.

parse_alloc_list(0, Bin) ->
    {[], Bin};
parse_alloc_list(N, Bin) ->
    {{u,Type}, Bin1} = parse_arg(Bin),
    {{u,Val}, Bin2} = parse_arg(Bin1),
    H =
        case Type of
            0 ->
                {words,Val};
            1 ->
                {floats,Val};
            2 ->
                {literal, Val}
        end,
    {T, Bin3} = parse_alloc_list(N-1, Bin2),
    {[H|T], Bin3}.


parse_imports(0, <<>>) ->
    [];
parse_imports(N, <<M:32,F:32,A:32, Bin/binary>>) ->
    [{M,F,A}|parse_imports(N-1, Bin)].

parse_exports(0, <<>>) ->
    [];
parse_exports(N, <<F:32,A:32,L:32, Bin/binary>>) ->
    [{F,A,L}|parse_exports(N-1, Bin)].


parse_literals(0, <<>>) ->
    [];
parse_literals(N, <<Size:32, Term:Size/binary, Bin/binary>>) ->
    [binary_to_term(Term)|parse_literals(N-1,Bin)].

parse_lines(0, Bin) ->
    {[], Bin};
parse_lines(N, Bin) ->
    case parse_arg(Bin) of
        {{a,_}=F, Bin1} ->
            {{i,_}=L, Bin2} = parse_arg(Bin1),
            {Lines, Bin3} = parse_lines(N-1, Bin2),
            {[{F,L}|Lines], Bin3};
        {{i,_}=L, Bin1} ->
            {Lines,Bin2} = parse_lines(N-1, Bin1),
            {[L|Lines], Bin2}
    end.

parse_fnames(0, <<>>) ->
    [];
parse_fnames(N, <<Size:16, Str:Size/binary, Bin/binary>>) ->
    [Str|parse_fnames(N-1,Bin)].


dump() ->
    dump(?MODULE).

dump(Module) ->
    {Module, Bin, _} = code:get_object_code(Module),
    Chunks = find_chunks(Bin),
    io:format("~p~n", [Chunks]),
    parse_chunk(<<"Atom">>, Bin, Chunks),
    parse_chunk(<<"Code">>, Bin, Chunks),
    parse_chunk(<<"ImpT">>, Bin, Chunks),
    parse_chunk(<<"ExpT">>, Bin, Chunks),
    parse_chunk(<<"LocT">>, Bin, Chunks),
    parse_chunk(<<"LitT">>, Bin, Chunks),
    parse_chunk(<<"Attr">>, Bin, Chunks),
    parse_chunk(<<"CInf">>, Bin, Chunks),
    parse_chunk(<<"Abst">>, Bin, Chunks),
    parse_chunk(<<"Line">>, Bin, Chunks).
