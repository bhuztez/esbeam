-module(term).

-export([from_binary/1]).

from_binary(<<131, Bin/binary>>) ->
    {Term, <<>>} = decode_term(Bin),
    Term.

decode_term(<<97, Int, Bin/binary>>) ->
    {Int, Bin};
decode_term(<<98, Int:32/signed, Bin/binary>>) ->
    {Int, Bin};
decode_term(<<100, Len:16, Name:Len/binary, Bin/binary>>) ->
    {binary_to_atom(Name, latin1), Bin};
decode_term(<<104, Arity:8, Bin/binary>>) ->
    {Terms, Bin1} = decode_terms(Arity, Bin),
    {list_to_tuple(Terms), Bin1};
decode_term(<<106, Bin/binary>>) ->
    {[], Bin};
decode_term(<<107, Len:16, Chars:Len/binary, Bin/binary>>) ->
    {binary_to_list(Chars), Bin};
decode_term(<<108, Length:32, Bin/binary>>) ->
    decode_list(Length, Bin);
decode_term(<<109, Length:32, Data:Length/binary, Bin/binary>>) ->
    {Data, Bin};
decode_term(<<110, Len:8, Sign:8, IntBin:Len/binary, Bin/binary>>) ->
    Bits = 8 * Len,
    <<Int:Bits/little-integer>> = IntBin,
    case Sign of
        0 ->
            {Int, Bin};
        1 ->
            {-Int, Bin}
    end;
decode_term(<<Tag, _/binary>>) ->
    io:format("Unknown Tag: ~p~n", [Tag]).

decode_terms(0, Bin) ->
    {[], Bin};
decode_terms(N, Bin) ->
    {H, Bin1} = decode_term(Bin),
    {T, Bin2} = decode_terms(N-1,Bin1),
    {[H|T], Bin2}.

decode_list(0, Bin) ->
    decode_term(Bin);
decode_list(N, Bin) ->
    {H, Bin1} = decode_term(Bin),
    {T, Bin2} = decode_list(N-1,Bin1),
    {[H|T], Bin2}.
