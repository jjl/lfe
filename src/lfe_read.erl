%% Copyright (c) 2016 James Laver
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(lfe_read).

%%%%%%%% public

-export([read/2]).

read(<<>>, Pos) -> {error, unexpected, eof, Pos, Pos};
read(Source, Pos) ->
    {Source2,Pos2} = skip_whitespace(Source, Pos),
    <<C, Rest/binary>> = Source2,
    case C of
        ${ -> read_map(Source2,        Pos2);
        $( -> read_list(Source2,       Pos2);
        $\ -> read_char(Source2,       Pos2);
        $[ -> read_tuple(Source2,      Pos2);
        $" -> read_string(Source2,     Pos2);
        $; -> read_comment(Source2,    Pos2);
        $# -> read_special(Source2,    Pos2);
        $| -> read_quoted_sym(Source2, Pos2);
        $) -> {error, unexpected, $), Pos2, Pos2, Rest};
        $] -> {error, unexpected, $], Pos2, Pos2, Rest};
        $} -> {error, unexpected, $}, Pos2, Pos2, Rest};
        _  -> case read_num(Source2, Pos2) of
                  R={ok, _, _, _, _} -> R;
                  _ -> read_bare_sym(Source2, Pos2)
              end
    end.

%%%%%%%% private

%% comments

read_comment(Source, Pos={Line, Col}) ->
    case Source of
        <<";", Rest/binary>> -> read_comment([], Rest, {Line, Col+1}, Pos);
        _ -> {error, expected, $;, Pos}
    end.

read_comment(Acc, Source, Pos={Line, Col}, StartPos) ->
    case Source of
        <<>> -> {ok, finalize_comment(Acc), <<>>, Pos, StartPos};
        <<$\n, Rest/binary>> -> {ok, finalize_comment(Acc), Rest, {Line+1, 0}, StartPos};
        <<C, Rest/binary>> -> read_comment([<<C>>|Acc], Rest, {Line, Col+1}, StartPos)
    end.
    
finalize_comment(C) -> [comment, erlang:iolist_to_binary(lists:reverse(C))].


%% lists

read_list(Source, Pos={Line, Col}) ->
    case Source of
        <<$(, Rest/binary>> -> read_list([], Rest, {Line, Col+1}, Pos);
        _ -> {error, expected, $(, Pos}
    end.

read_list(Acc, Source, Pos, StartPos) ->
    case read(Source, Pos) of
        {ok, T, Rest, {Line, Col}, _}  -> read_list([T|Acc], Rest, {Line, Col}, StartPos);
        {error, unexpected, $), {Line, Col}, StartPos, Rest} ->
            {ok, lists:reverse(Acc), Rest, {Line, Col+1}, StartPos};
        {error, unexpected, eof, _, _} -> {error, unexpected, eof, StartPos, StartPos}
    end.

%% tuples

read_tuple(Source, Pos={Line, Col}) ->
    case Source of
        <<>> -> {error, unexpected, eof};
        <<$[, Rest/binary>> -> read_tuple([], Rest, {Line, Col+1}, Pos);
        _ -> {error, expected, $[, Source, Pos}
    end.

read_tuple(Acc, Source, Pos={Line, Col}, StartPos) ->
    case read(Source, Pos) of
        {ok, T, Rest, {Line, Col}, _}  -> read_tuple([T|Acc], Rest, {Line, Col}, StartPos);
        {error, unexpected, eof, _, _} -> {error, unexpected, eof, StartPos, StartPos};
        {error, unexpected, $], {Line, Col}, StartPos, Rest} ->
            {ok, finalize_tuple(Acc), Rest, {Line, Col+1}, StartPos}
    end.

finalize_tuple(T) -> list_to_tuple(lists:reverse(T)).

%% maps

read_map(Source, Pos={Line, Col}) ->
    case Source of
        <<>> -> {error, unexpected, eof};
        <<${, Rest/binary>> -> read_map([], Rest, {Line, Col+1}, Pos);
        _ -> {error, expected, $[,Source, [{line, Line}, {col, Col}]}
    end.

read_map(Acc, Source, Pos={Line, Col}, StartPos) ->
    case read(Source, Pos) of
        {error, unexpected, eof, _, _} -> {error, unexpected, eof, StartPos, StartPos};
        {error, unexpected, $}, {Line, Col}, StartPos, Rest} ->
            {ok, maps:from_list(Acc), Rest, {Line, Col+1}, StartPos};
        {ok, T, Source2, Pos2, _}  ->
            {ok, T2, Rest, Pos3, _} = read(Source2, Pos2),
            read_map([{T, T2}|Acc], Rest, Pos3, StartPos)
    end.

%% characters

read_char(Source, Pos={Line, Col}) ->
    case Source of
        <<>> -> {error, unexpected, eof, Pos, Pos};
        <<"\\\\\\", Rest/binary>> -> {ok, <<$\\>>, Rest, {Line, Col+3}, Pos};
        <<"\\\\",   Rest/binary>> -> read_esc_char(Rest, {Line, Col+2}, Pos);
        <<"\\", C,  Rest/binary>> -> {ok, <<C>>,   Rest, {Line, Col+2}, Pos};
        _ -> {error, expected, $\\, Source, [{position, Pos}]}
    end.

%% strings

read_string(Source, Pos={Line, Col}) ->
    case Source of
        <<>> -> {error, unexpected, eof};
        <<"\"\"\"", Rest/binary>> -> read_string(<<>>, Rest, {Line, Col+3}, Pos, true);
        <<"\"\"",   Rest/binary>> -> {ok, "", Rest, {Line, Col+2}, Pos};
        <<"\"",     Rest/binary>> -> read_string(<<>>, Rest, {Line, Col+1}, Pos, false);
        _ -> {error, expected, $", Source, [{position, Pos}]}
    end.

read_string(Acc, Source, Pos={Line, Col}, StartPos, true) ->
    case Source of
        <<>>     -> {error, unexpected, eof, Pos, StartPos};
        <<"\"\"\"", Rest/binary>> ->
            {ok, iolist_to_binary(Acc), Rest, {Line, Col+3}, StartPos};
        <<"\\", Rest/binary>> -> 
            {ok, T, Rest, Pos2, _} = read_esc_char(Source, Pos, StartPos),
            read_string([Acc|T], Rest, Pos2, StartPos, false);
        <<C, Rest/binary>> -> read_string([Acc|<<C>>], Rest, {Line, Col+1}, StartPos, false)
    end;
read_string(Acc, Source, Pos={Line, Col}, StartPos, false) ->
    case Source of
        <<>>     -> {error, unexpected, eof, Pos, StartPos};
        <<"\"", Rest/binary>> -> {ok,iolist_to_binary(Acc), Rest, {Line, Col+1}, StartPos};
        <<"\\", Rest/binary>> -> 
            {ok, T, Rest, Pos2, _} = read_esc_char(Source, Pos, StartPos),
            read_string([Acc|T], Rest, Pos2, StartPos, false);
        <<C, Rest/binary>> -> read_string([Acc|<<C>>], Rest, {Line, Col+1}, StartPos,false)
    end.

%% TODO: I should support `\u{LATIN_SMALL_LETTER_A_WITH_DIARESIS}` etc.
read_esc_char(Source, Pos={Line, Col}, StartPos) ->
    case Source of
        <<>>     -> {error, unexpected, eof, Pos, StartPos};
        <<$x, Rest/binary>> -> read_init_digit(Rest, {Line, Col+1}, StartPos, 16);
        <<$u, _/binary>> -> read_esc_uni(Source, Pos);
        <<$b, Rest/binary>> -> {ok, $\b,   Rest, {Line, Col+2}, StartPos};
        <<$t, Rest/binary>> -> {ok, $\t,   Rest, {Line, Col+2}, StartPos};
        <<$n, Rest/binary>> -> {ok, $\n,   Rest, {Line, Col+2}, StartPos};
        <<$v, Rest/binary>> -> {ok, $\v,   Rest, {Line, Col+2}, StartPos};
        <<$f, Rest/binary>> -> {ok, $\f,   Rest, {Line, Col+2}, StartPos};
        <<$r, Rest/binary>> -> {ok, $\r,   Rest, {Line, Col+2}, StartPos};
        <<$e, Rest/binary>> -> {ok, $\e,   Rest, {Line, Col+2}, StartPos};
        <<$s, Rest/binary>> -> {ok, $\s,   Rest, {Line, Col+2}, StartPos};
        <<$d, Rest/binary>> -> {ok, $\d,   Rest, {Line, Col+2}, StartPos};
        <<C,  Rest/binary>> -> {ok, <<C>>, Rest, {Line, Col+2}, StartPos}
    end.
             
%% TODO: `{LATIN_SMALL_LETTER_A_WITH_DIARESIS}` etc.
read_esc_uni(Source, Pos={Line, Col}) ->
    {error, not_implemented, read_esc_uni}.

%% Specials. NOTE: We haven't setled on the final assignments of these

read_regex(Acc, Source, Pos={Line, Col}, StartPos) ->
    case Source of
        <<>> -> {error, unexpected, eof, Pos, StartPos};
        <<$\n, _/binary>>       -> {error, unexpected, $\n, Pos, StartPos};
        <<"\"", Rest/binary>>   -> {ok,iolist_to_binary(Acc), Rest, {Line, Col+1}, StartPos};
        <<"\\\"", Rest/binary>> -> read_string([Acc|"\""], Rest, {Line, Col+2}, StartPos, false);
        <<C, Rest/binary>>      -> read_string([Acc|<<C>>], Rest, {Line, Col+1}, StartPos,false)
    end.

read_liststring(Source, Pos, StartPos) ->
    case read_string([], Source, Pos, StartPos, false) of
        {ok, Ret, Rest, NewPos, _} -> {ok, binary_to_list(Ret), Rest, NewPos, StartPos};
        R -> R
    end.

read_shortfun(Source, Pos, StartPos) ->
    case read_list([], Source, Pos, StartPos) of
        {ok, Ret, Rest, NewPos, _} -> {ok, [rfn|Ret], Rest, NewPos, StartPos};
        R -> R
    end.

%% TODO: FIXME
read_special(Source, Pos={Line, Col}) ->
    case Source of
        <<"##\"", Rest/binary>> -> read_regex([],  Rest, {Line, Col+3}, Pos);
        <<"#\"",  Rest/binary>> -> read_liststring(Rest, {Line, Col+2}, Pos);
        <<"#(",   Rest/binary>> -> read_shortfun(Rest, {Line, Col+2}, Pos);
        _ -> {error, expected, $#, Pos, Pos}
    end.

%% Numbers

%% TODO: fixme, I only support non-negative integers without exponents 
read_num(Source, Pos={Line, Col}) ->
    case Source of
        <<>> -> {error, unexpected, eof, Pos, Pos};
        <<"0r", Rest/binary>> -> read_radix(Rest, {Line, Col+2}, Pos);
        <<"0x", Rest/binary>> -> read_init_digit(Rest, {Line, Col+2}, Pos, 16);
        <<"0o", Rest/binary>> -> read_init_digit(Rest, {Line, Col+2}, Pos, 8);
        <<"0b", Rest/binary>> -> read_init_digit(Rest, {Line, Col+2}, Pos, 2);
        _ -> read_init_digit(Source, Pos, Pos, 10)
        end.

%% TODO: fixme
read_radix(Source, Pos={Line, Col}, StartPos) ->
    case Source of
        <<>> -> {error, unexpected, eof};
        <<A,$:,   Rest/binary>> ->
            true = is_base_n(A,10),
            read_num([], Rest, {Line, Col+2}, StartPos, list_to_integer([A]));
        <<A,B,$:, Rest/binary>> ->
            {true, true} = {is_base_n(A,10),is_base_n(B,10)},
            read_num([], Rest, {Line, Col+3}, StartPos, list_to_integer([B, A]));
        _ -> {error, expected, radix, Pos}
    end.

read_init_digit(Source, Pos={Line, Col}, StartPos, Base) ->
    case Source of
        <<>> -> {error, unexpected, eof, Pos, Pos};
        <<C, Rest/binary>> ->
            case is_base_n(C, Base) of
                true  -> read_num([C], Rest, {Line, Col+1}, StartPos, 10);
                false -> {error, expected, {digit_in_base, Base}, Pos, Pos}
            end
    end.
            
read_num(Acc, Source, {Line, Col}, StartPos, Base) ->
    case Source of 
        <<>> -> finalize_num(Acc, Base);
        <<C, Rest/binary>> ->
            case is_base_n(C, Base) of
                true  -> read_num([C|Acc], Rest, {Line, Col+1}, StartPos, 10);
                false -> finalize_num(Acc, Base)
            end
    end.

finalize_num(N, Base) -> list_to_integer(lists:reverse(N), Base).

%% not supporting capitals is a bit opinionated, but i'm not inclined to change it
base36(C) when C >= $0 andalso C =< $9 -> C - $0;
base36(C) when C >= $a andalso C =< $z -> C + 10 - $a;
base36(_) -> error.

is_base_n(C, B) ->
    case base36(C) of
        error -> false;
        B2 -> B2 < B
    end.

%% symbols

%% TODO: fixme
% we need to clarify precisely what should be legal
read_bare_sym(Source, Pos={Line, Col}) ->
    {error,not_implemented}.

%% TODO: fixme
% should we allow these to go multiline? in which case how about escape chars?
read_quoted_sym(Source, Pos={Line, Col}) ->
    case Source of 
        <<>> -> {error, unexpected, eof};
        <<$|, Source2/binary>> ->
            case Source2 of
                <<>> -> {error, unexpected, eof};
                <<C, Rest/binary>> when C /= $| -> read_quoted_sym([C], Rest, {Line, Col+2}, Pos);
                _ -> {error, unexpected, $|, {Line, Col+1}, Pos}
            end
    end.
%        <<$b, Rest/binary>> -> {ok, $\b,   Rest, {Line, Col+2}, StartPos};
    
read_quoted_sym(Acc, Source, {Line, Col}, StartPos) ->
    case Source of 
        <<>> -> {error, unexpected, eof};
        <<$|, Rest/binary>> -> {ok, lists:reverse(Acc), Rest, {Line, Col+1}, StartPos};
        <<C, Rest/binary>> when C /= $\n -> read_quoted_sym([C], Rest, {Line, Col+1}, StartPos);
        _ -> {error, unexpected, $|, {Line, Col+1}, StartPos}
    end.

%% Whitespace, of which we have a special definition

is_ws($\t) -> true;
is_ws($\v) -> true;
is_ws($\f) -> true;
is_ws($\r) -> true;
is_ws(_) -> false.


skip_whitespace(Source, Pos) ->
    skip_whitespace(Source, Pos, Pos).

skip_whitespace(<<>>, Pos, StartPos) -> {ok, <<>>, <<>>, Pos, StartPos};
skip_whitespace(Source, Pos={Line, Col}, StartPos) ->
    <<C, Rest/binary>> = Source,
    case C of
        $\n -> skip_whitespace(Rest, {Line+1, 0}, StartPos);
        _   -> case is_ws(C) of
                   true  -> skip_whitespace(Rest, {Line, Col+1}, StartPos);
                   false -> {ok, <<>>, Source, Pos, StartPos}
             end
    end.
