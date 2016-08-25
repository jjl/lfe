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

-module(lemon_read).

% macros

-define(INT(C),      is_integer(C)).
% Deliberately dumb
-define(SPACE(C),    (C =:= 32 orelse C =:= $\r orelse C =:= $\n)).
-define(UNISPACE(C), (is_integer(C) andalso
                      (C =:= 133 orelse C =:= 160 orelse C =:= 5760 orelse
                       C =:= 8232 orelse C =:= 8233 orelse C =:= 8239 orelse
                       C =:= 8287 orelse C =:= 12288 orelse
		       (C >= 8192 andalso C =< 8202)))).
-define(CONTROL(C),  (?INT(C) andalso C < 32 andalso not ?SPACE(C))).
-define(BANNED(C),   (?UNISPACE(C) orelse ?CONTROL(C))).
-define(LOWER(C),    (?INT(C) andalso C >= $a andalso C =< $z)).
-define(UPPER(C),    (?INT(C) andalso C >= $A andalso C =< $Z)).
-define(BASE10(C),   (?INT(C) andalso C >= $0 andalso C =< $9)).
-define(ALPHA(C),    (?LOWER(C) orelse ?UPPER(C))).
-define(BASE36(C),   (?ALPHA(C) orelse ?BASE10(C))).
-define(SYNTAX(C),   (?INT(C) andalso
		      (C =:= $# orelse C =:= $|  orelse C =:= $( orelse C =:= $) orelse
		       C =:= $[ orelse C =:= $]  orelse C =:= ${ orelse C =:= $} orelse
		       C =:= $' orelse C =:= $\\ orelse C =:= $; orelse C =:= $" orelse
		       C =:= $;  orelse C =:= $"))).
-define(ATOM(C),     (?INT(C) andalso not ?SYNTAX(C) andalso
		      not ?SPACE(C) andalso not ?BANNED(C))).
-define(ATOM1(C),    (?INT(C) andalso C /= $/ andalso not ?BASE10(C) andalso ?ATOM(C))).
-define(QSYM(C),     (?INT(C) andalso C /= $| andalso C /= $\n)).

% dialyzer

-type position() :: {non_neg_integer(), non_neg_integer()}.
-type positions() :: {position(), position()}.
-type state() :: {binary(), positions()}.
-type goodreturn() :: { 'ok', any(), state()}.
-type errortype() :: 'unexpected' | 'expected'.
-type errorreturn() :: {'error', errortype(), atom() | pos_integer(), positions()}.
-type readreturn() :: goodreturn() | errorreturn().

%%%%%%%% public

-export([read/1, read/3]).

read(Source) -> read(Source, {0,0}, unset). %% minor ick, massive deduplication
read(<<>>, Pos, StartPos) -> {error, unexpected, eof, Pos, StartPos};
read(Source, Pos, StartPos) ->
    {ok, _, Source2,Pos2={Line,Col}, _} = skip_whitespace(Source, Pos),
    StartPos2 = case StartPos of unset -> Pos2; _ -> StartPos end,
    case Source2 of
	<<>> -> {error, unexpected, eof, Pos2, StartPos2};
	<<C/utf8, Rest/binary>> ->
	    NuPos={Line,Col+1},
	    case C of
		$'  -> read_quote(Rest, Pos2, StartPos2);
		$\\ -> read_char(Rest, NuPos, StartPos2);
		$#  -> read_special(Rest, NuPos, StartPos2);
		$;  -> read_comment(Rest, NuPos, StartPos2);
		${  -> read_map(Rest, NuPos, StartPos2);
		$(  -> read_list(Rest, NuPos, StartPos2);
		$[  -> read_tuple(Rest, NuPos, StartPos2);
		$"  -> read_string(Rest, NuPos,  StartPos2);
		$|  -> read_quoted_atom(Rest, NuPos, StartPos2);
		_ when C =:= $- orelse ?BASE10(C)  -> read_num(Source2, Pos2, StartPos2);
		_ when ?BANNED(C) -> {error, illegal_char, C, Pos2, StartPos2};
		_ when ?ATOM1(C)   -> read_atom(Source2, NuPos, StartPos2);
		_ -> {error, unexpected, C, Pos2, Pos2}
	    end
    end.

%%%%%%%% private

%% quote

%% -spec(read_quote( binary(), position(), position() ) -> goodreturn()).

read_quote(Source, Pos, StartPos) ->
    {ok, T, Rest, Pos2, _} = read(Source, Pos, StartPos),
    {ok, [quote, T], Rest, Pos2, StartPos}.

%% comments

read_comment(Source, {Line, _}, StartPos) ->
    {C,Rest} = binary:split(Source,<<$\n>>),
    {ok, [comment, C], Rest, {Line+1, 0}, StartPos}.

%% lists, tuples, maps

read_list(Source, Pos, StartPos) ->
    read_coll([], Source, Pos, StartPos, <<$)>>).

read_tuple(Source, Pos, StartPos) ->
    case read_coll([], Source, Pos, StartPos, <<$]>>) of
	{ok, T, Rest, Pos2, _} -> {ok, list_to_tuple(T), Rest, Pos2, StartPos};
	R -> R
    end.

read_map(Source, Pos, StartPos) ->
    case read_pairs([], Source, Pos, StartPos, <<$}>>) of
	{ok, T, Rest, Pos2, _} -> {ok, maps:from_list(T), Rest, Pos2, StartPos};
	R -> R
    end.
		
read_form_or_end(Source, Pos, StartPos, End) ->
    {ok, _, Source2, Pos2={Line, Col}, _} = skip_whitespace(Source, Pos, StartPos),
    BS = byte_size(End),
    case binary:longest_common_prefix([End, Source2]) of
        L when BS =:= L ->
            BP = binary:part(Source, byte_size(End), byte_size(Source) - byte_size(End)),
            {done, $_, BP, {Line, Col+BS}, StartPos};
        _ -> read(Source2, Pos2, StartPos)
    end.

read_coll(Acc, Source, Pos, StartPos, End) ->
    io:format("read_coll: ~p~n", [[Acc, Source, Pos, StartPos, End]]),
    case read_form_or_end(Source, Pos, StartPos, End) of
        {done, V, Source2, Pos2, _} -> {ok, lists:reverse([V|Acc]), Source2, Pos2, StartPos};
        {ok, V, Source2, Pos2, _} -> read_coll([V|Acc], Source2, Pos2, StartPos, End)
    end.

read_pairs(Acc, Source, Pos, StartPos, End) ->
    case read_form_or_end(Source, Pos, StartPos, End) of
        {ok, V, Source2, Pos2, _} -> % 
            case read_form_or_end(Source2, Pos2, StartPos, End) of
                {ok, V2, Source3, Pos3, _} ->
                    read_pairs([{V,V2}|Acc], Source3, Pos3, StartPos, End);
                {done, _, _, Pos3, _} -> {error, uneven_map, [V|Acc], Pos3, StartPos};
                R -> R
            end;
        {done, _, Rest, Pos2, _} -> {ok, lists:reverse(Acc), Rest, Pos2, StartPos};
        R -> R
    end.

%% strings

read_string(Source, Pos={Line, Col}, StartPos) ->
    case Source of
        <<$\",$\", Rest/binary>> -> read_string("", Rest, {Line, Col+2}, StartPos, <<$\",$\",$\">>);
        <<$\",     Rest/binary>> -> {ok, "", Rest, {Line, Col+1}, StartPos};
        _ -> read_string("", Source, Pos, Pos, <<$\">>)
    end.

read_string(Acc, Source, Pos={Line, Col}, StartPos, End) ->
    io:format("read_string: ~p~n", [[Acc, Source, Pos, StartPos, End]]),
    case Source of
        <<>> -> {error, unexpected, eof, Pos, StartPos};
        <<$\\, _/binary>> -> 
            {ok, T, Rest, Pos2, _} = read_char(Source, Pos, StartPos),
            read_string([T|Acc], Rest, Pos2, StartPos, End);
        <<C/utf8, Rest/binary>> ->
            BS = byte_size(End),
            case binary:longest_common_prefix([End, Source]) of
                L when BS =:= L ->
                    BP = binary:part(Source, byte_size(End), byte_size(Source) - byte_size(End)),
                    {ok, lists:reverse(Acc), BP, {Line, Col+BS}, StartPos};
                _ -> read_string([C|Acc], Rest, {Line, Col+1}, StartPos, End)
            end
    end.

%% I think \x should be optionally terminated by a semicolon
read_char(Source, Pos={Line, Col}, StartPos) ->
    NuPos = {Line, Col+1},
    case Source of
        <<>>     -> {error, unexpected, eof, Pos, StartPos};
        <<$\\, $x, Source2/binary>> -> raw_read_int([], Source2, NuPos, StartPos, 16);
        <<$\\, $u, _/binary>> -> read_uni_char(Source, Pos);
        <<$\\, $b, Rest/binary>> -> {ok, $\b,   Rest, NuPos, StartPos};
        <<$\\, $t, Rest/binary>> -> {ok, $\t,   Rest, NuPos, StartPos};
        <<$\\, $n, Rest/binary>> -> {ok, $\n,   Rest, NuPos, StartPos};
        <<$\\, $v, Rest/binary>> -> {ok, $\v,   Rest, NuPos, StartPos};
        <<$\\, $f, Rest/binary>> -> {ok, $\f,   Rest, NuPos, StartPos};
        <<$\\, $r, Rest/binary>> -> {ok, $\r,   Rest, NuPos, StartPos};
        <<$\\, $e, Rest/binary>> -> {ok, $\e,   Rest, NuPos, StartPos};
        <<$\\, $s, Rest/binary>> -> {ok, $\s,   Rest, NuPos, StartPos};
        <<$\\, $d, Rest/binary>> -> {ok, $\d,   Rest, NuPos, StartPos};
        <<C/utf8,  Rest/binary>> -> {ok, C, Rest, NuPos, StartPos}
    end.
             
%% TODO: `{LATIN_SMALL_LETTER_A_WITH_DIARESIS}` etc.
read_uni_char(Source, Pos={Line, Col}) ->
    {error, not_implemented, unicode_escape_syntax}.

%% Specials. NOTE: We haven't setled on the final assignments of these

read_regex(Acc, Source, Pos={Line, Col}, StartPos) ->
    case Source of
        <<>> -> {error, unexpected, eof, Pos, StartPos};
        <<$\n, _/binary>>       -> {error, unexpected, $\n, Pos, StartPos};
        <<"\"", Rest/binary>>   -> {ok, list_to_binary(lists:reverse(Acc)), Rest, {Line, Col+1}, StartPos};
        <<"\\\"", Rest/binary>> -> read_regex([$\"|Acc], Rest, {Line, Col+2}, StartPos);
        <<C/utf8, Rest/binary>>      -> read_regex([C|Acc], Rest, {Line, Col+1}, StartPos)
    end.

read_binstring(Source, Pos, StartPos) ->
    io:format("read_binstring: ~p~n", [[Source, Pos, StartPos]]),
    case read_string(Source, Pos, StartPos) of
        {ok, Ret, Rest, NewPos, _} -> {ok, list_to_binary(Ret), Rest, NewPos, StartPos};
        R -> R
    end.

read_shortfun(Source, Pos, StartPos) ->
    case read_list(Source, Pos, StartPos) of
        {ok, Ret, Rest, NewPos, _} -> {ok, [rfn|Ret], Rest, NewPos, StartPos};
        R -> R
    end.

read_special(Source, Pos={Line, Col},StartPos) ->
    case Source of
        <<$#, $", Rest/binary>> -> read_regex([], Rest, {Line, Col+2}, StartPos);
        <<$",     Rest/binary>> -> read_binstring(Rest, {Line, Col+1}, StartPos);
        <<$(,     Rest/binary>> -> read_shortfun(Rest,  {Line, Col+1}, StartPos);
        _ -> {error, expected, $#, Pos, StartPos}
    end.

%% Numbers


%% TODO: fixme, I only support non-negative integers without exponents 
read_num(Source, Pos={Line, Col}, StartPos) ->
    case Source of
        <<>> -> {error, unexpected, eof, Pos, StartPos};
        <<"0r", Rest/binary>> -> read_radix(Rest, {Line, Col+2}, StartPos);
        <<"0x", Rest/binary>> -> read_init_digit(Rest, {Line, Col+2}, StartPos, 16, fun read_num/5);
        <<"0o", Rest/binary>> -> read_init_digit(Rest, {Line, Col+2}, StartPos, 8,  fun read_num/5);
        <<"0b", Rest/binary>> -> read_init_digit(Rest, {Line, Col+2}, StartPos, 2,  fun read_num/5);
        _ -> read_init_digit(Source, Pos, Pos, 10, fun read_num/5)
        end.

read_radix(Source, Pos={Line, Col}, StartPos) ->
    case Source of
        <<>> -> {error, unexpected, eof};
        <<A,$:,   Rest/binary>> when ?BASE10(A) ->
            read_num([], Rest, {Line, Col+2}, StartPos, list_to_integer([A]));
        <<A,B,$:, Rest/binary>> when ?BASE10(A) andalso ?BASE10(B) ->
            read_num([], Rest, {Line, Col+3}, StartPos, list_to_integer([B, A]));
        _ -> {error, expected, radix_in_decimal, Pos}
    end.

read_init_digit(Source,Pos,StartPos,Base,Basic) -> read_init_digit([], Source, Pos, StartPos, Base, Basic).

read_init_digit(Acc, Source, Pos={Line, Col}, StartPos, Base, Next) ->
    case Source of
        <<>> -> {error, unexpected, eof, Pos, Pos};
        <<$+, C/utf8, Rest/binary>> ->
            case is_base_n(C, Base) of
                true  -> Next([C], Rest, {Line, Col+1}, StartPos, 10);
                false -> {error, expected, {one_of, [$-, {digit_in_base, Base}]}, Pos, Pos}
            end;
        <<$-, C/utf8, Rest/binary>> ->
            case is_base_n(C, Base) of
                true  -> Next([C,$-], Rest, {Line, Col+1}, StartPos, 10);
                false -> {error, expected, {one_of, [$-, {digit_in_base, Base}]}, Pos, Pos}
            end;
        <<C/utf8, Rest/binary>> ->
            case is_base_n(C, Base) of
                true  -> Next([C], Rest, {Line, Col+1}, StartPos, 10);
                false -> {error, expected, {one_of, [$-, {digit_in_base, Base}]}, Pos, Pos}
            end
    end.
            
check_num(N, Rest, Pos, StartPos) ->
    case N of
	<<>> -> {error, unexpected, eof, Pos, StartPos};
        _ -> {ok, N, Rest, Pos, StartPos}
    end.

raw_read_int(Acc, Source, Pos={Line, Col}, StartPos, Base) ->
    case Source of 
        <<>> -> check_num(Acc, Source, Pos, StartPos);
        <<$_, Rest/binary>> -> raw_read_int(Acc, Rest, {Line, Col+1}, StartPos, 10);
        <<C/utf8, Rest/binary>> ->
            case is_base_n(C, Base) of
                true  -> raw_read_int([C|Acc], Rest, {Line, Col+1}, StartPos, 10);
                false -> check_num(Acc, Source, Pos, StartPos)
            end
    end.

read_num(Acc, Source, Pos, StartPos, Base) ->
    case raw_read_int(Acc, Source, Pos, StartPos, Base) of
        {ok, Acc2, Rest, Pos2, _} -> maybe_extend_num(Acc2, Base, Rest, Pos2, StartPos);
        R -> R
    end.

maybe_exp(I, Base, Source, Pos={Line, Col}, StartPos) ->
    case Source of
        <<$e, $+, Source2/binary>> ->
            case raw_read_int("", Source2, {Line, Col+1}, StartPos, Base) of
                {ok, I2, Rest, Pos2, _} ->
                    I3 = I * math:pow(10, list_to_integer(lists:reverse(I2))),
                    I4 = if is_integer(I) -> list_to_integer(float_to_list(I3,[{decimals, 0}]));
                            true -> I3
                         end,
                    {ok, I4, Rest, Pos2, StartPos};
                R -> R
            end;
        <<$e, $-, Source2/binary>> ->
            case raw_read_int("-", Source2, {Line, Col+1}, StartPos, Base) of
                {ok, I2, Rest, Pos2, _} ->
                    I3 = I * math:pow(10, list_to_integer(lists:reverse(I2))),
                    I4 = if is_integer(I) -> list_to_integer(float_to_list(I3,[{decimals, 0}]));
                            true -> I3
                         end,
                    {ok, I4, Rest, Pos2, StartPos};
                R -> R
            end;
        <<$e, Source2/binary>> ->
            case raw_read_int("", Source2, {Line, Col+1}, StartPos, Base) of
                {ok, I2, Rest, Pos2, _} ->
                    I3 = I * math:pow(10, list_to_integer(lists:reverse(I2))),
                    I4 = if is_integer(I) -> trunc(I3); true -> I3 end,
                    {ok, I4, Rest, Pos2, StartPos};
                R -> R
            end;
        <<$;, Rest/binary>> -> {ok, I, Rest, {Line, Col+1}, StartPos};
        _ -> {ok, I, Source, Pos, StartPos}
    end.
            
maybe_float(Acc, Base, Source, Pos={Line, Col}, StartPos) ->
    case Source of
        <<$., Source2/binary>> ->
            case raw_read_int("", Source2, {Line, Col+1}, StartPos, Base) of
                {ok, I, Rest, Pos2, _} ->
                    A1=lists:reverse(Acc),
                    A2=lists:reverse(I),
                    {ok, list_to_float(A1 ++ [$.|A2]), Rest, Pos2, StartPos};
                R -> R
            end;
        _ -> {ok, list_to_integer(lists:reverse(Acc)), Source, Pos, StartPos}
    end.

maybe_extend_num(Acc, Base, Source, Pos, StartPos) ->
    {ok, Acc2, Rest, Pos2, _} = maybe_float(Acc,Base,Source,Pos, StartPos),
    maybe_exp(Acc2, Base, Rest, Pos2, StartPos).

%% not supporting capitals is a bit opinionated, but i'm not inclined to change it
base36(C) when ?BASE10(C) -> C - $0;
base36(C) when ?LOWER(C)  -> C + 10 - $a;
base36(_) -> 36. % never < 36

is_base_n(C, B) -> base36(C) < B.

%% atoms

quoted_atom_char(C) -> ?QSYM(C).
atom_char(C) -> ?ATOM(C).

read_atom(Source, Pos, StartPos) ->
    {ok, S, Rest, Pos2,_} = read_while("", Source, Pos, StartPos, fun atom_char/1, atom_char),
    {ok, list_to_atom(S), Rest, Pos2, StartPos}.

read_quoted_atom(Source, Pos, StartPos) ->
    {ok, S, Source2, Pos2={Line, Col}, _} = read_while("", Source, Pos, StartPos, fun quoted_atom_char/1, quoted_atom_char),
    case Source2 of
        <<>> -> {error, unexpected, eof, Pos, StartPos};
        <<C/utf8, Rest/binary>> ->
            case C of
                $| -> {ok, list_to_atom(S), Rest, {Line, Col+1}, StartPos};
                _ -> {error, expected, $|, Pos2, StartPos}
            end
    end.

read_while(Acc, Source, Pos={Line, Col}, StartPos, Fun, Name) ->
    case Source of
	<<>> -> finalize_while(Acc, Source, Pos, StartPos, Name);
        <<C/utf8, Rest/binary>> ->
            case Fun(C) of
                true -> read_while([C|Acc], Rest,
                                   case C of $\n -> {Line+1,0}; _ -> {Line, Col+1} end,
                                   StartPos, Fun, Name);
                false -> finalize_while(Acc, Source, Pos, StartPos, Name)
            end
    end.

finalize_while(Acc, Source, Pos, StartPos, Name) ->
    case Acc of
        [] -> {error, expected, Name, Pos, StartPos};
        _ -> {ok, lists:reverse(Acc), Source, Pos, StartPos}
    end.

%% Whitespace, of which we have a special definition

skip_whitespace(Source, Pos) ->
    skip_whitespace(Source, Pos, Pos).

skip_whitespace(<<>>, Pos, StartPos) -> {ok, <<>>, <<>>, Pos, StartPos};
skip_whitespace(Source, Pos={Line, Col}, StartPos) ->
    <<C/utf8, Rest/binary>> = Source,
    case C of
        $\n -> skip_whitespace(Rest, {Line+1, 0}, StartPos);
        _ when ?SPACE(C) -> skip_whitespace(Rest, {Line, Col+1}, StartPos);
        _ -> {ok, <<>>, Source, Pos, StartPos}
    end.
