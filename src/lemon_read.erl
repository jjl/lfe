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
-type goodtuple(T) :: {'ok', T, state()}.
-type unexpected() :: non_neg_integer() | 'eof'.
-type expected() :: atom().
-type numberbase() :: 2..36.
-type err_expected() :: {'error', 'expected', expected(), positions()}.
-type err_unexpected() :: {'error', 'unexpected', unexpected(), positions()}.
-type badtuple() :: err_expected() | err_unexpected().
-type returntuple(T) :: goodtuple(T) | badtuple().

%%%%%%%% public<

-export([read/1, read/2]).

-spec read(binary()) -> returntuple(any()).
-spec read(binary(), positions() | {position(),'unset'}) -> returntuple(any).

read(Source) -> read(Source, {{0,0}, unset}). %% minor ick, massive deduplication
read(Source, Pos) ->
    {ok, _, Source2,Pos2={{Line,Col}, StartPos}} = skip_whitespace(Source, Pos),
    StartPos2 = case StartPos of unset -> Pos2; _ -> StartPos end,
    case Source2 of
	<<>> -> {error, unexpected, eof, {{Line, Col}, StartPos2}};
	<<C/utf8, Rest/binary>> ->
	    NuPos={{Line,Col+1},StartPos2},
	    case C of
		$'  -> read_quote(Rest, NuPos);
		$\\ -> read_char(Rest, NuPos);
		$#  -> read_special(Rest, NuPos);
		$;  -> read_comment(Rest, NuPos);
		${  -> read_map(Rest, NuPos);
		$(  -> read_list(Rest, NuPos);
		$[  -> read_tuple(Rest, NuPos);
		$"  -> read_string(Rest, NuPos);
		$|  -> read_quoted_atom(Rest, NuPos);
		_ when C =:= $- orelse ?BASE10(C)  -> read_num(Source2, Pos2);
		_ when ?BANNED(C) -> {error, illegal_char, C, Pos2};
		_ when ?ATOM1(C)   -> read_atom(Source2, NuPos);
		_ -> {error, unexpected, C, Pos2}
	    end
    end.

%%%%%%%% private

%% quote

-spec read_quote(binary(), positions()) -> returntuple(any()).

read_quote(Source, Pos) ->
    case read(Source, Pos) of
        {ok, T, Rest, Pos2} -> {ok, [quote, T], Rest, Pos2};
        R -> R
    end.

%% comments

-spec read_comment(binary(), tuple()) -> goodtuple(['comment' | binary(),...]).

read_comment(Source, {{Line, _}, StartPos}) ->
    [C,Rest] = binary:split(Source,<<$\n>>),
    {ok, [comment, C], Rest, {{Line+1, 0}, StartPos}}.

%% lists, tuples, maps

-spec read_list(binary(), positions()) -> returntuple(list()).

read_list(Source, {Pos, StartPos}) ->
    read_coll([], Source, {Pos, StartPos}, <<$)>>).

-spec read_tuple(binary(), positions()) -> returntuple(tuple()).

read_tuple(Source, {Pos, StartPos}) ->
    case read_coll([], Source, {Pos, StartPos}, <<$]>>) of
	{ok, T, Rest, Pos2} -> {ok, list_to_tuple(T), Rest, Pos2};
	R -> R
    end.

-spec read_map(binary(), positions()) -> returntuple(map()).

read_map(Source, Pos) ->
    case read_pairs([], Source, Pos, <<$}>>) of
	{ok, T, Rest, Pos2} -> {ok, maps:from_list(T), Rest, Pos2};
	R -> R
    end.
		
-spec read_form_or_end(binary(), positions(), binary()) -> returntuple(any()).

read_form_or_end(Source, Pos, End) ->
    {ok, _, Source2, Pos2={{Line, Col},StartPos}} = skip_whitespace(Source, Pos),
    BS = byte_size(End),
    case binary:longest_common_prefix([End, Source2]) of
        L when BS =:= L ->
            BP = binary:part(Source, byte_size(End), byte_size(Source) - byte_size(End)),
            {done, $_, BP, {{Line, Col+BS}, StartPos}};
        _ -> read(Source2, Pos2)
    end.

-spec read_coll(list(), binary(), positions(), binary()) -> returntuple(list()).

read_coll(Acc, Source, Pos, End) ->
    case read_form_or_end(Source, Pos, End) of
        {done, V, Source2, Pos2} -> {ok, lists:reverse([V|Acc]), Source2, Pos2};
        {ok, V, Source2, Pos2} -> read_coll([V|Acc], Source2, Pos2, End);
        R -> R
    end.

-spec read_pairs(list(), binary(), positions(), binary()) -> returntuple(list()).

read_pairs(Acc, Source, Pos, End) ->
    case read_form_or_end(Source, Pos, End) of
        {ok, V, Source2, Pos2} -> % 
            case read_form_or_end(Source2, Pos2, End) of
                {ok, V2, Source3, Pos3} ->
                    read_pairs([{V,V2}|Acc], Source3, Pos3, End);
                {done, _, _, Pos3} -> {error, uneven_map, [V|Acc], Pos3};
                R -> R
            end;
        {done, _, Rest, Pos2} -> {ok, lists:reverse(Acc), Rest, Pos2};
        R -> R
    end.

%% strings

-spec read_string(binary(), positions()) -> returntuple(string()).

read_string(Source, Pos={{Line, Col}, StartPos}) ->
    case Source of
        <<$\",$\", Rest/binary>> -> read_string("", Rest, {{Line, Col+2}, StartPos}, <<$\",$\",$\">>);
        <<$\",     Rest/binary>> -> {ok, "", Rest, {{Line, Col+1}, StartPos}};
        _ -> read_string("", Source, Pos, <<$\">>)
    end.

-spec read_string(string(), binary(), positions(), binary()) -> returntuple(string()).

read_string(Acc, Source, Pos={{Line, Col}, StartPos}, End) ->
    case Source of
        <<>> -> {error, unexpected, eof, Pos};
        <<$\\, _/binary>> -> 
            {ok, T, Rest, Pos2} = read_char(Source, Pos),
            read_string([T|Acc], Rest, Pos2, End);
        <<C/utf8, Rest/binary>> ->
            BS = byte_size(End),
            case binary:longest_common_prefix([End, Source]) of
                L when BS =:= L ->
                    BP = binary:part(Source, byte_size(End), byte_size(Source) - byte_size(End)),
                    {ok, lists:reverse(Acc), BP, {{Line, Col+BS}, StartPos}};
                _ -> read_string([C|Acc], Rest, {{Line, Col+1}, StartPos}, End)
            end
    end.

-spec read_char(binary(), positions()) -> returntuple(non_neg_integer()).

read_char(Source, Pos={{Line, Col}, StartPos}) ->
    NuPos = {{Line, Col+1}, StartPos},
    case Source of
        <<>>     -> {error, unexpected, eof, Pos};
        <<$\\, $x, Source2/binary>> -> raw_read_int([], Source2, NuPos, 16);
        <<$\\, $u, _/binary>> -> read_uni_char(Source, Pos);
        <<$\\, $b, Rest/binary>> -> {ok, $\b,   Rest, NuPos};
        <<$\\, $t, Rest/binary>> -> {ok, $\t,   Rest, NuPos};
        <<$\\, $n, Rest/binary>> -> {ok, $\n,   Rest, NuPos};
        <<$\\, $v, Rest/binary>> -> {ok, $\v,   Rest, NuPos};
        <<$\\, $f, Rest/binary>> -> {ok, $\f,   Rest, NuPos};
        <<$\\, $r, Rest/binary>> -> {ok, $\r,   Rest, NuPos};
        <<$\\, $e, Rest/binary>> -> {ok, $\e,   Rest, NuPos};
        <<$\\, $s, Rest/binary>> -> {ok, $\s,   Rest, NuPos};
        <<$\\, $d, Rest/binary>> -> {ok, $\d,   Rest, NuPos};
        <<C/utf8,  Rest/binary>> -> {ok, C,     Rest, NuPos}
    end.

             
%% TODO: `{LATIN_SMALL_LETTER_A_WITH_DIARESIS}` etc.
read_uni_char(Source, Pos={Line, Col}) ->
    {error, not_implemented, unicode_escape_syntax}.

-spec read_regex(string(), binary(), positions()) -> returntuple(binary()).

read_regex(Acc, Source, Pos={{Line, Col}, StartPos}) ->
    case Source of
        <<>> -> {error, unexpected, eof, Pos};
        <<$\n, _/binary>>       -> {error, unexpected, $\n, Pos};
        <<"\"", Rest/binary>>   -> {ok, list_to_binary(lists:reverse(Acc)), Rest, {{Line, Col+1}, StartPos}};
        <<"\\\"", Rest/binary>> -> read_regex([$\"|Acc], Rest, {{Line, Col+2}, StartPos});
        <<C/utf8, Rest/binary>> -> read_regex([C|Acc], Rest, {{Line, Col+1}, StartPos})
    end.

-spec read_binstring(binary(), positions()) -> returntuple(binary()).

read_binstring(Source, Pos) ->
    case read_string(Source, Pos) of
        {ok, Ret, Rest, NewPos} -> {ok, list_to_binary(Ret), Rest, NewPos};
        R -> R
    end.

-spec read_shortfun(binary(), positions()) -> returntuple(list()).

read_shortfun(Source, Pos) ->
    case read_list(Source, Pos) of
        {ok, Ret, Rest, NewPos} -> {ok, [rfn|Ret], Rest, NewPos};
        R -> R
    end.

-spec read_special(binary(), positions()) -> returntuple(any()).

read_special(Source, Pos={{Line, Col},StartPos}) ->
    case Source of
        <<$#, $", Rest/binary>> -> read_regex([], Rest, {{Line, Col+2}, StartPos});
        <<$",     Rest/binary>> -> read_binstring(Rest, {{Line, Col+1}, StartPos});
        <<$(,     Rest/binary>> -> read_shortfun(Rest,  {{Line, Col+1}, StartPos});
        _ -> {error, expected, $#, Pos}
    end.

%% Numbers

-spec read_num(binary(), positions()) -> returntuple(number()).

read_num(Source, Pos={{Line, Col}, StartPos}) ->
    case Source of
        <<>> -> {error, unexpected, eof, Pos};
        <<"0r", Rest/binary>> -> read_radix(Rest, {{Line, Col+2}, StartPos});
        <<"0x", Rest/binary>> -> read_init_digit(Rest, {{Line, Col+2}, StartPos}, 16, fun read_num/4);
        <<"0o", Rest/binary>> -> read_init_digit(Rest, {{Line, Col+2}, StartPos}, 8,  fun read_num/4);
        <<"0b", Rest/binary>> -> read_init_digit(Rest, {{Line, Col+2}, StartPos}, 2,  fun read_num/4);
        _ -> read_init_digit(Source, Pos, 10, fun read_num/4)
        end.

-spec read_radix(binary(), positions()) -> returntuple(number()).

read_radix(Source, Pos={{Line, Col}, StartPos}) ->
    case Source of
        <<>> -> {error, unexpected, eof, Pos};
        <<A,$:,   Rest/binary>> when ?BASE10(A) ->
            read_num([], Rest, {{Line, Col+2}, StartPos}, list_to_integer([A]));
        <<A,B,$:, Rest/binary>> when ?BASE10(A) andalso ?BASE10(B) ->
            read_num([], Rest, {{Line, Col+3}, StartPos}, list_to_integer([B, A]));
        _ -> {error, expected, radix_in_decimal, Pos}
    end.

-spec read_init_digit(binary(), positions(), numberbase(), fun((string(), binary(), positions(), numberbase()) -> returntuple(number()))) -> returntuple(number()).

read_init_digit(Source, Pos={{Line, Col}, StartPos}, Base, Next) ->
    NuPos = {{Line, Col+1}, StartPos},
    case Source of
        <<>> -> {error, unexpected, eof, Pos};
        <<$+, C/utf8, Rest/binary>> ->
            case is_base_n(C, Base) of
                true  -> Next([C], Rest, NuPos, 10);
                false -> {error, expected, {one_of, [$-, {digit_in_base, Base}]}, Pos}
            end;
        <<$-, C/utf8, Rest/binary>> ->
            case is_base_n(C, Base) of
                true  -> Next([C,$-], Rest, NuPos, 10);
                false -> {error, expected, {one_of, [$-, {digit_in_base, Base}]}, Pos}
            end;
        <<C/utf8, Rest/binary>> ->
            case is_base_n(C, Base) of
                true  -> Next([C], Rest, NuPos, 10);
                false -> {error, expected, {one_of, [$-, {digit_in_base, Base}]}, Pos}
            end
    end.
            
-spec check_num(string(), binary(), positions()) -> returntuple(string()).

check_num(N, Rest, Pos) ->
    case N of
	<<>> -> {error, unexpected, eof, Pos};
        _ -> {ok, N, Rest, Pos}
    end.

-spec raw_read_int(string(), binary(), positions(), pos_integer()) -> returntuple(string()).

raw_read_int(Acc, Source, Pos={{Line, Col}, StartPos}, Base) ->
    NuPos = {{Line, Col+1}, StartPos},
    case Source of 
        <<>> -> check_num(Acc, Source, Pos);
        <<$_, Rest/binary>> -> raw_read_int(Acc, Rest, NuPos, 10);
        <<C/utf8, Rest/binary>> ->
            case is_base_n(C, Base) of
                true  -> raw_read_int([C|Acc], Rest, NuPos, 10);
                false -> check_num(Acc, Source, Pos)
            end
    end.

-spec read_num(list(), binary(), positions(), numberbase()) -> returntuple(number()).

read_num(Acc, Source, Pos, Base) ->
    case raw_read_int(Acc, Source, Pos, Base) of
        {ok, Acc2, Rest, Pos2} -> maybe_extend_num(Acc2, Base, Rest, Pos2);
        R -> R
    end.

-spec maybe_exp(number(), numberbase(), binary(), positions()) -> returntuple(number()).

maybe_exp(I, Base, Source, Pos={{Line, Col}, StartPos}) ->
    NuPos = {{Line, Col+1}, StartPos},
    case Source of
        <<$e, $+, Source2/binary>> ->
            case raw_read_int("", Source2, NuPos, Base) of
                {ok, I2, Rest, Pos2} ->
                    I3 = I * math:pow(10, list_to_integer(lists:reverse(I2))),
                    I4 = if is_integer(I) -> list_to_integer(float_to_list(I3,[{decimals, 0}]));
                            true -> I3
                         end,
                    {ok, I4, Rest, Pos2};
                R -> R
            end;
        <<$e, $-, Source2/binary>> ->
            case raw_read_int("-", Source2, NuPos, Base) of
                {ok, I2, Rest, Pos2} ->
                    I3 = I * math:pow(10, list_to_integer(lists:reverse(I2))),
                    I4 = if is_integer(I) -> list_to_integer(float_to_list(I3,[{decimals, 0}]));
                            true -> I3
                         end,
                    {ok, I4, Rest, Pos2};
                R -> R
            end;
        <<$e, Source2/binary>> ->
            case raw_read_int("", Source2, NuPos, Base) of
                {ok, I2, Rest, Pos2} ->
                    I3 = I * math:pow(10, list_to_integer(lists:reverse(I2))),
                    I4 = if is_integer(I) -> trunc(I3); true -> I3 end,
                    {ok, I4, Rest, Pos2};
                R -> R
            end;
        <<$;, Rest/binary>> -> {ok, I, Rest, NuPos};
        _ -> {ok, I, Source, Pos, StartPos}
    end.
            
-spec maybe_float(string(), numberbase(), binary(), positions()) -> returntuple(number()).

maybe_float(Acc, Base, Source, Pos={{Line, Col}, StartPos}) ->
    case Source of
        <<$., Source2/binary>> ->
            case raw_read_int("", Source2, {{Line, Col+1}, StartPos}, Base) of
                {ok, I, Rest, Pos2} ->
                    A1=lists:reverse(Acc),
                    A2=lists:reverse(I),
                    {ok, list_to_float(A1 ++ [$.|A2]), Rest, Pos2};
                R -> R
            end;
        _ -> {ok, list_to_integer(lists:reverse(Acc)), Source, Pos}
    end.

-spec maybe_extend_num(string(), numberbase(), binary(), positions()) -> returntuple(number()).

maybe_extend_num(Acc, Base, Source, Pos) ->
    case maybe_float(Acc,Base,Source,Pos) of
        {ok, Acc2, Rest, Pos2} -> maybe_exp(Acc2, Base, Rest, Pos2);
        R -> R
    end.

-spec base36(integer()) -> integer().

%% not supporting capitals is a bit opinionated, but i'm not inclined to change it
base36(C) when ?BASE10(C) -> C - $0;
base36(C) when ?LOWER(C)  -> C + 10 - $a;
base36(_) -> 36. % never < 36

-spec is_base_n(integer(), integer()) -> boolean().

is_base_n(C, B) -> base36(C) < B.

%% atoms

-spec quoted_atom_char(non_neg_integer()) -> boolean().
-spec atom_char(non_neg_integer()) -> boolean().

quoted_atom_char(C) -> ?QSYM(C).
atom_char(C) -> ?ATOM(C).

-spec read_atom(binary(), positions()) -> returntuple(atom()).

read_atom(Source, Pos) ->
    case read_while("", Source, Pos, fun atom_char/1, atom_char) of
        {ok, S, Rest, Pos2} -> {ok, list_to_atom(S), Rest, Pos2};
        R -> R
    end.

-spec read_quoted_atom(binary(), positions()) -> returntuple(atom()).

read_quoted_atom(Source, Pos) ->
    case read_while("", Source, Pos, fun quoted_atom_char/1, quoted_atom_char) of
        {ok, S, Source2, Pos2={{Line, Col},StartPos}} ->
            case Source2 of
                <<>> -> {error, unexpected, eof, Pos2};
                <<C/utf8, Rest/binary>> ->
                    case C of
                        $| -> {ok, list_to_atom(S), Rest, {{Line, Col+1}, StartPos}};
                        _ -> {error, expected, $|, Pos2}
                    end
            end;
        R -> R
    end.

-spec read_while(list(), binary(), positions(), fun((integer()) -> boolean()), atom()) -> returntuple(list()).

read_while(Acc, Source, Pos={{Line, Col}, StartPos}, Fun, Name) ->
    case Source of
	<<>> -> finalize_while(Acc, Source, Pos, Name);
        <<C/utf8, Rest/binary>> ->
            case Fun(C) of
                true -> read_while([C|Acc], Rest,
                                   {case C of $\n -> {Line+1,0}; _ -> {Line, Col+1} end, StartPos},
                                   Fun, Name);
                false -> finalize_while(Acc, Source, Pos, Name)
            end
    end.

-spec finalize_while( list(), binary(), positions(), atom() ) -> returntuple(list()) .

finalize_while(Acc, Source, Pos, Name) ->
    case Acc of
        [] -> {error, expected, Name, Pos};
        _ -> {ok, lists:reverse(Acc), Source, Pos}
    end.

%% Whitespace, of which we have a special definition

-spec skip_whitespace(binary(), positions()) -> goodtuple(binary()).

skip_whitespace(Source, Pos={{Line, Col}, StartPos}) ->
    case Source of
        <<>> -> {ok, <<>>, <<>>, Pos};
        <<C/utf8, Rest/binary>> ->
            case C of
                $\n -> skip_whitespace(Rest, {{Line+1, 0}, StartPos});
                _ when ?SPACE(C) -> skip_whitespace(Rest, {{Line, Col+1}, StartPos});
                _ -> {ok, <<>>, Source, Pos}
            end
    end.
