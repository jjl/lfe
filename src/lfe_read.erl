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

read(<<>>,_) -> {error,unexpected,eof};
read(Source,Pos) ->
    {Source2,Pos2} = skip_whitespace(Source,Pos),
    <<C, Rest/binary>> = Source2,
    case C of
        $; -> read_comment(Source2,Pos2);
        $| -> read_quoted_symbol(Source2,Pos2);
        $( -> read_list(Source2,Pos2);
        $[ -> read_tuple(Source2,Pos2);
        ${ -> read_map(Source2,Pos2);
        $) -> {error,unexpected,$),Pos2,Rest};
        $] -> {error,unexpected,$],Pos2,Rest};
        $} -> {error,unexpected,$},Pos2,Rest};
        $\ -> read_char(Source2,Pos2);
        $" -> read_string(Source2,Pos2);
        $# -> read_special(Source2,Pos2);
        _  -> read_other(Source2,Pos2)
     end.

%%%%%%%% private

%% comments

read_comment(Source, Pos={Line,Col}) ->
    case Source of
        <<";", Rest/binary>> -> read_comment([], Rest, {Line,Col+1}, Pos);
        _ -> {error,expected,$;,Pos}
    end.

read_comment(Acc, Source, Pos={Line,Col}, StartPos) ->
    case Source of
        <<>> -> {ok,finalize_comment(Acc), <<>>, Pos,StartPos};
        <<$\n, Rest/binary>> -> {ok,finalize_comment(Acc), Rest, {Line+1,0},StartPos};
        <<C, Rest/binary>> -> read_comment([<<C>>|Acc], Rest, {Line,Col+1}, StartPos)
    end.

finalize_comment(C) -> [comment,erlang:iolist_to_binary(lists:reverse(C))].

read_list(Source,Pos={Line,Col}) ->
    case Source of
        <<$(, Rest/binary>> -> read_list([],Rest,{Line,Col+1},Pos);
        _ -> {error,expected,$(,Pos}
    end.

read_list(Acc,Source,Pos,StartPos) ->
    case read(Source,Pos) of
        {error,unexpected,$),{Line,Col},Rest} -> {ok,finalize_list(Acc),Rest,{Line,Col+1},StartPos};
        {ok, T, Rest, {Line,Col}, _}  -> read_list([T|Acc],Rest,{Line,Col},StartPos)
    end.

finalize_list(L) -> lists:reverse(L).

%% tuples

read_tuple(Source,Pos={Line,Col}) ->
    case Source of
        <<>> -> {error,unexpected,eof};
        <<$[, Rest/binary>> -> read_tuple([],Rest,{Line,Col+1},Pos);
        _ -> {error,expected,$[,Source,[{line,Line},{col,Col}]}
    end.

read_tuple(Acc,Source,Pos={Line,Col},StartPos) ->
    case read(Source,Pos) of
        {error,unexpected,$],{Line,Col},Rest} -> {ok,finalize_tuple(Acc),Rest,{Line,Col+1},StartPos};
        {ok, T, Rest, {Line,Col}, _}  -> read_tuple([T|Acc],Rest,{Line,Col},StartPos)
    end.

finalize_tuple(T) -> list_to_tuple(lists:reverse(T)).

%% maps

read_map(Source,Pos={Line,Col}) ->
    case Source of
        <<>> -> {error,unexpected,eof};
        <<${, Rest/binary>> -> read_map([],Rest,{Line,Col+1},Pos);
        _ -> {error,expected,$[,Source,[{line,Line},{col,Col}]}
    end.

read_map(Acc,Source,Pos={Line,Col},StartPos) ->
    case read(Source,Pos) of
        {error,unexpected,$},{Line,Col},Rest} -> {ok,finalize_map(Acc),Rest,{Line,Col+1},StartPos};
        {ok, T, Source2, Pos2, _}  ->
            {ok, T2, Rest, Pos3, _} = read(Source2, Pos2),
            read_map([{T,T2}|Acc],Rest,Pos3,StartPos)
    end.

finalize_map(M) -> maps:from_list(M).

%% characters and strings

%% TODO: fixme
read_char(Source,Pos={Line,Col}) ->
    case Source of
        <<>> -> {error, unexpected, eof};
        <<"\\\\",  Rest/binary>> -> {ok, <<"\\">>, Rest, {Line,Col+2},Pos};
        %% <<"\\x",   Rest/binary>> -> ...
        %% <<"\\u",   Rest/binary>> -> ...
        <<"\\", C, Rest/binary>> -> {ok, <<C>>,    Rest, {Line,Col+2},Pos};
        _ -> {error, expected, $\\, Source, [{position, Pos}]}
    end.

read_string(Source, Pos={Line,Col}) ->
    case Source of
        <<>> -> {error,unexpected,eof};
        <<"\"\"\"", Rest/binary>> -> read_string(<<>>, Rest, {Line,Col+3}, Pos, true);
        <<"\"\"",   Rest/binary>> -> {ok,"",Rest,{Line,Col+2},Pos};
        <<"\"",     Rest/binary>> -> read_string(<<>>, Rest, {Line,Col+1}, Pos, false);
        _ -> {error,expected,$",Source,[{position,Pos}]}
    end.

read_string(Acc,Source,Pos={Line,Col},StartPos,true) ->
    case Source of
        <<>>     -> {error,unexpected,eof,[{started,StartPos}]};
        <<"\"\"\"", Rest/binary>> -> {ok,finalize_string(Acc),Rest,{Line,Col+3},StartPos};
        <<"\\", Rest/binary>> -> 
            {ok, T, Rest, Pos2, _} = read_string_esc(Source, Pos),
            read_string([T|Acc], Rest, Pos2, StartPos, false);
        <<C, Rest/binary>> -> read_string([<<C>>|Acc], Rest,{Line,Col+1},StartPos,false)
    end;
read_string(Acc, Source, Pos={Line,Col}, StartPos, false) ->
    case Source of
        <<>>     -> {error,unexpected,eof,[{started,StartPos}]};
        <<"\"", Rest/binary>> -> {ok,finalize_string(Acc),Rest,{Line,Col+1},StartPos};
        <<"\\", Rest/binary>> -> 
            {ok, T, Rest, Pos2, _} = read_string_esc(Source, Pos),
            read_string([T|Acc], Rest, Pos2, StartPos, false);
        <<C, Rest/binary>> -> read_string([<<C>>|Acc], Rest,{Line,Col+1},StartPos,false)
    end.
read_string_esc(Source, Pos={Line,Col}) ->
    case Source of
        <<"\\b",   Rest/binary>> -> {ok, $\b, Rest, {Line, Col+2}};
        <<"\\t",   Rest/binary>> -> {ok, $\t, Rest, {Line, Col+2}};
        <<"\\n",   Rest/binary>> -> {ok, $\n, Rest, {Line, Col+2}};
        <<"\\v",   Rest/binary>> -> {ok, $\v, Rest, {Line, Col+2}};
        <<"\\f",   Rest/binary>> -> {ok, $\f, Rest, {Line, Col+2}};
        <<"\\r",   Rest/binary>> -> {ok, $\r, Rest, {Line, Col+2}};
        <<"\\e",   Rest/binary>> -> {ok, $\e, Rest, {Line, Col+2}};
        <<"\\s",   Rest/binary>> -> {ok, $\s, Rest, {Line, Col+2}};
        <<"\\d",   Rest/binary>> -> {ok, $\d, Rest, {Line, Col+2}};
        <<"\\\\",  Rest/binary>> -> {ok, $\\, Rest, {Line, Col+2}};
        <<"\\x",   _/binary>> -> read_hex_esc(Source,Pos);
        <<"\\u",   _/binary>> -> read_uni_esc(Source,Pos);
        <<"\\", C, Rest/binary>> -> {ok, <<C>>, Rest, {Line,Col+2}};
        _ -> {error, expected, $\\, [{position,Pos}]}
    end.
             
finalize_string(S) -> erlang:iolist_to_binary(lists:reverse(S)).

%% TODO: fixme
read_hex_esc(Source, Pos={Line,Col}) ->
    {error,not_implemented}.

%% TODO: fixme
read_uni_esc(Source, Pos={Line,Col}) ->
    {error,not_implemented}.

%% Specials

% NOTE: We haven't setled on the final assignments of these

%% TODO: fixme
% what specifically is different about this input mode? Are all backslashes disabled?
read_regex(Source, Pos={Line,Col}) ->
    {error, not_implemented}.

%% TODO: fixme
read_liststring(Source, Pos={Line,Col}) ->
    {error, not_implemented}.

%% TODO: fixme
% TODO: define short function syntax
read_shortfun(Source, Pos={Line,Col}) ->
    {error, not_implemented}.

%% TODO: FIXME
read_special(Source, Pos={Line,Col}) ->
    case Source of
        <<"#\"", _/binary>> -> read_regex(Source,Pos);
        <<"#{",  _/binary>> -> read_liststring(Source,Pos);
        <<"#(",  _/binary>> -> read_shortfun(Source,Pos)
    end.

%% TODO: fixme
read_decimal(Source, Pos={Line,Col}) ->
    {error, not_implemented}.

%% TODO: fixme
read_radix(Source, Pos={Line,Col}, StartPos) ->
    %% case Source of
    %%     <<>> -> {error,unexpected,eof};
    %%     <<A,B,$:, Rest/binary>> -> ...;
    %%     <<A,$:,   Rest/binary>> -> ...;
    %%     _ -> {error,expected,radix,Pos}
    %% end.
    {error, not_implemented}.

%% TODO: fixme
read_num(Source, Pos={Line,Col}) ->
    case Source of
        <<>> -> {error,unexpected,eof};
        <<"0x", Rest/binary>> -> read_num(Rest, {Line,Col+2}, Pos, 16);
        <<"0o", Rest/binary>> -> read_num(Rest, {Line,Col+2}, Pos, 8);
        <<"0b", Rest/binary>> -> read_num(Rest, {Line,Col+2}, Pos, 2);
        <<"0r", Rest/binary>> -> read_radix(Rest, {Line,Col+2}, Pos);
        _ -> {error, unimplemented, numeric_parsing}
        end.

%% TODO: FIXME
read_num(Source, Pos={Line,Col}, StartPos, Base) ->
    {error,not_implemented}.

%% TODO: fixme
% we need to clarify precisely what should be legal
read_bare_symbol(Source, Pos={Line,Col}) ->
    {error,not_implemented}.

%% TODO: fixme
% should we allow these to go multiline? in which case how about escape chars?
read_quoted_symbol(Source, Pos={Line,Col}) ->
    {error,not_implemented}.
%% TODO: fixme
    

%% TODO: fixme
% try parse number, fall back to read_bare_symbol
read_other(Source,Pos={Line,Col}) ->
    {error,not_implemented}.
 
%% Whitespace, of which we have a special definition

is_ws($\t) -> true;
is_ws($\v) -> true;
is_ws($\f) -> true;
is_ws($\r) -> true;
is_ws(_) -> false.

skip_whitespace(Source,Pos) ->
    skip_whitespace(Source,Pos,Pos).

skip_whitespace(<<>>,Pos,StartPos) -> {ok,<<>>,<<>>,Pos,StartPos};
skip_whitespace(Source,Pos={Line,Col},StartPos) ->
    <<C, Rest/binary>> = Source,
    case C of
        $\n -> skip_whitespace(Rest,{Line+1,0},StartPos);
        _   -> case is_ws(C) of
                   true  -> skip_whitespace(Rest,{Line,Col+1},StartPos);
                   false -> {ok,<<>>,Source,{Pos},StartPos}
             end
    end.
