%%% @author Ilya A.Shlyakhovoy <ilya_cat@mail.ru>
%%% @copyright (C) 2016 Ilya A.Shlyakhovoy
%%%
%%% This software is free software'; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation'; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This software is distributed in the hope that it will be useful, but
%%% WITHOUT ANY WARRANTY'; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this software'; if not, write to the Free Software Foundation,
%%% Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
%%%
%%% @doc Lexer
%%% @end

Definitions.

D   = \-?[0-9]
F	= \-?[0-9]+\.[0-9]+
L   = [a-zA-Z\x{100}-\x{FFFF}_\.]
WS  = \s+
C   = (<|<=|=|=>|>|!=|<>|><|=<|>=)
ENDPROJECTION = \ep
ENDFILTER = \ef
ENDORDER = \eo
TIME=[0-2][0-9]:[0-5][0-9]:[0-5][0-9]
DATE=[0-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9]
PROJ = (select|SELECT).*\s(where|WHERE)
FILTERO = \f(where|WHERE).*\s(ORDER|order)\s+(by|BY)
FILTERL = \v(where|WHERE).*\s(LIMIT|limit)
FILTER = \b(where|WHERE).*
PREFILTER = (where|WHERE).*[^((ORDER|order)\s+(by|BY)|(LIMIT|limit))]
ORDER = (ORDER|order)\s+(by|BY)\s+(.+\s+(ASC|asc|DESC|desc))+
ASC = [a-zA-Z\x{100}-\x{FFFF}_\.]+\s+(ASC|asc)
DESC = [a-zA-Z\x{100}-\x{FFFF}_\.]+\s+(DESC|desc)
LIMIT = (limit|LIMIT)\s+([0-9]+\s*,\s*[0-9]+|[0-9]+)

Rules.

{PROJ} : {token, {projection_op, TokenLine}, string:strip(string:sub_string(TokenChars, 7, TokenLen-5)) ++ " \ep WHERE "}.
{LIMIT} : {token, {limit, TokenLine, re:split(string:strip(string:substr(TokenChars,6)), "\s*,\s*",[trim])}}.
{FILTER} : {token, {filter_op, TokenLine}, string:strip(string:substr(TokenChars,7)) ++ " \ef "}.
{FILTERO} : {token, {filter_op, TokenLine}, string:strip(string:sub_string(TokenChars, 7, TokenLen-8)) ++ " \ef ORDER BY "}.
{FILTERL} : {token, {filter_op, TokenLine}, string:strip(string:sub_string(TokenChars, 7, TokenLen-5)) ++ " \ef LIMIT "}.
{ORDER} : {token, {order_op, TokenLine}, string:strip(string:substr(TokenChars,9)) ++ " \eo "}.
{ASC} : {token, {asc, TokenLine, unicode:characters_to_binary(string:strip(string:sub_string(TokenChars,1, TokenLen-3)))}}.
{DESC} : {token, {desc, TokenLine , unicode:characters_to_binary(string:strip(string:sub_string(TokenChars,1, TokenLen-4)))}}.
(in|IN)     : {token,{set,TokenLine}}.
(or|OR)     : {token,{union,TokenLine}}.
(and|AND)    : {token,{intersection,TokenLine}}.
{C}    : {token,{comparator,TokenLine,comparator(TokenChars)}}.
"{L}+" : S = unicode:characters_to_binary(strip(TokenChars,TokenLen)), {token,{string,TokenLine,S}}.
{L}+   : {token,{var,TokenLine,unicode:characters_to_binary(TokenChars)}}.
{D}+   : {token,{integer,TokenLine,list_to_integer(TokenChars)}}.
{F}   : {token,{float,TokenLine,list_to_float(TokenChars)}}.
[,]  : {token,{list_to_atom(TokenChars),TokenLine}}.
\(	: {token,{group_start,TokenLine}}.
\)	: {token,{group_end,TokenLine}}.
\[	: {token,{'[',TokenLine}}.
\]	: {token,{']',TokenLine}}.
{DATE}T{TIME}Z : {token, {timestamp, TokenLine, iso8601totimestamp(TokenChars)}}.
{WS}+  : skip_token.
{PREFILTER} : {skip_token, prefilter(TokenChars)}.
{ENDPROJECTION} : {token, {projection_end, TokenLine}}.
{ENDFILTER} : {token, {filter_end, TokenLine}}.
{ENDORDER} : {token, {order_end, TokenLine}}.
\* 	: skip_token.

Erlang code.

strip(TokenChars,TokenLen) -> 
    lists:sublist(TokenChars, 2, TokenLen - 2).

comparator("<") -> '$lt';
comparator("<=") -> '$lte';
comparator("=<") -> '$lte';
comparator("=") -> '$eq';
comparator(">=") -> '$gte';
comparator("=>") -> '$gte';
comparator(">") -> '$gt';
comparator("!=") -> '$ne';
comparator("<>") -> '$ne';
comparator("><") -> '$ne'.

prefilter(Chars) ->
	{ok, MP}=re:compile("(order\s+by\s+(.+\s+(asc|desc))+|limit\s+([0-9]+\s*,\s*[0-9]+|[0-9]+))", [unicode, caseless]),
	case re:run(Chars, MP) of
		nomatch -> "\b"++Chars;
		_ -> {ok, MR}=re:compile("order\s+by\s+(.+\s+(asc|desc))+", [unicode, caseless]),
			case re:run(Chars, MR) of
			nomatch -> "\v"++Chars;
			_ -> "\f"++Chars
		end
	end
.


iso8601totimestamp(DateTimeString) ->
  DateTime = lists:map(fun(V) -> list_to_integer(V) end,
                       string:tokens(DateTimeString, ":T-Z")),
  datetime2timestamp(DateTime).

datetime2timestamp([Y, M, D, H, Min, S]) ->
  Seconds = calendar:datetime_to_gregorian_seconds(
              {{Y, M, D}, {H, Min, S}}) - 62167219200,
{Seconds div 1000000, Seconds rem 1000000, 0}.
