%%% @author Ilya A.Shlyakhovoy <ilya_cat@mail.ru>
%%% @copyright (C) 2016 Ilya A.Shlyakhovoy
%%%
%%% This software is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This software is distributed in the hope that it will be useful, but
%%% WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this software; if not, write to the Free Software Foundation,
%%% Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
%%%
%%% @doc MQL API
%%% @end

-module('mql').

%% API exports
-export([parse/1, parse/2]).

-export_type([query/0]).

%%%_ * Types -----------------------------------------------------------

-type query() :: list() | binary().
-type placeholders() :: list().

%%====================================================================
%% API functions
%%====================================================================

-spec parse(query()) -> {ok, list()} | {error, unknow_input}.
parse(Query) when is_binary(Query) ->
  parse(binary_to_list(Query));
parse(Query) when is_list(Query) ->
  {ok, Tokens, _} = mql_lexer:string(Query),
  mql_parser:parse(Tokens);
parse(_Query) ->
  {error, unknow_input}.

-spec parse(query(), placeholders()) -> {ok, list()} | {error, unknow_input}.
parse(Query, Placeholders) when is_binary(Query) ->
  parse(binary_to_list(Query), Placeholders);
parse(Query, Placeholders) when is_list(Query) ->
	NewQuery=lists:foldl(fun({Key, Value}, A) -> 
		{ok, MP}=re:compile(":"++Key, [unicode, caseless]),
		re:replace(A,MP, prepare(Value), [{return,list},global])
	end, Query, Placeholders),
	parse(NewQuery)
.
%%====================================================================
%% Internal functions
%%====================================================================


prepare(Val) when is_integer(Val) -> integer_to_list(Val);
prepare(Val) when is_binary(Val) -> [$"] ++ bitstring_to_list(Val) ++[$"];
prepare(Val) when is_float(Val) -> float_to_list(Val);
prepare(Val) when is_list(Val) -> Val;
prepare(Val) when is_atom(Val) -> [$"] ++atom_to_list(Val)++[$"].
