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
%%% @doc MQL API - Tests
%%% @end

-module(mql_tests).

-author('Ilya A.Shlyakhovoy <ilya_cat@mail.ru>').

-include_lib("eunit/include/eunit.hrl").

mql_test_() ->
  {setup,
    fun start/0,
    fun stop/1,
    fun (SetupData) ->
        [
         query(SetupData)
        ]
    end
  }.

start() ->
	ok.

stop(_SetupData) ->
	ok.

test_query(Query, Expect) ->
	{ok, Result}= mql:parse(Query),
	?assertEqual(Expect, Result).
	
test_query(Query, Params, Expect) ->
	{ok, Result}= mql:parse(Query, Params),
	?assertEqual(Expect, Result).

query(_) ->
  fun() ->
  
      test_query(
         "SELECT name, owner WHERE temperature>10 and (color=\"green\" or color=\"red\") ORDER BY name ASC LIMIT 2,3",
		#{batchsize => <<"3">>,
      projector => [{<<"name">>,1},{<<"owner">>,1}],
      query => {'$query',{{<<"temperature">>,{'$gt',10}},
                 {'$or',[{<<"color">>,{'$eq',<<"green">>}},
                         {<<"color">>,{'$eq',<<"red">>}}]}},
                '$orderby',
                [{<<"name">>,1}]},
      skip => <<"2">>}),
      
      test_query(
         "SELECT name, owner WHERE temperature>:temp and (color=:color1 or color=:color2) ORDER BY name ASC LIMIT 2,3",
         [{"temp", 25}, {"color1", "\"red\""}, {"color2", <<"green">>}],
		#{batchsize => <<"3">>,
      projector => [{<<"name">>,1},{<<"owner">>,1}],
      query => {'$query',{{<<"temperature">>,{'$gt',25}},
                 {'$or',[{<<"color">>,{'$eq',<<"red">>}},
                         {<<"color">>,{'$eq',<<"green">>}}]}},
                '$orderby',
                [{<<"name">>,1}]},
      skip => <<"2">>}),

      test_query("SELECT * WHERE a > 2016-01-15T18:19:28Z",
                 #{batchsize => 0,
      projector => [],
      query => {'$query',{<<"a">>,{'$gt',{1452,881968,0}}},'$orderby',{}},
      skip => 0})
  end.
