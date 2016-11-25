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
%%% @doc Parser
%%% @end

Terminals
'[' ']' ','  var integer string set atom union intersection comparator projection_op timestamp float group_start group_end limit asc desc filter_op order_op projection_end filter_end order_end .

Nonterminals
   order orders orderby query projection list group  element elements where vars conditions .
   
%%% orderby orders
Rootsymbol query.

Left 300 comparator.
Left 100 intersection.
Left 100 union.
Right 400 group.

query -> projection where orderby limit	: query('$1', '$2', '$3', unwrap('$4')).
query -> projection where orderby		: query('$1', '$2', '$3', [0,0]).
query -> projection where limit			: query('$1', '$2', {}, unwrap('$3')).
query -> projection orderby limit		: query('$1', {}, '$2', unwrap('$3')).
query -> where orderby limit				: query([], '$1', '$2', unwrap('$3')).
query -> projection where					: query('$1', '$2', {}, [0,0]).
query -> projection orders				: query('$1', {},'$2', [0,0]).
query -> projection limit				: query('$1', {}, {}, unwrap('$2')).
query -> where orderby 					: query([], '$1', '$2', [0,0]).
query -> where limit 					: query([], '$1', {}, unwrap('$2')).
query -> orderby limit 					: query([], {}, '$1', unwrap('$2')).
query -> projection 						: query('$1', {}, {}, [0,0]).
query -> where 								: query([], '$1', {}, [0,0]).
query -> orderby 						: query([], {}, '$1', [0,0]).
query -> limit 							: query([], {}, {}, unwrap('$1')).


vars -> var : ['$1'].
vars -> var ',' vars :  ['$1'] ++ '$3'.

projection -> projection_op vars projection_end : lists:zip(lists:map(fun(X) -> unwrap(X) end, '$2'), lists:duplicate(length('$2'), 1)).
projection -> projection_op projection_end : [].

where -> filter_op filter_end : {}.
where -> filter_op conditions filter_end : unwrap('$2').

conditions -> conditions intersection conditions : {conditions, {unwrap('$1'), unwrap('$3')}}.
conditions -> element intersection conditions : {conditions, {unwrap('$1'), unwrap('$3')}}.
conditions -> conditions intersection element : {conditions, {unwrap('$1'), unwrap('$3')}}.
conditions -> element intersection element : {conditions, {unwrap('$1'), unwrap('$3')}}.
conditions -> conditions union conditions : {conditions, {'$or', [unwrap('$1'), unwrap('$3')]}}.
conditions -> element union conditions : {conditions, {'$or', [unwrap('$1'), unwrap('$3')]}}.
conditions -> conditions union element : {conditions, {'$or', [unwrap('$1'), unwrap('$3')]}}.
conditions -> element union element : {conditions, {'$or', [unwrap('$1'), unwrap('$3')]}}.
conditions -> element comparator element :  {conditions, { unwrap('$1'), {unwrap('$2'), unwrap('$3')}}}.

conditions -> var set list :  {conditions, {'$1', { '$in', unwrap('$3')}}}.
conditions -> conditions comparator conditions : {conditions, { unwrap('$1'), {unwrap('$2'), '$3'}}}.


list -> '[' ']' :  nil.
list -> '[' elements ']' :  {list,'$2'}.

group -> group_start group_end : nil.
group -> group_start conditions group_end : {group, unwrap('$2')}.





orderby -> order_op order_end : [].
orderby -> order_op orders order_end : parseorders('$2').

orders -> order : ['$1'].
orders -> order ',' orders :  ['$1'] ++ '$3'.
order -> asc : '$1'.
order -> desc : '$1'.



elements -> element : ['$1'].
elements -> element ',' elements :  ['$1'| '$3'].

element -> var : '$1'.
element -> group : '$1'.

element -> atom : '$1'.
element -> integer : unwrap('$1').
element -> string : unwrap('$1').
element -> timestamp : '$1'.
element -> float : unwrap('$1').

Erlang code.

query(Projections, Filters, Orders, [From, Count]) -> 
	#{query=>
		{'$query', Filters, 
		'$orderby', Orders}
		, 
	  projector=>Projections, 
	skip=>From, 
	batchsize=>Count}.

unwrap({_,V})   -> V;
unwrap({_,_,V}) -> V;
unwrap(V) -> V.



parseorders([]) -> [];
parseorders([Head|Tail]) ->
	PH=case Head of
		{asc, _, Name} -> {Name, 1};
		{desc, _, Name2} -> {Name2, -1}
	end,
	[PH|parseorders(Tail)]
.
		
		
		
