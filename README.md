MQL
=======
An OTP library to translate a SQL-like query in a MongoDB query.
Based on idea by https://github.com/hachreak/mongoql


Examples
--------

E.g. 

```
SELECT name, owner WHERE temperature>10 and (color="green" or color="red") ORDER BY name ASC LIMIT 2,3
```

Translated in the follow map with  MongoDB query:

```erlang
#{batchsize => <<"3">>,
      projector => [{<<"name">>,1},{<<"owner">>,1}],
      query => {'$query',{{<<"temperature">>,{'$gt',10}},
                 {'$or',[{<<"color">>,{'$eq',<<"красный"/utf8>>}},
                         {<<"color">>,{'$eq',<<"red">>}}]}},
                '$orderby',
                [{<<"name">>,1}]},
      skip => <<"2">>}.
```


How to use
----------

```erlang
Collection = <<"list">>,
QueryString = "SELECT name, owner WHERE temperature>10 and (color=\"green\" or color=\"red\") ORDER BY name ASC LIMIT 2,3",
{ok, QMap} = mql:parse(QueryString),
Cursor=mongo:find(Collection, maps:get(query, QMap), maps:get(projector, QMap), maps:get(skip, QMap), maps:get(batchsize, QMap))
.
```
Placeholders
----------
Also you can use placeholders.
If placeholder value is atom, float or integer, it will be converted to string. 
If placeholder value is binary, it will be converted to string with leading and trailing double quotes.
If placeholder value is list, it will remains unchanged.

```erlang
Collection = <<"list">>,
QueryString = "SELECT name, owner WHERE temperature>:temp and (color=:color1 or color=:color2) ORDER BY name ASC LIMIT 2,3",
{ok, QMap} = mql:parse(QueryString, [{"temp", 25}, {"color1", "\"red\""}, {"color2", <<"green">>}]),
Cursor=mongo:find(Collection, maps:get(query, QMap), maps:get(projector, QMap), maps:get(skip, QMap), maps:get(batchsize, QMap))
.
```
