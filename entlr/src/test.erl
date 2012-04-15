%% Author: kbmurali
%% Created: Mar 16, 2012
%% Description: TODO: Add description to test
-module(test).

-record( item, {id, name, manf_date} ).
%%
%% Include files
%%

%%
%% Exported Functions
%%
-compile( [export_all] ).

%%
%% API Functions
%%
rule( _, #item{} = Item1, #item{} = Item2 ) when Item1#item.id == Item2#item.id ->
	io:format( "This is test rule" ).

get( String, Props ) ->
	Tokens = to_prop_tokens(String),
	Props_List = deep_flatten( Props ),
	get_value( Tokens, Props_List ).

get_value( _, [] ) ->
	{error, not_found };
get_value( [], [Value] ) ->
	{ok, Value};
get_value( [Token | Rest ], [Token | Props ] ) ->
	get_value( Rest, Props );
get_value( Tokens, [ Prop | Rest_Props ] ) ->
	case get_value( Tokens, Prop ) of
		{ok, Value } ->
			{ok, Value};
		{error, _ } ->
			get_value( Tokens, Rest_Props )
	end;
get_value( _, _ ) ->
	{error, not_found }.

to_prop_tokens( String ) ->
	Splits = re:split( String, "\\.", [ {return, list} ] ),
	[ list_to_atom( Token) || Token <- Splits, Token =/= [] ].

deep_flatten( List ) when is_list( List ) ->
	[ deep_flatten( E ) || E <- List ];
deep_flatten( Tuple ) when is_tuple( Tuple ) ->
	[ deep_flatten( E ) || E <- tuple_to_list( Tuple) ];
deep_flatten( Other ) ->
	Other.