%% Author: kbmurali
%% Created: Mar 5, 2012
%% Description: TODO: Add description to entlr_grammar_parser
-module(entlr_grammar_parser).



%%
%% Exported Functions
%%
-export([get_lines/1]).
-export([get_entries/1]).

%%
%% API Functions
%%
get_entries( Filename ) ->
	[ Grammar_Header | R_Lines ] = get_lines( Filename ),
	case eval_grammar_header( Grammar_Header ) of
		{error, R} ->
			{error, R};
		{grammar, Grammar_Name} ->
			{Rules, Tokens} = get_rules_tokens( R_Lines, [], [] ),
			Resolved_Tokens = resolve_tokens( Tokens, [] ),
			{Grammar_Name, Rules, Resolved_Tokens}
	end.
	
	
get_lines( Filename ) ->
	{ok, Dev} = file:open(Filename, [read] ),
	Lines = read( Dev, 0, [], [], none, none, none ),
	file:close( Dev),
	Lines.

get_rules_tokens( [], R_List, T_List ) ->
	Rules = lists:reverse( R_List ),
	Tokens = lists:reverse( T_List ),
	{Rules, Tokens};
get_rules_tokens( [ Line | Rest ], R_List, T_List ) ->
	case eval_token( Line ) of
		{error, not_a_token} ->
			case eval_rule( Line ) of
				{error, not_a_rule} ->
					throw( {error, invalid_line, Line} );
				{error, invalid_rule} ->
					throw( {error, invalid_line, Line} );
				Rule ->
					get_rules_tokens( Rest, [ Rule | R_List], T_List )
			end;
		{error, invalid_token} ->
			throw( {error, invalid_token, Line} );
		Token ->
			get_rules_tokens( Rest, R_List, [ Token | T_List ] )
	end.

eval_grammar_header( Grammar_Header ) ->
	[_ | Name_Token ] = re:split( Grammar_Header, "[\\s]*grammar[\\s]*", [ {return, list}, trim ] ),
	case length(Name_Token) == 1 of
		true ->
			Name_Tokens = [ M || M <- re:split( Name_Token, "\\s+", [ {return, list} ] ), M =/= [] ],
			case length( Name_Tokens ) == 1 of
				true ->
					{ grammar, lists:nth(1, Name_Tokens) };
				false ->
					{error, invalid_grammar_header}
			end;
		false ->
			{error, invalid_grammar_header}
	end.

eval_token( Line ) ->
	Parts = split_entry( Line, [] ),
	case length( Parts ) == 2 of
		true ->
			[Name_Part, Value_Part] = Parts,
			Name_Tokens = [ M || M <- re:split( Name_Part, "\\s+", [ {return, list} ] ), M =/= [] ],
								  
			case length( Name_Tokens ) == 1 of
				true ->
					[Token_Name | _ ] = Name_Tokens,
					case is_token( Token_Name ) of
						true ->
							{Token_Name, Value_Part };
						false ->
							{error, not_a_token}
					end;
				false ->
					{error, invalid_token}
			end;
		false ->
			{error, invalid_token}
	end.

eval_rule( Line ) ->
	Parts = split_entry( Line, [] ),
	case length( Parts ) == 2 of
		true ->
			[Name_Part, Value_Part] = Parts,
			Name_Tokens = [ M || M <- re:split( Name_Part, "\\s+", [ {return, list} ] ), M =/= [] ],
								  
			case length( Name_Tokens ) == 1 of
				true ->
					[Rule_Name | _ ] = Name_Tokens,
					case is_rule( Rule_Name ) of
						true ->
							{Rule_Name, Value_Part };
						false ->
							{error, not_a_rule}
					end;
				false ->
					{error, invalid_rule}
			end;
		false ->
			{error, invalid_rule}
	end.

split_entry( [], Acc ) ->
	Part = lists:flatten( lists:reverse( Acc ) ),
	[Part];
split_entry( [ $: | Rest ], Acc ) ->
	Name_Part = lists:flatten( lists:reverse( Acc ) ),
	[ Name_Part, Rest ];
split_entry( [ C | Rest ], Acc ) ->
	split_entry( Rest, [C | Acc] ).
	
read( Dev, Cur_Location, Cur_Line, Acc, none, none, none ) ->
	case eval_read( Dev, Cur_Location ) of
		{ok, ";" } ->
			Line = lists:concat( lists:reverse( Cur_Line ) ),
			N_Acc = [Line | Acc ],
			read( Dev, Cur_Location + 1, [], N_Acc, none, none, none );
		{ok, C } ->
			read( Dev, Cur_Location + 1, Cur_Line, Acc, C, none, none );
		continue ->
			read( Dev, Cur_Location + 1, Cur_Line, Acc, none, none, none );
		eof ->
			Line = lists:concat( lists:reverse( Cur_Line ) ),
			N_Acc = [Line | Acc ],
			N_Lines = [ L || L <- N_Acc, L =/= [] ],
			lists:reverse( N_Lines )
	end;
read( Dev, Cur_Location, Cur_Line, Acc, "{", none, none ) ->
	New_Line = [ "{" | Cur_Line ],
	case inner_read( Dev, Cur_Location, New_Line, Acc, none, none, none ) of
		{ok, {Dev1, New_Location, N_Acc, C1, C2, C3 } } ->
			read( Dev1, New_Location, [], N_Acc, C1, C2, C3 );
		{eof, {_Dev1, _Location, N_Acc, _,_,_ } } ->
			N_Lines = [ L || L <- N_Acc, L =/= [] ],
			lists:reverse( N_Lines )
	end;
read( Dev, Cur_Location, Cur_Line, Acc, C1, none, none ) ->
	case eval_read( Dev, Cur_Location ) of
		{ok, C } ->
			read( Dev, Cur_Location + 1, Cur_Line, Acc, C1, C, none );
		continue ->
			read( Dev, Cur_Location + 1, Cur_Line, Acc, C1, none, none );
		eof ->
			New_Line = [ C1 | Cur_Line ],
			Line = lists:concat( lists:reverse( New_Line ) ),
			N_Acc = [Line | Acc ],
			N_Lines = [ L || L <- N_Acc, L =/= [] ],
			lists:reverse( N_Lines )
	end;
read( Dev, Cur_Location, Cur_Line, Acc, C1, C2, none ) ->
	case eval_read( Dev, Cur_Location ) of
		{ok, C } ->
			read( Dev, Cur_Location + 1, Cur_Line, Acc, C1, C2, C );
		continue ->
			read( Dev, Cur_Location + 1, Cur_Line, Acc, C1, C2, none );
		eof ->
			New_Line =[ C2 | [ C1 | Cur_Line ] ],
			Line = lists:concat( lists:reverse( New_Line ) ),
			N_Acc = [Line | Acc ],
			N_Lines = [ L || L <- N_Acc, L =/= [] ],
			lists:reverse( N_Lines )
	end;
read( Dev, Cur_Location, Cur_Line, Acc, ";", C2, C3 ) ->
	Line = lists:concat( lists:reverse( Cur_Line ) ),
	N_Acc = [Line | Acc ],
	read( Dev, Cur_Location, [], N_Acc, C2, C3, none );
read( Dev, Cur_Location, Cur_Line, Acc, "'", ";", "'" ) ->
	New_Line = [ "'" | [ ";" | [ "'" | Cur_Line ] ] ],
	read( Dev, Cur_Location, New_Line, Acc, none, none, none );
read( Dev, Cur_Location, Cur_Line, Acc, "'", ";", C3 ) ->
	Line = lists:concat( lists:reverse( [ "'" | Cur_Line ] ) ),
	N_Acc = [Line | Acc ],
	read( Dev, Cur_Location, [], N_Acc, C3, none, none );
read( Dev, Cur_Location, Cur_Line, Acc, C1, ";", C3 ) ->
	Line = lists:concat( lists:reverse( [ C1 | Cur_Line ] ) ),
	N_Acc = [Line | Acc ],
	read( Dev, Cur_Location, [], N_Acc, C3, none, none );
read( Dev, Cur_Location, Cur_Line, Acc, "'", "{", "'" ) ->
	New_Line = [ "'" | [ "{" | [ "'" | Cur_Line ] ] ],
	read( Dev, Cur_Location, New_Line, Acc, none, none, none );
read( Dev, Cur_Location, Cur_Line, Acc, "'", "{", C ) ->
	New_Line = [ C | [ "{" | [ "'" | Cur_Line ] ] ],
	case inner_read( Dev, Cur_Location, New_Line, Acc, none, none, none ) of
		{ok, {Dev1, New_Location, N_Acc, C1, C2, C3 } } ->
			read( Dev1, New_Location, [], N_Acc, C1, C2, C3 );
		{eof, {_Dev1, _Location, N_Acc, _,_,_ } } ->
			N_Lines = [ L || L <- N_Acc, L =/= [] ],
			lists:reverse( N_Lines )
	end;
read( Dev, Cur_Location, Cur_Line, Acc, C1, "'", "{" ) ->
	New_Line = [ C1 | Cur_Line ],
	read( Dev, Cur_Location, New_Line, Acc, "'", "{", none );
read( Dev, Cur_Location, Cur_Line, Acc, C1, C2, "{" ) ->
	New_Line = [ "{" | [ C2 | [ C1 | Cur_Line ] ] ],
	case inner_read( Dev, Cur_Location, New_Line, Acc, none, none, none ) of
		{ok, {Dev1, New_Location, N_Acc, NC1, NC2, NC3 } } ->
			read( Dev1, New_Location, [], N_Acc, NC1, NC2, NC3 );
		{eof, {_Dev1, _Location, N_Acc, _,_,_ } } ->
			N_Lines = [ L || L <- N_Acc, L =/= [] ],
			lists:reverse( N_Lines )
	end;
read( Dev, Cur_Location, Cur_Line, Acc, C1, C2, C3 ) ->
	New_Line = [ C1 | Cur_Line ],
	read( Dev, Cur_Location, New_Line, Acc, C2, C3, none ).


inner_read( Dev, Cur_Location, Cur_Line, Acc, none, none, none ) ->
	case eval_read( Dev, Cur_Location ) of
		{ok, "}" } ->
			Line = lists:concat( lists:reverse( [ "}" | Cur_Line ] ) ),
			N_Acc = [Line | Acc ],
			{ok, {Dev, Cur_Location, N_Acc, none, none, none } };
		{ok, C } ->
			inner_read( Dev, Cur_Location + 1, Cur_Line, Acc, C, none, none );
		continue ->
			inner_read( Dev, Cur_Location + 1, Cur_Line, Acc, none, none, none );
		eof ->
			Line = lists:concat( lists:reverse( Cur_Line ) ),
			N_Acc = [Line | Acc ],
			{eof, {Dev, Cur_Location, N_Acc, none, none, none } }
	end;
inner_read( Dev, Cur_Location, Cur_Line, Acc, C1, none, none ) ->
	case eval_read( Dev, Cur_Location ) of
		{ok, C } ->
			inner_read( Dev, Cur_Location + 1, Cur_Line, Acc, C1, C, none );
		continue ->
			inner_read( Dev, Cur_Location + 1, Cur_Line, Acc, C1, none, none );
		eof ->
			New_Line = [ C1 | Cur_Line ],
			Line = lists:concat( lists:reverse( New_Line ) ),
			N_Acc = [Line | Acc ],
			{eof, {Dev, Cur_Location, N_Acc, none, none, none } }
	end;
inner_read( Dev, Cur_Location, Cur_Line, Acc, C1, C2, none ) ->
	case eval_read( Dev, Cur_Location ) of
		{ok, C } ->
			inner_read( Dev, Cur_Location + 1, Cur_Line, Acc, C1, C2, C );
		continue ->
			inner_read( Dev, Cur_Location + 1, Cur_Line, Acc, C1, C2, none );
		eof ->
			New_Line =[ C2 | [ C1 | Cur_Line ] ],
			Line = lists:concat( lists:reverse( New_Line ) ),
			N_Acc = [Line | Acc ],
			{eof, {Dev, Cur_Location, N_Acc, none, none, none } }
	end;
inner_read( Dev, Cur_Location, Cur_Line, Acc, "}", C2, C3 ) ->
	Line = lists:concat( lists:reverse( [ "}" | Cur_Line ] ) ),
	N_Acc = [Line | Acc ],
	{ok, {Dev, Cur_Location, N_Acc, C2, C3, none } };
inner_read( Dev, Cur_Location, Cur_Line, Acc, "'", "}", "'" ) ->
	New_Line = [ "'" | [ "}" | [ "'" | Cur_Line ] ] ],
	inner_read( Dev, Cur_Location, New_Line, Acc, none, none, none );
inner_read( Dev, Cur_Location, Cur_Line, Acc, "'", "}", C3 ) ->
	Line = lists:concat( lists:reverse( [ "}" | [ "'" | Cur_Line ] ] ) ),
	N_Acc = [Line | Acc ],
	{ok, {Dev, Cur_Location, N_Acc, C3, none, none } };
inner_read( Dev, Cur_Location, Cur_Line, Acc, C1, "}", C3 ) ->
	Line = lists:concat( lists:reverse( [ "}" | [ C1 | Cur_Line ] ] ) ),
	N_Acc = [Line | Acc ],
	{ok, {Dev, Cur_Location, N_Acc, C3, none, none } };
inner_read( Dev, Cur_Location, Cur_Line, Acc, C1, C2, C3 ) ->
	New_Line = [ C1 | Cur_Line ],
	inner_read( Dev, Cur_Location, New_Line, Acc, C2, C3, none ).

eval_read( Dev, Cur_Location ) ->
	case file:pread( Dev, Cur_Location, 1 ) of
		eof ->
			eof;
		{ok, "\r" } ->
			continue;
		{ok, "\n" } ->
			continue;
		{ok, "\t" } ->
			continue;
		{ok, C } ->
			{ok, C }
	end.

resolve_tokens( [], Resolved_Tokens ) ->
	lists:reverse( Resolved_Tokens );
resolve_tokens( [ {Name, Value} | Unresolved_Tokens], Resolved_Tokens ) ->
	case get_refs( Value ) of
		[] ->
			N_Resolved_Tokens = [ {Name, Value} | Resolved_Tokens ],
			resolve_tokens( Unresolved_Tokens, N_Resolved_Tokens );
		Refs ->
			case resolve_token( Refs, Value, Unresolved_Tokens, Resolved_Tokens ) of
				{error, ref_not_resolved } ->
					N_Tokens = lists:append( Unresolved_Tokens, [ {Name, Value} ] ),
					resolve_tokens( N_Tokens, Resolved_Tokens );
				{ok, N_Value} ->
					N_Resolved_Tokens = [ {Name, N_Value} | Resolved_Tokens ],
					resolve_tokens( Unresolved_Tokens, N_Resolved_Tokens )
			end
	end.

resolve_token( [], N_Value, _Unresolved_Token, _ ) ->
	{ok, N_Value};
resolve_token( [Ref | Rest], Value, Unresolved_Tokens, Resolved_Tokens ) ->
	case is_token( Ref ) of
		false ->
			resolve_token( Rest, Value, Unresolved_Tokens, Resolved_Tokens );
		true ->
			case lists:keyfind( Ref, 1, Resolved_Tokens ) of
				false ->
					case lists:keyfind( Ref, 1, Unresolved_Tokens ) of
						false ->
							resolve_token( Rest, Value, Unresolved_Tokens, Resolved_Tokens );
						_ ->
							{error, ref_not_resolved}
					end;
				{_, T_Value } ->
					N_Value = re:replace( Value, Ref, T_Value, [ {return, list} ] ),
					resolve_token( Rest, N_Value, Unresolved_Tokens, Resolved_Tokens )
			end
	end.

get_refs( Value ) ->
	W_List = re:split( Value, "[^A-Z^a-z]+", [{return, list}, unicode] ),
	[ R || R <- W_List, R =/= [] ].

is_token( Token_Name ) ->
	C = lists:nth(1, Token_Name),
	C >= $A andalso C =< $Z.

is_rule( Rule_Name ) ->
	C = lists:nth(1, Rule_Name),
	C >= $a andalso C =< $z.
