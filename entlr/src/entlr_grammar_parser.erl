%% Author: kbmurali
%% Created: Mar 5, 2012
%% Description: TODO: Add description to entlr_grammar_parser
-module(entlr_grammar_parser).

%%
%% Exported Functions
%%
-export([get_entries/2]).
-export([parse_rule/4]).
-export([resolve_rule_tokens/3]).

%%
%% API Functions
%%
get_entries( file, Filename ) ->
	{ok, Dev} = file:open(Filename, [read] ),
	Entries = get_entries( iodevice, Dev ),
	file:close( Dev),
	Entries;
get_entries( iodevice, Dev ) ->
	[ Grammar_Header | R_Lines ] = read( Dev, 0, [], [], none, none, none ),
	case eval_grammar_header( Grammar_Header ) of
		{error, R} ->
			{error, R};
		{grammar, Grammar_Name} ->
			{Rules, Tokens} = get_rules_tokens( R_Lines, [], [] ),
			Resolved_Tokens = resolve_tokens( Tokens, [] ),
			Token_RE_List = convert_to_re(Resolved_Tokens, false, [] ),
			%Resolved_Rules = resolve_rules( Rules, Resolved_Tokens, [] ),
			%Rule_RE_List = convert_to_re(Resolved_Rules, true, [] ),
			{Grammar_Name, Rules, Token_RE_List}
	end.
	
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

parse_rule( [], Cur_Ref, Type, Refs ) ->
	I_Refs = case Cur_Ref of
				 [] ->
					 Refs;
				 _ ->
					 Index = length(Refs) + 1,
					 Ref = lists:flatten( lists:reverse( Cur_Ref ) ),
					 [ {Index, Type, Ref} | Refs]
			 end,
	F_Refs = resolve_var_tokens( I_Refs, [] ),
	{ok, F_Refs};
parse_rule( [C | Rest ], Cur_Ref, Type, Refs ) when C == 9 orelse C == 32 -> 
	case Cur_Ref of
		[] ->
			parse_rule( Rest, [], unknown, Refs );
		_ ->
			Index = length(Refs) + 1,
			Ref = lists:flatten( lists:reverse( Cur_Ref ) ),
			parse_rule( Rest, [], unknown, [ {Index, Type,Ref} | Refs] )
	end;
parse_rule( [ 123 | Rest ], Cur_Ref, Type, Refs ) ->
	%This is the case for {. Ignore until } is found
	%123 is the asci value for '{'
	%125 is the asci value for '}'
	N_Refs = case Cur_Ref of
				 [] ->
					 Refs;
				 _ ->
					 Index = length(Refs) + 1,
					 Ref = lists:flatten( lists:reverse( Cur_Ref ) ),
					 [{Index, Type, Ref} | Refs]
			 end,
	case parse_until( Rest, 125, [] ) of
		{true, Code, [] } ->
			Index2 = length(N_Refs) + 1,
			F_Refs = [ {Index2, code, Code} | N_Refs ],
			parse_rule( [], [], unknown, F_Refs );
		{_, _, _ } ->
			throw( {error, invalid_specification} )
	end;
parse_rule( [40 | Rest], Cur_Ref, Type, Refs ) ->
	%32 is the asci value for " "
	%40 is the asci value for (
	N_Refs = case Cur_Ref of
				 [] ->
					 Refs;
				 _ ->
					 Index = length(Refs) + 1,
					 Ref = lists:flatten( lists:reverse( Cur_Ref ) ),
					 [ {Index, Type, Ref} | Refs]
			 end,
	{ok, Inner_String, Inner_Rest} = parse_rule_inner( Rest, [] ),
	{ok, And_Refs} = parse_rule( Inner_String, [], unknown, [] ),
	Index2 = length(N_Refs) + 1,
	F_Refs = [ { Index2, and_cond, And_Refs } | N_Refs ],
	parse_rule( Inner_Rest, [], unknown, F_Refs );
parse_rule( [61 | Rest], Cur_Ref, Type, Refs ) when Type =/= var_token andalso Type =/= unknown ->
	%61 is the asci value for =
	N_Cur_Ref = [61|Cur_Ref],
	parse_rule( Rest, N_Cur_Ref, var_token, Refs );
parse_rule( [124 | Rest], Cur_Ref, Type, Refs ) when Type =/= expr ->
	%124 is the asci value for |
	N_Refs = case Cur_Ref of
				 [] ->
					 Refs;
				 _ ->
					 Index = length(Refs) + 1,
					 Ref = lists:flatten( lists:reverse( Cur_Ref ) ),
					 [ {Index, Type, Ref} | Refs]
			 end,
	{ok, Or_Refs} = parse_rule( Rest, [], unknown, [] ),
	Index2 = length(N_Refs) + 1,
	F_Refs = [ { Index2, or_cond, Or_Refs } | N_Refs ],
	parse_rule( [], [], unknown, F_Refs );
parse_rule( [C | Rest ], Cur_Ref, unknown, Refs ) when (C >= $A andalso C =< $Z) ->
	N_Cur_Ref = [C|Cur_Ref],
	parse_rule( Rest, N_Cur_Ref, token, Refs );
parse_rule( [C | Rest ], Cur_Ref, unknown, Refs ) when (C >= $a andalso C =< $z) ->
	N_Cur_Ref = [C|Cur_Ref],
	parse_rule( Rest, N_Cur_Ref, rule, Refs );
parse_rule( [C | Rest ], Cur_Ref, Type, Refs ) when (C >= $A andalso C =< $Z) orelse (C >= $a andalso C =< $z) ->
	N_Cur_Ref = [C|Cur_Ref],
	parse_rule( Rest, N_Cur_Ref, Type, Refs );
parse_rule( [C | Rest ], Cur_Ref, Type, Refs ) when Type =/= unknown andalso (C >= $0 andalso C =< $9) ->
	N_Cur_Ref = [C|Cur_Ref],
	parse_rule( Rest, N_Cur_Ref, Type, Refs );
parse_rule( [C | Rest ], Cur_Ref, _, Refs ) ->
	N_Cur_Ref = [C|Cur_Ref],
	parse_rule( Rest, N_Cur_Ref, expr, Refs ).

ignore_until( [], _ ) ->
	{error, not_found};
ignore_until( [C | Rest], C ) ->
	{ok, Rest};
ignore_until( [_ | Rest], C ) ->
	ignore_until( Rest, C ).

parse_until( [], _, Acc ) ->
	{false, lists:reverse(Acc), [] };
parse_until( [C | Rest], C, Acc ) ->
	{true, lists:reverse(Acc), Rest };
parse_until( [C1 | Rest], C, Acc ) ->
	parse_until( Rest, C, [C1 | Acc] ).

parse_rule_inner( [], Acc ) ->
	Parsed_String = lists:reverse( Acc ),
	{ok, Parsed_String, [] };
parse_rule_inner( [ 40 | Rest], Acc ) ->
	%Asci value for { is 40
	{ok, Inner_String, Inner_Rest} = parse_rule_inner( Rest, [] ),
	Current_Acc = [ 40 | Acc ],
	I_Acc = [41 | lists:reverse( Inner_String ) ],
	N_Acc = lists:append( I_Acc, Current_Acc ),
	parse_rule_inner( Inner_Rest, N_Acc );
parse_rule_inner( [ 41 | Rest], Acc ) ->
	%Asci value for } is 41
	Parsed_String = lists:reverse( Acc ),
	{ok, Parsed_String, Rest };
parse_rule_inner( [ C | Rest], Acc ) ->
	parse_rule_inner( Rest, [C | Acc ] ).

resolve_rule_tokens( [], _, Acc ) ->
	{ok, lists:reverse(Acc) };
resolve_rule_tokens( [ {Index, token, Name} | Rest ], Tokens, Acc ) ->
	case lists:keyfind(Name, 1, Tokens) of
		false ->
			{error, {token_not_found, Name} };
		{_, Value } ->
			Resolved_Token = {Index, token, Value },
			N_Acc = [ Resolved_Token | Acc ],
			resolve_rule_tokens( Rest, Tokens, N_Acc )
	end;
resolve_rule_tokens( [ {Index, var_token, {Var, Name} } | Rest ], Tokens, Acc ) ->
	case lists:keyfind(Name, 1, Tokens) of
		false ->
			{error, {token_not_found, Name} };
		{_, Value } ->
			Resolved_Token = {Index, var_token, {Var, Value} },
			N_Acc = [ Resolved_Token | Acc ],
			resolve_rule_tokens( Rest, Tokens, N_Acc )
	end;
resolve_rule_tokens( [ {Index, expr, Expr} | Rest ], Tokens, Acc ) ->
	RE = entlr_util:to_re( Expr, false ),
	Resolved_Token = {Index, expr, RE },
	N_Acc = [ Resolved_Token | Acc ],
	resolve_rule_tokens( Rest, Tokens, N_Acc );
resolve_rule_tokens( [ {Index, and_cond, List} | Rest ], Tokens, Acc ) ->
	{ok, Resolved_List} = resolve_rule_tokens( List, Tokens, [] ),
	Resolved_Entry = {Index, and_cond, Resolved_List },
	N_Acc = [ Resolved_Entry | Acc ],
	resolve_rule_tokens( Rest, Tokens, N_Acc );
resolve_rule_tokens( [ {Index, or_cond, List} | Rest ], Tokens, Acc ) ->
	{ok, Resolved_List} = resolve_rule_tokens( List, Tokens, [] ),
	Resolved_Entry = {Index, or_cond, Resolved_List },
	N_Acc = [ Resolved_Entry | Acc ],
	resolve_rule_tokens( Rest, Tokens, N_Acc );
resolve_rule_tokens( [ T | Rest ], Tokens, Acc ) ->
	resolve_rule_tokens( Rest, Tokens, [T| Acc] ).

resolve_var_tokens( [], Acc ) ->
	Acc;
resolve_var_tokens( [{Index, var_token, T } | Rest ], Acc ) ->
	[Var, Token] = re:split( T, "=", [ {return, list} ] ),
	resolve_var_tokens( Rest, [ {Index, var_token, {Var, Token} } | Acc ] );
resolve_var_tokens( [ T | Rest ], Acc ) ->
	resolve_var_tokens( Rest, [ T | Acc ] ).

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
			L = lists:concat( lists:reverse( Cur_Line ) ),
			Line = string:strip( L, both ),
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
			R_Value = string:strip( Value, both),
			N_Resolved_Tokens = [ {Name, R_Value} | Resolved_Tokens ],
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

convert_to_re( [], _Shorten_WS, Acc ) ->
	lists:reverse( Acc );
convert_to_re( [ {Name, Value} | Rest ], Shorten_WS, Acc ) ->
	RE = entlr_util:to_re( Value, Shorten_WS ),
	convert_to_re( Rest, Shorten_WS, [ {Name, RE} | Acc ] ).

get_refs( Value ) ->
	parse_refs( Value, [], [], none ).


parse_refs( [], Cur_Ref, Refs, C ) when (C >= $A andalso C =< $Z) orelse (C >= $z andalso C =< $z)->
	N_Cur_Ref = [C | Cur_Ref],
	Ref = lists:flatten( lists:reverse( N_Cur_Ref ) ),
	lists:reverse( [Ref | Refs] );
parse_refs( [], Cur_Ref, Refs, _ ) ->
	case Cur_Ref of
		[] ->
			lists:reverse(Refs);
		_ ->
			Ref = lists:flatten( lists:reverse( Cur_Ref ) ),
			lists:reverse( [Ref | Refs] )
	end;
parse_refs( [C | Rest ], Cur_Ref, Refs, none ) ->
	parse_refs( Rest, Cur_Ref, Refs, C );
parse_refs( Rest, Cur_Ref, Refs, 123 ) ->
	%This is the case for {. Ignore until } is found
	%123 is the asci value for '{'
	%125 is the asci value for '}'
	N_Refs = case Cur_Ref of
				 [] ->
					 Refs;
				 _ ->
					 Ref = lists:flatten( lists:reverse( Cur_Ref ) ),
					 [Ref | Refs]
			 end,
	case ignore_until( Rest, 125 ) of
		{error, not_found } ->
			parse_refs( [], [], N_Refs, none );
		{ok, Remaining } ->
			parse_refs( Remaining, [], N_Refs, none )
	end;
parse_refs( Rest, Cur_Ref, Refs, 39 ) ->
	%This is the case for '. Ignore until closing ' is found
	%39 is the asci value for '
	N_Refs = case Cur_Ref of
				 [] ->
					 Refs;
				 _ ->
					 Ref = lists:flatten( lists:reverse( Cur_Ref ) ),
					 [Ref | Refs]
			 end,
	case ignore_until( Rest, 39 ) of
		{error, not_found } ->
			parse_refs( [], [], N_Refs, none );
		{ok, Remaining } ->
			parse_refs( Remaining, [], N_Refs, none )
	end;
parse_refs( [C | Rest ], Cur_Ref, Refs, C1 ) when (C1 >= $A andalso C1 =< $Z) orelse (C1 >= $z andalso C1 =< $z) ->
	N_Cur_Ref = [C1|Cur_Ref],
	parse_refs( Rest, N_Cur_Ref, Refs, C );
parse_refs( [C | Rest ], Cur_Ref, Refs, _ ) -> 
	case Cur_Ref of
		[] ->
			parse_refs( Rest, [], Refs, C );
		_ ->
			Ref = lists:flatten( lists:reverse( Cur_Ref ) ),
			parse_refs( Rest, [], [Ref | Refs], C )
	end.

is_token( Token_Name ) ->
	C = lists:nth(1, Token_Name),
	C >= $A andalso C =< $Z.

is_rule( Rule_Name ) ->
	C = lists:nth(1, Rule_Name),
	C >= $a andalso C =< $z.
