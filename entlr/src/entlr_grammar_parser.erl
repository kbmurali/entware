%% Author: kbmurali
%% Created: Mar 5, 2012
%% Description: TODO: Add description to entlr_grammar_parser
-module(entlr_grammar_parser).

%%
%% Exported Functions
%%
-export([get_entries/2]).
-export([parse_rule/5]).
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

parse_rule( [], Cur_Ref, Type, [], Var ) ->
	N_Refs = case Cur_Ref of
				 [] ->
					 [];
				 _ ->
					 Index = 1,
					 Ref = lists:flatten( lists:reverse( Cur_Ref ) ),
					 case Var of
						 [] ->
							 [{Index, Type, Ref}];
						 _ ->
							 Var_Type = var_type( Type ),
							 [{Index, Var_Type, {Var, Ref} }]
					 end
			 end,
	F_Refs = lists:reverse( N_Refs ),
	{ok, F_Refs };						 
parse_rule( [], Cur_Ref, Type, [ {Index, or_cond, Inner_Refs} | O_Refs ], Var ) ->
	I_Refs = case Cur_Ref of
				 [] ->
					 case Var of
						 [] ->
							 [ {Index, or_cond, Inner_Refs} | O_Refs ];
						 _ ->
							 Var_Type = var_type( or_cond ),
							 [ {Index, Var_Type, {Var,Inner_Refs} } | O_Refs ]
					 end;
				 _ ->
					 Ind1 = length(Inner_Refs) + 1,
					 Ref = lists:flatten( lists:reverse( Cur_Ref ) ),
					 N_Inner_Ref = case Var of
									   [] ->
										   {Ind1, Type, Ref};
									   _ ->
										   Var_Type = var_type( Type ),
										   {Ind1, Var_Type, {Var, Ref} }
								   end,
					 
					 N_Inner_Refs = lists:append( Inner_Refs, [ N_Inner_Ref ] ),
					 [ { Index, or_cond, N_Inner_Refs } | O_Refs ]
			 end,
	F_Refs = lists:reverse( I_Refs ),
	{ok, F_Refs};
parse_rule( [], Cur_Ref, Type, [{Index, Inner_Type, Inner_Refs} | O_Refs ] = Refs, Var ) ->
	N_Refs = case Cur_Ref of
				 [] ->
					 case Var of
						 [] ->
							 [ {Index, Inner_Type, Inner_Refs} | O_Refs ];
						 _ ->
							 Var_Type = var_type( Inner_Type ),
							 [ {Index, Var_Type, {Var,Inner_Refs} } | O_Refs ]
					 end;
				 _ ->
					 Ind1 = length(Refs) + 1,
					 Ref = lists:flatten( lists:reverse( Cur_Ref ) ),
					 N_Ref = case Var of
								 [] ->
									 {Ind1, Type, Ref };
								 _ ->
									 Var_Type = var_type( Type ),
									 {Ind1, Var_Type, {Var, Ref} }
							 end,
					 [ N_Ref | Refs]
			 end,
	F_Refs = lists:reverse( N_Refs ),
	{ok, F_Refs};
parse_rule( [C | Rest ], Cur_Ref, Type, [ {Index, or_cond, Inner_Refs} | O_Refs ], Var ) when C == 9 orelse C == 32 -> 
	N_Refs = case Cur_Ref of
				 [] ->
					 case Var of
						 [] ->
							 [ {Index, or_cond, Inner_Refs} | O_Refs ];
						 _ ->
							 Var_Type = var_type( or_cond ),
							 [ {Index, Var_Type, {Var,Inner_Refs} } | O_Refs ]
					 end;
				 _ ->
					 Ind1 = length(Inner_Refs) + 1,
					 Ref = lists:flatten( lists:reverse( Cur_Ref ) ),
					 N_Ref = case Var of
								 [] ->
									 {Ind1, Type, Ref };
								 _ ->
									 Var_Type = var_type( Type ),
									 {Ind1, Var_Type, {Var, Ref} }
							 end, 
					 N_Inner_Refs = lists:append( Inner_Refs, [ N_Ref ] ),
					 [ { Index, or_cond, N_Inner_Refs } | O_Refs ]
			 end,
	parse_rule( Rest, [], unknown, N_Refs, [] );
parse_rule( [C | Rest ], Cur_Ref, Type, [{Inner_Index, Inner_Type, Inner_Refs} | O_Refs ] = Refs, Var ) when C == 9 orelse C == 32 -> 
	case Cur_Ref of
		[] ->
			N_Refs = case Var of
					   [] ->
						   Refs;
					   _ ->
						   Var_Type = var_type( Inner_Type ),
						   [ {Inner_Index, Var_Type, {Var,Inner_Refs} } | O_Refs ]
				   end,
			parse_rule( Rest, [], unknown, N_Refs, [] );
		_ ->
			Index = length(Refs) + 1,
			Ref = lists:flatten( lists:reverse( Cur_Ref ) ),
			N_Ref = case Var of
						[] ->
							{Index, Type, Ref };
						_ ->
							Var_Type = var_type( Type ),
							{Index, Var_Type, {Var, Ref} }
					end,
			parse_rule( Rest, [], unknown, [ N_Ref | Refs], [] )
	end;
parse_rule( [C | Rest ], Cur_Ref, Type, [], Var ) when C == 9 orelse C == 32 -> 
	case Cur_Ref of
		[] ->
			parse_rule( Rest, [], unknown, [], [] );
		_ ->
			Index = 1,
			Ref = lists:flatten( lists:reverse( Cur_Ref ) ),
			N_Ref = case Var of
						[] ->
							{Index, Type, Ref };
						_ ->
							Var_Type = var_type( Type ),
							{Index, Var_Type, {Var, Ref} }
					end,
			parse_rule( Rest, [], unknown, [N_Ref], [] )
	end;
parse_rule( [ 123 | Rest ], Cur_Ref, Type, [{Inner_Index, Inner_Type, Inner_Refs} | O_Refs ] = Refs, Var ) ->
	%This is the case for {. Ignore until } is found
	%123 is the asci value for '{'
	%125 is the asci value for '}'
	N_Refs = case Cur_Ref of
				 [] ->
					 case Var of
						 [] ->
							 Refs;
						 _ ->
							 Var_Type = var_type( Inner_Type ),
							 [ {Inner_Index, Var_Type, {Var,Inner_Refs} } | O_Refs ]
					 end;
				 _ ->
					 Index = length(Refs) + 1,
					 Ref = lists:flatten( lists:reverse( Cur_Ref ) ),
					 N_Ref = case Var of
								 [] ->
									 {Index, Type, Ref };
								 _ ->
									 Var_Type = var_type( Type ),
									 {Index, Var_Type, {Var, Ref} }
							 end,
					 [ N_Ref | Refs]
			 end,
	case parse_until( Rest, 125, [] ) of
		{true, Code, [] } ->
			Index2 = length(N_Refs) + 1,
			F_Refs = [ {Index2, code, Code} | N_Refs ],
			parse_rule( [], [], unknown, F_Refs, [] );
		{_, _, _ } ->
			throw( {error, invalid_specification} )
	end;
parse_rule( [40 | Rest], Cur_Ref, Type, Refs, Var ) ->
	%32 is the asci value for " "
	%40 is the asci value for (
	{N_Refs,N_Var} = case Cur_Ref of
						 [] ->
							 {Refs, Var};
						 _ ->
							 Index = length(Refs) + 1,
							 Ref = lists:flatten( lists:reverse( Cur_Ref ) ),
							 case Var of
								 [] ->
									 {[ {Index, Type, Ref} | Refs], [] };
								 _ ->
									 Var_Type = var_type( Type ),
									 {[ {Index, Var_Type, {Var, Ref} } | Refs], [] }
							 end
					 end,
	{ok, Inner_String, Inner_Rest} = parse_rule_inner( Rest, [] ),
	{ok, And_Refs} = parse_rule( Inner_String, [], unknown, [], [] ),
	Index2 = length(N_Refs) + 1,
	F_Refs = case N_Var of
				 [] ->
					 [ { Index2, and_cond, And_Refs } | N_Refs ];
				 _ ->
					 Var_Type2 = var_type( and_cond ),
					 [ { Index2, Var_Type2, {N_Var, And_Refs} } | N_Refs ]
			 end,
	
	parse_rule( Inner_Rest, [], unknown, F_Refs, [] );
parse_rule( [42 | Rest], [], unknown, [ { Inner_Index, and_cond, Inner_Refs } | O_Refs ], _ ) ->
	N_Ref = {Inner_Index, multi_cond, Inner_Refs },
	F_Refs = [ N_Ref | O_Refs ],
	parse_rule( Rest, [], unknown, F_Refs, [] );
parse_rule( [42 | Rest], [], unknown, [ { Inner_Index, _, {Var, Inner_Refs} } | O_Refs ], _ ) ->
	Var_Type = var_type( multi_cond ),
	N_Ref = {Inner_Index, Var_Type, {Var, Inner_Refs} },
	F_Refs = [ N_Ref | O_Refs ],
	parse_rule( Rest, [], unknown, F_Refs, [] );
parse_rule( [42 | Rest], Cur_Ref, Type, Refs, Var ) when Type =/= unknown ->
	%42 is the asci value for *
	Index = length(Refs) + 1,
	Ref = lists:flatten( lists:reverse( Cur_Ref ) ),
	I_Ref = case Var of
				[] ->
					{1, Type, Ref};
				_ ->
					Var_Type = var_type( Type ),
					{1, Var_Type, {Var, Ref} }
			end,
	
	N_Ref = {Index, multi_cond, [I_Ref] },
	F_Refs = [ N_Ref | Refs ],
	parse_rule( Rest, [], unknown, F_Refs, [] );
parse_rule( [61 | Rest], Cur_Ref, Type, Refs, _ ) when Type =/= unknown ->
	%61 is the asci value for =
	Var = lists:flatten( lists:reverse( Cur_Ref ) ),
	parse_rule( Rest, [], unknown, Refs, Var );
parse_rule( [124 | Rest], Cur_Ref, Type, [ {Inner_Index, or_cond, Inner_Refs} | O_Refs ], Var ) when Type =/= expr ->
	%124 is the asci value for |
	N_Refs = case Cur_Ref of
				 [] ->
					 case Var of
						 [] ->
							 [ {Inner_Index, or_cond, Inner_Refs} | O_Refs ];
						 _ ->
							 Var_Type = var_type( or_cond ),
							 [ {Inner_Index, Var_Type, {Var,Inner_Refs} } | O_Refs ]
					 end;
				 _ ->
					 Ind1 = length(Inner_Refs) + 1,
					 Ref = lists:flatten( lists:reverse( Cur_Ref ) ),
					 N_Ref = case Var of
								 [] ->
									 {Ind1, Type, Ref };
								 _ ->
									 Var_Type = var_type( Type ),
									 {Ind1, Var_Type, {Var, Ref} }
							 end, 
					 N_Inner_Refs = lists:append( Inner_Refs, [ N_Ref ] ),
					 [ { Inner_Index, or_cond, N_Inner_Refs } | O_Refs ]
			 end,
	
	parse_rule( Rest, [], unknown, N_Refs, [] );
parse_rule( [124 | Rest], Cur_Ref, Type, [ {Inner_Index, Inner_Type, Inner_Refs} | O_Refs], Var ) when Type =/= expr ->
	%124 is the asci value for |
	N_Refs = case Cur_Ref of
				 [] ->
					 case Var of
						 [] ->
							 [ {Inner_Index, or_cond, [ {1, Inner_Type, Inner_Refs} ] } | O_Refs ];
						 _ ->
							 Var_Type = var_type( or_cond ),
							 [ {Inner_Index, Var_Type, {Var, [ {1, Inner_Type, Inner_Refs} ] } } | O_Refs ]
					 end;
				 _ ->
					 Ref = lists:flatten( lists:reverse( Cur_Ref ) ),
					 case Var of
						 [] ->
							 [ {Inner_Index, or_cond, [ {1, Inner_Type, Inner_Refs}, {2, Type, Ref } ] } | O_Refs ];
						 _ ->
							 Var_Type = var_type( Type ),
							 [ {Inner_Index, or_cond, [ {1, Inner_Type, Inner_Refs}, {2, Var_Type, {Var, Ref} } ] } | O_Refs ]
					 end 
			 end,
	parse_rule( Rest, [], unknown, N_Refs, [] );
parse_rule( [C | Rest ], Cur_Ref, unknown, Refs, Var ) when (C >= $A andalso C =< $Z) ->
	N_Cur_Ref = [C|Cur_Ref],
	parse_rule( Rest, N_Cur_Ref, token, Refs, Var );
parse_rule( [C | Rest ], Cur_Ref, unknown, Refs, Var ) when (C >= $a andalso C =< $z) ->
	N_Cur_Ref = [C|Cur_Ref],
	parse_rule( Rest, N_Cur_Ref, rule, Refs, Var );
parse_rule( [C | Rest ], Cur_Ref, Type, Refs, Var ) when (C >= $A andalso C =< $Z) orelse (C >= $a andalso C =< $z) ->
	N_Cur_Ref = [C|Cur_Ref],
	parse_rule( Rest, N_Cur_Ref, Type, Refs, Var );
parse_rule( [C | Rest ], Cur_Ref, Type, Refs, Var ) when Type =/= unknown andalso (C >= $0 andalso C =< $9) ->
	N_Cur_Ref = [C|Cur_Ref],
	parse_rule( Rest, N_Cur_Ref, Type, Refs, Var );
parse_rule( [C | Rest ], Cur_Ref, _, Refs, Var ) ->
	N_Cur_Ref = [C|Cur_Ref],
	parse_rule( Rest, N_Cur_Ref, expr, Refs, Var ).

var_type( token ) ->
	var_token;
var_type( expr ) ->
	var_expr;
var_type( or_cond ) ->
	var_or_cond;
var_type( and_cond ) ->
	var_and_cond;
var_type( multi_cond ) ->
	var_multi_cond;
var_type( rule ) ->
	var_rule;
var_type( Type ) ->
	Type.

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
resolve_rule_tokens( [ {Index, var_expr, {Var, Expr} } | Rest ], Tokens, Acc ) ->
	RE = entlr_util:to_re( Expr, false ),
	Resolved_Token = {Index, var_expr, {Var, RE} },
	N_Acc = [ Resolved_Token | Acc ],
	resolve_rule_tokens( Rest, Tokens, N_Acc );
resolve_rule_tokens( [ {Index, and_cond, List} | Rest ], Tokens, Acc ) ->
	{ok, Resolved_List} = resolve_rule_tokens( List, Tokens, [] ),
	Resolved_Entry = {Index, and_cond, Resolved_List },
	N_Acc = [ Resolved_Entry | Acc ],
	resolve_rule_tokens( Rest, Tokens, N_Acc );
resolve_rule_tokens( [ {Index, var_and_cond, {Var, List} } | Rest ], Tokens, Acc ) ->
	{ok, Resolved_List} = resolve_rule_tokens( List, Tokens, [] ),
	Resolved_Entry = {Index, var_and_cond, {Var, Resolved_List} },
	N_Acc = [ Resolved_Entry | Acc ],
	resolve_rule_tokens( Rest, Tokens, N_Acc );
resolve_rule_tokens( [ {Index, or_cond, List} | Rest ], Tokens, Acc ) ->
	{ok, Resolved_List} = resolve_rule_tokens( List, Tokens, [] ),
	Resolved_Entry = {Index, or_cond, Resolved_List },
	N_Acc = [ Resolved_Entry | Acc ],
	resolve_rule_tokens( Rest, Tokens, N_Acc );
resolve_rule_tokens( [ {Index, var_or_cond, {Var, List} } | Rest ], Tokens, Acc ) ->
	{ok, Resolved_List} = resolve_rule_tokens( List, Tokens, [] ),
	Resolved_Entry = {Index, var_or_cond, {Var, Resolved_List} },
	N_Acc = [ Resolved_Entry | Acc ],
	resolve_rule_tokens( Rest, Tokens, N_Acc );
resolve_rule_tokens( [ {Index, multi_cond, List} | Rest ], Tokens, Acc ) ->
	{ok, Resolved_List} = resolve_rule_tokens( List, Tokens, [] ),
	Resolved_Entry = {Index, multi_cond, Resolved_List },
	N_Acc = [ Resolved_Entry | Acc ],
	resolve_rule_tokens( Rest, Tokens, N_Acc );
resolve_rule_tokens( [ {Index, var_multi_cond, {Var, List} } | Rest ], Tokens, Acc ) ->
	{ok, Resolved_List} = resolve_rule_tokens( List, Tokens, [] ),
	Resolved_Entry = {Index, var_multi_cond, {Var, Resolved_List} },
	N_Acc = [ Resolved_Entry | Acc ],
	resolve_rule_tokens( Rest, Tokens, N_Acc );
resolve_rule_tokens( [ T | Rest ], Tokens, Acc ) ->
	resolve_rule_tokens( Rest, Tokens, [T| Acc] ).


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
