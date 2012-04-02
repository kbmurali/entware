%%% -------------------------------------------------------------------
%%% Author  : Murali Kashaboina
%%% -------------------------------------------------------------------
-module(testre).

-define( CURLY1, "CURLY###ERLANG####BRACKET1").
-define( CURLY2, "CURLY###ERLANG####BRACKET2").


%%
%% Exported Functions
%%
-compile( [export_all ] ).

%%
%% API Functions
%%
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
var_type( _ ) ->
	unknown.

test( Input ) ->
	{_, Rules, Tokens } = entlr_grammar_parser:get_entries(file, "./test/test.g" ),
	[ {_, Rule} | _ ] = Rules,
	{ok, Rule_Tokens} = parse_rule( Rule, [], unknown, [] ),
	resolve_rule_tokens( Rule_Tokens, Tokens, [] ).

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



parse_rule( [], Cur_Ref, Type, Refs, none, none, none ) ->
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
parse_rule( [], Cur_Ref, Type, Refs, C, none, none ) ->
	N_Cur_Ref = [C | Cur_Ref ],
	Index = length(Refs) + 1,
	Ref = lists:flatten( lists:reverse( N_Cur_Ref ) ),
	I_Refs = [ {Index, Type, Ref} | Refs],
	F_Refs = resolve_var_tokens( I_Refs, [] ),
	{ok, F_Refs};
parse_rule( [], Cur_Ref, Type, Refs, C1, C2, none ) ->
	N_Cur_Ref = [ C2 | [C1 | Cur_Ref ] ],
	Index = length(Refs) + 1,
	Ref = lists:flatten( lists:reverse( N_Cur_Ref ) ),
	I_Refs = [ {Index, Type, Ref} | Refs],
	F_Refs = resolve_var_tokens( I_Refs, [] ),
	{ok, F_Refs};
parse_rule( [], Cur_Ref, Type, Refs, C1, C2, C3 ) ->
	N_Cur_Ref = [ C3 | [ C2 | [C1 | Cur_Ref ] ] ],
	Index = length(Refs) + 1,
	Ref = lists:flatten( lists:reverse( N_Cur_Ref ) ),
	I_Refs = [ {Index, Type, Ref} | Refs],
	F_Refs = resolve_var_tokens( I_Refs, [] ),
	{ok, F_Refs};
parse_rule( [ C | Rest ], Cur_Ref, Type, Refs, none, none, none ) when C == 9 orelse C == 32 orelse C == 124 ->
	case Cur_Ref of
		[] ->
			parse_rule( Rest, [], unknown, Refs, none, none, none );
		_ ->
			Index = length(Refs) + 1,
			Ref = lists:flatten( lists:reverse( Cur_Ref ) ),
			parse_rule( Rest, [], unknown, [ {Index, Type,Ref} | Refs], none, none, none )
	end;
parse_rule( [ C | Rest ], Cur_Ref, Type, Refs, C1, none, none ) when C == 9 orelse C == 32 orelse C == 124 ->
	N_Cur_Ref = [C1 | Cur_Ref ],
	Index = length(Refs) + 1,
	Ref = lists:flatten( lists:reverse( N_Cur_Ref ) ),
	parse_rule( Rest, [], unknown, [ {Index, Type,Ref} | Refs], none, none, none );
parse_rule( [ C | Rest ], Cur_Ref, Type, Refs, C1, C2, none ) when C == 9 orelse C == 32 orelse C == 124 ->
	N_Cur_Ref = [C2 | [C1 | Cur_Ref ] ],
	Index = length(Refs) + 1,
	Ref = lists:flatten( lists:reverse( N_Cur_Ref ) ),
	parse_rule( Rest, [], unknown, [ {Index, Type,Ref} | Refs], none, none, none );
parse_rule( [ C | Rest ], Cur_Ref, Type, Refs, C1, C2, C3 ) when C == 9 orelse C == 32 orelse C == 124 ->
	N_Cur_Ref = [C3 | [C2 | [C1 | Cur_Ref ] ] ],
	Index = length(Refs) + 1,
	Ref = lists:flatten( lists:reverse( N_Cur_Ref ) ),
	parse_rule( Rest, [], unknown, [ {Index, Type,Ref} | Refs], none, none, none );
parse_rule( [ 123 | Rest ], Cur_Ref, Type, Refs, none, none, none ) ->
	%This is the case for {. Parse until } is found
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
			parse_rule( [], [], unknown, F_Refs, none, none, none );
		{_, _, _ } ->
			throw( {error, invalid_rule_spec} )
	end;
parse_rule( [ 123 | Rest ], Cur_Ref, Type, Refs, C1, none, none ) ->
	%This is the case for {. Ignore until } is found
	%123 is the asci value for '{'
	%125 is the asci value for '}'
	Index = length(Refs) + 1,
	N_Cur_Ref = [ C1 | Cur_Ref ],
	Ref = lists:flatten( lists:reverse( N_Cur_Ref ) ),
	N_Refs = [{Index, Type, Ref} | Refs],
	
	case parse_until( Rest, 125, [] ) of
		{true, Code, [] } ->
			Index2 = length(N_Refs) + 1,
			F_Refs = [ {Index2, code, Code} | N_Refs ],
			parse_rule( [], [], unknown, F_Refs, none, none, none );
		{ _, _, _ } ->
			throw( {error, invalid_rule_spec} )
	end;
parse_rule( [ 123 | Rest ], Cur_Ref, Type, Refs, C1, C2, none ) ->
	%This is the case for {. Ignore until } is found
	%123 is the asci value for '{'
	%125 is the asci value for '}'
	Index = length(Refs) + 1,
	N_Cur_Ref =  [ C2 | [ C1 | Cur_Ref ] ],
	Ref = lists:flatten( lists:reverse( N_Cur_Ref ) ),
	N_Refs = [{Index, Type, Ref} | Refs],
	
	case parse_until( Rest, 125, [] ) of
		{true, Code, [] } ->
			Index2 = length(N_Refs) + 1,
			F_Refs = [ {Index2, code, Code} | N_Refs ],
			parse_rule( [], [], unknown, F_Refs, none, none, none );
		{ _, _, _ } ->
			throw( {error, invalid_rule_spec} )
	end;
parse_rule( [ 123 | Rest ], Cur_Ref, Type, Refs, C1, C2, C3 ) ->
	%This is the case for {. Ignore until } is found
	%123 is the asci value for '{'
	%125 is the asci value for '}'
	Index = length(Refs) + 1,
	N_Cur_Ref =  [C3 | [ C2 | [ C1 | Cur_Ref ] ] ],
	Ref = lists:flatten( lists:reverse( N_Cur_Ref ) ),
	N_Refs = [{Index, Type, Ref} | Refs],
	
	case parse_until( Rest, 125, [] ) of
		{true, Code, [] } ->
			Index2 = length(N_Refs) + 1,
			F_Refs = [ {Index2, code, Code} | N_Refs ],
			parse_rule( [], [], unknown, F_Refs, none, none, none );
		{_, _, _ } ->
			throw( {error, invalid_rule_spec} )
	end;
parse_rule( [40 | Rest], Cur_Ref, Type, Refs, none, none, none ) ->
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
	{ok, And_Refs} = parse_rule( Inner_String, [], unknown, [], none, none, none ),
	Index2 = length(N_Refs) + 1,
	F_Refs = [ { Index2, and_cond, And_Refs } | N_Refs ],
	parse_rule( Inner_Rest, [], unknown, F_Refs, none, none, none );
parse_rule( [40 | Rest], Cur_Ref, Type, Refs, C1, none, none ) ->
	%32 is the asci value for " "
	%40 is the asci value for (
	Index = length(Refs) + 1,
	N_Cur_Ref = [ C1 | Cur_Ref ],
	Ref = lists:flatten( lists:reverse( N_Cur_Ref ) ),
	N_Refs = [{Index, Type, Ref} | Refs],
	
	{ok, Inner_String, Inner_Rest} = parse_rule_inner( Rest, [] ),
	{ok, And_Refs} = parse_rule( Inner_String, [], unknown, [], none, none, none ),
	Index2 = length(N_Refs) + 1,
	F_Refs = [ { Index2, and_cond, And_Refs } | N_Refs ],
	parse_rule( Inner_Rest, [], unknown, F_Refs, none, none, none );
parse_rule( [40 | Rest], Cur_Ref, Type, Refs, C1, C2, none ) ->
	%32 is the asci value for " "
	%40 is the asci value for (
	Index = length(Refs) + 1,
	N_Cur_Ref = [C2 | [ C1 | Cur_Ref ] ],
	Ref = lists:flatten( lists:reverse( N_Cur_Ref ) ),
	N_Refs = [{Index, Type, Ref} | Refs],
	
	{ok, Inner_String, Inner_Rest} = parse_rule_inner( Rest, [] ),
	{ok, And_Refs} = parse_rule( Inner_String, [], unknown, [], none, none, none ),
	Index2 = length(N_Refs) + 1,
	F_Refs = [ { Index2, and_cond, And_Refs } | N_Refs ],
	parse_rule( Inner_Rest, [], unknown, F_Refs, none, none, none );
parse_rule( [40 | Rest], Cur_Ref, Type, Refs, C1, C2, C3 ) ->
	%32 is the asci value for " "
	%40 is the asci value for (
	Index = length(Refs) + 1,
	N_Cur_Ref = [C3 | [C2 | [ C1 | Cur_Ref ] ] ],
	Ref = lists:flatten( lists:reverse( N_Cur_Ref ) ),
	N_Refs = [{Index, Type, Ref} | Refs],
	
	{ok, Inner_String, Inner_Rest} = parse_rule_inner( Rest, [] ),
	{ok, And_Refs} = parse_rule( Inner_String, [], unknown, [], none, none, none ),
	Index2 = length(N_Refs) + 1,
	F_Refs = [ { Index2, and_cond, And_Refs } | N_Refs ],
	parse_rule( Inner_Rest, [], unknown, F_Refs, none, none, none );
parse_rule( [61 | Rest], Cur_Ref, Type, Refs, C1, none, none ) when Type =/= var_token andalso Type =/= unknown   ->
	%61 is the asci value for =
	parse_rule( Rest, Cur_Ref, var_token, Refs, C1, 61, none );
parse_rule( [61 | Rest], Cur_Ref, Type, Refs, C1, C2, none ) when Type =/= var_token andalso Type =/= unknown   ->
	%61 is the asci value for =
	parse_rule( Rest, Cur_Ref, var_token, Refs, C1, C2, 61 );
parse_rule( [61 | Rest], Cur_Ref, Type, Refs, C1, C2, C3 ) when Type =/= var_token andalso Type =/= unknown   ->
	%61 is the asci value for =
	N_Cur_Ref = [C3 | [C2 | [C1 | Cur_Ref ] ] ],
	parse_rule( Rest, N_Cur_Ref, var_token, Refs, 61, none, none );
parse_rule( [C | Rest ], Cur_Ref, unknown, Refs, none, none, none ) when (C >= $A andalso C =< $Z) ->
	parse_rule( Rest, Cur_Ref, token, Refs, C, none, none );
parse_rule( [C | Rest ], Cur_Ref, unknown, Refs, none, none, none ) when (C >= $a andalso C =< $z) ->
	parse_rule( Rest, Cur_Ref, rule, Refs, C, none, none );
parse_rule( [C | Rest ], Cur_Ref, Type, Refs, C1, none, none ) when (C >= $A andalso C =< $Z) orelse (C >= $a andalso C =< $z) ->
	parse_rule( Rest, Cur_Ref, Type, Refs, C1, C, none );
parse_rule( [C | Rest ], Cur_Ref, Type, Refs, C1, C2, none ) when (C >= $A andalso C =< $Z) orelse (C >= $a andalso C =< $z) ->
	parse_rule( Rest, Cur_Ref, Type, Refs, C1, C2, C );
parse_rule( [C | Rest ], Cur_Ref, Type, Refs, C1, C2, C3 ) when (C >= $A andalso C =< $Z) orelse (C >= $a andalso C =< $z) ->
	N_Cur_Ref = [C3 | [C2 | [C1 | Cur_Ref ] ] ],
	parse_rule( Rest, N_Cur_Ref, Type, Refs, C, none, none );
parse_rule( [C | Rest ], Cur_Ref, _, Refs, none, none, none ) ->
	parse_rule( Rest, Cur_Ref, expr, Refs, C, none, none );
parse_rule( [C | Rest ], Cur_Ref, _, Refs, C1, none, none ) ->
	parse_rule( Rest, Cur_Ref, expr, Refs, C1, C, none );
parse_rule( [C | Rest ], Cur_Ref, _, Refs, C1, C2, none ) ->
	parse_rule( Rest, Cur_Ref, expr, Refs, C1, C2, C );
parse_rule( [C | Rest ], Cur_Ref, _, Refs, C1, C2, C3 ) ->
	N_Cur_Ref = [C3 | [C2 | [C1 | Cur_Ref ] ] ],
	parse_rule( Rest, N_Cur_Ref, expr, Refs, C, none, none ).

parse_rule( [], Cur_Ref, Type, [ {Index, or_cond, Inner_Refs} | O_Refs ] ) ->
	I_Refs = case Cur_Ref of
				 [] ->
					 [ {Index, or_cond, Inner_Refs} | O_Refs ];
				 _ ->
					 Ind1 = length(Inner_Refs) + 1,
					 Ref = lists:flatten( lists:reverse( Cur_Ref ) ),
					 N_Inner_Refs = lists:append( Inner_Refs, [ {Ind1, Type, Ref} ] ),
					 [ { Index, or_cond, N_Inner_Refs } | O_Refs ]
			 end,
	F_Refs = resolve_var_tokens( I_Refs, [] ),
	{ok, F_Refs};
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
parse_rule( [C | Rest ], Cur_Ref, Type, [ {Index, or_cond, Inner_Refs} | O_Refs ] ) when C == 9 orelse C == 32 -> 
	N_Refs = case Cur_Ref of
				 [] ->
					 [ {Index, or_cond, Inner_Refs} | O_Refs ];
				 _ ->
					 Ind1 = length(Inner_Refs) + 1,
					 Ref = lists:flatten( lists:reverse( Cur_Ref ) ),
					 N_Inner_Refs = lists:append( Inner_Refs, [ {Ind1, Type, Ref} ] ),
					 [ { Index, or_cond, N_Inner_Refs } | O_Refs ]
			 end,
	parse_rule( Rest, [], unknown, N_Refs );
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
parse_rule( [42 | Rest], [], unknown, [ { Index, and_cond, Inner_Refs } | O_Refs ] ) ->
	N_Ref = {Index, multi_cond, Inner_Refs },
	F_Refs = [ N_Ref | O_Refs ],
	parse_rule( Rest, [], unknown, F_Refs );
parse_rule( [42 | Rest], Cur_Ref, Type, Refs ) when Type =/= unknown ->
	%42 is the asci value for *
	Index = length(Refs) + 1,
	Ref = lists:flatten( lists:reverse( Cur_Ref ) ),
	N_Ref = {Index, multi_cond, [{1, Type, Ref}] },
	F_Refs = [ N_Ref | Refs ],
	parse_rule( Rest, [], unknown, F_Refs );
parse_rule( [61 | Rest], Cur_Ref, Type, Refs ) when Type =/= var_token andalso Type =/= unknown ->
	%61 is the asci value for =
	N_Cur_Ref = [61|Cur_Ref],
	parse_rule( Rest, N_Cur_Ref, var_token, Refs );
parse_rule( [124 | Rest], Cur_Ref, Type, [ {Index, or_cond, Inner_Refs} | O_Refs ] ) when Type =/= expr ->
	%124 is the asci value for |
	N_Refs = case Cur_Ref of
				 [] ->
					 [ {Index, or_cond, Inner_Refs} | O_Refs ];
				 _ ->
					 Ind1 = length(Inner_Refs) + 1,
					 Ref = lists:flatten( lists:reverse( Cur_Ref ) ),
					 N_Inner_Refs = lists:append( Inner_Refs, [ {Ind1, Type, Ref} ] ),
					 [ { Index, or_cond, N_Inner_Refs } | O_Refs ]
			 end,
	parse_rule( Rest, [], unknown, N_Refs );
parse_rule( [124 | Rest], Cur_Ref, Type, [ {Index, Inner_Type, Inner_Refs} | O_Refs] ) when Type =/= expr ->
	%124 is the asci value for |
	N_Refs = case Cur_Ref of
				 [] ->
					 [ {Index, or_cond, [ {1, Inner_Type, Inner_Refs} ] } | O_Refs ];
				 _ ->
					 Ref = lists:flatten( lists:reverse( Cur_Ref ) ),
					 [ {Index, or_cond, [ {1, Inner_Type, Inner_Refs}, {2, Type, Ref } ] } | O_Refs ]
			 end,
	parse_rule( Rest, [], unknown, N_Refs );
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

resolve_var_tokens( [], Acc ) ->
	Acc;
resolve_var_tokens( [{Index, var_token, T } | Rest ], Acc ) ->
	[Var, Token] = re:split( T, "=", [ {return, list} ] ),
	resolve_var_tokens( Rest, [ {Index, var_token, {Var, Token} } | Acc ] );
resolve_var_tokens( [ T | Rest ], Acc ) ->
	resolve_var_tokens( Rest, [ T | Acc ] ).

match( Input, RE ) ->
	Len = length( Input ),
	case re:run(Input, RE) of
		{match, [{0,Len}] } ->
			true;
		_ ->
			false
	end.
create_re( Input ) ->
	Tokens = re:split(Input,"\\|+",[{return,list}]),
	create_re( Tokens, [] ).
	
create_re( [], Acc ) ->
	lists:concat( lists:reverse( Acc ) );
create_re( [Token | Rest], [] ) ->
	RE = "[" ++ to_re( Token ) ++ "]",
	Acc = [RE],
	create_re( Rest, Acc );
create_re( [Token | Rest], Acc ) ->
	RE = "[" ++ to_re( Token ) ++ "]",
	N_Acc = [RE | Acc],
	create_re( Rest, N_Acc ).

to_re( Input ) ->
	Replace_Ts = [ { "'\\('", ?CURLY1 },
				   { "'\\)'", ?CURLY2 },
				   { "'\\['", "\\\\[" },
				   { "'\\]'", "\\\\]" },
				   { "'\\+'", "\\\\+" },
				   { "'\\?'", "\\\\?" },
				   { "'\\*'", "\\\\*" },
				   { "'\\-'", "\\\\-" },
				   { "'\\.'", "\\\\." },
				   { "\\(", "[" },
				   { "\\)", "]" },
				   { "[\\.]{2}", "-" },
				   { ?CURLY1, "\\(" },
				   { ?CURLY2, "\\)" },
				   { "\\s+", "" },
				   { "'", "" } ],
	
	Replaced_Token = replace( Replace_Ts, Input ),
	
	"[" ++ Replaced_Token ++ "]?".
	

replace( [], R ) ->
	R;
replace( [ {From, To} | Rest], Input ) ->
	R = re:replace( Input, From, To, [global, {return, list}] ),
	replace( Rest, R ).
	
tfun( Token ) ->
	RE = entlr_util:to_re(Token, true),
	entlr_util:create_token_validation_fun(RE).

tfun2( Input ) ->
	RE = "[a-z]",
	case entlr_util:match(Input, RE) of
		true ->
			{true, Input};
		false ->
			{false, Input}
	end.

tfun3( Input ) ->
	RE = [{1,token,"rule"},
		  {2,var_token,{"DName","[a-zA-Z]+[0-9]+"}},
		  {3,code,"io:format( \"~p\", [DName] ) "}],
	case entlr_util:match(Input, RE) of
		true ->
			{true, Input};
		false ->
			{false, Input}
	end.

tfun4( Input ) ->
	Trim_Left = true,
	
	case tfun4_match( Input, Trim_Left ) of
		{true, [], Vars } ->
			{_, DName } = lists:keyfind( "DName", 1, Vars ),
			io:format( "~p", [DName] );
		false ->
			throw( {error, invalid_input, Input} )
	end.

tfun4_match( Input, Trim_Left ) ->
	ok.

get_mod_spec( Mod ) ->
	File = lists:concat( [Mod, ".beam" ] ),
	Path = code:where_is_file( File ),
	case beam_lib:chunks( Path, [abstract_code] ) of
		{ok, {Mod, [ {abstract_code, { _, Form}} ]} } ->
			{ok, Form};
		Error ->
			{error, Error }
	end.

get_func_spec( Mod, Func ) ->
	case get_mod_spec(Mod) of
		{ok, Form} ->
			case find_funcs( Form, Func, [] ) of
				{error, no_function_found } -> 
					{error, no_function_found };
				Funcs ->
					Fun  = fun( F_Spec, Acc ) ->
								   [ F_Spec | Acc ]
						   end,
					lists:foldl(Fun, [], Funcs)
			end;
		Error ->
			Error
	end.

find_funcs( [], _, [] ) ->
	{error, no_function_found };
find_funcs( [], _, Acc ) ->
	lists:reverse(Acc);
find_funcs( [ {function, _, Func, _, _ } = F | Other ], Func, Acc ) ->
	find_funcs( Other, Func, [F | Acc ] );
find_funcs( [ _Tuple | Other ], Func, Acc ) ->
	find_funcs( Other, Func, Acc ).

read_lines( Dev, Cur_Location, Cur_List, Delimiter, Acc ) ->
	case file:pread( Dev, Cur_Location, 1 ) of
		{ok, ";" } ->
			case Delimiter of
				true ->
					Line = lists:concat( lists:reverse( Cur_List ) ),
					N_Acc = [Line | Acc ],
					read_lines( Dev, Cur_Location + 1, [], true, N_Acc );
				false ->
					New_List = [ ";" | Cur_List ],
					read_lines( Dev, Cur_Location + 1, New_List, false, Acc )
			end;
		{ok, "\r" } ->
			read_lines( Dev, Cur_Location + 1, Cur_List, Delimiter, Acc );
		{ok, "\n" } ->
			read_lines( Dev, Cur_Location + 1, Cur_List, Delimiter, Acc );
		{ok, "\t" } ->
			read_lines( Dev, Cur_Location + 1, Cur_List, Delimiter, Acc );
		{ok, C } ->
			New_List = [ C | Cur_List ],
			read_lines( Dev, Cur_Location + 1, New_List, Delimiter, Acc );
		eof ->
			Line = lists:concat( lists:reverse( Cur_List ) ),
			N_Acc = [Line | Acc ],
			N_Lines = [ L || L <- N_Acc, L =/= [] ],
			lists:reverse( N_Lines );
		Error ->
			throw( Error )
	end.

resolve_rules( [], _Resolved_Tokens, Resolved_Rules ) ->
	lists:reverse( Resolved_Rules );
resolve_rules( [ {Name, Value} | Unresolved_Rules ], Resolved_Tokens, Resolved_Rules ) ->
	case get_refs( Value ) of
		[] ->
			R_Value = string:strip( Value, both),
			N_Resolved_Rules = [ {Name, R_Value} | Resolved_Rules ],
			resolve_rules( Unresolved_Rules, Resolved_Tokens, N_Resolved_Rules );
		Refs ->
			case resolve_rule( Refs, Value, Unresolved_Rules, Resolved_Tokens, Resolved_Rules ) of
				{error, invalid_ref} ->
					throw( {error, invalid_ref, Name ++ " : " ++ Value } );
				{error, ref_not_resolved } ->
					N_Unresolved_Rules = lists:append( Unresolved_Rules, [ {Name, Value} ] ),
					resolve_rules( N_Unresolved_Rules, Resolved_Tokens, Resolved_Rules );
				{ok, N_Value} ->
					N_Resolved_Rules = [ {Name, N_Value} | Resolved_Rules ],
					resolve_rules( Unresolved_Rules, Resolved_Tokens, N_Resolved_Rules )
			end
	end.

resolve_rule( [], Value, _Unresolved_Rules, _Resolved_Tokens, _Resolved_Rules ) ->
	{ok, Value};
resolve_rule( [Ref | Rest ], Value, Unresolved_Rules, Resolved_Tokens, Resolved_Rules ) ->
	case is_token( Ref ) of
		false ->
			case is_rule( Ref ) of
				false ->
					resolve_rule( Rest, Value, Unresolved_Rules, Resolved_Tokens, Resolved_Rules );
				true ->
					case lists:keyfind( Ref, 1, Resolved_Rules ) of
						false ->
							case lists:keyfind( Ref, 1, Unresolved_Rules ) of
								false ->
									{error, invalid_ref};
								_ ->
									{error, ref_not_resolved}
							end;
						{_, T_Value } ->
							N_Value = re:replace( Value, Ref, T_Value, [ {return, list} ] ),
							resolve_rule( Rest, N_Value, Unresolved_Rules, Resolved_Tokens, Resolved_Rules )
					end
			end;
		true ->
			case lists:keyfind( Ref, 1, Resolved_Tokens ) of
				false ->
					{error, invalid_ref};
				{_, T_Value } ->
					N_Value = re:replace( Value, Ref, T_Value, [ {return, list} ] ),
					resolve_rule( Rest, N_Value, Unresolved_Rules, Resolved_Tokens, Resolved_Rules )
			end
	end.


is_token( Token_Name ) ->
	C = lists:nth(1, Token_Name),
	C >= $A andalso C =< $Z.

is_rule( Rule_Name ) ->
	C = lists:nth(1, Rule_Name),
	C >= $a andalso C =< $z.

get_refs( Value ) ->
	Value.

create_token_validation_fun( RE ) ->
	N = 1,
	M = N + 1,
	O = M + 1,
	P = O + 1,
	Q = P + 1,
	R = Q + 1,
	S = R + 1,
	T = S + 1,
	
	G_Fun = { 'fun',N, {clauses,
						[{clause,M,
						  [{var,M,'Input'}],
						  [],
						  [{match,O,
							{var,O,'RE'},
							{string,O,RE}},
						   {'case',Q,
							{'catch',Q,
							 {call,Q,
							  {remote,Q,{atom,Q,entlr_util},{atom,Q,match}},
							  [{var,Q,'Input'},{var,Q,'RE'}]}},
							[{clause,R,[{atom,R,true}],[],[{tuple,S,[{atom,S,true},{var,S,'Input'}]}] },
							 {clause,T,[{var,T,'_'}],[], [{tuple,T,[{atom,T,false},{var,T,'Input'}]}] }]}]}]}},

	{value, Fun, _ } = erl_eval:expr( G_Fun, [] ),
	Fun.

create_token_fun( Token, RE ) ->
	F_Name = create_token_fun_name(Token),
	N = 1,
	M = N + 1,
	O = M + 1,
	P = O + 1,
	Q = P + 1,
	R = Q + 1,
	S = R + 1,
	T = S + 1,
	
	G_Fun = { 'fun',N, {clauses,
						[{clause,M,
						  [{var,M,'Input'}],
						  [],
						  [{match,O,
							{var,O,'RE'},
							{string,O,RE}},
						   {'case',Q,
							{'catch',Q,
							 {call,Q,
							  {remote,Q,{atom,Q,entlr_util},{atom,Q,match}},
							  [{var,Q,'Input'},{var,Q,'RE'}]}},
							[{clause,R,[{atom,R,true}],[],[{tuple,S,[{atom,S,true},{var,S,'Input'}]}] },
							 {clause,T,[{var,T,'_'}],[], [{tuple,T,[{atom,T,false},{var,T,'Input'}]}] }]}]}]}},
	
	Fun_String = erl_prettypr:format(G_Fun),
	[ _, _, _ | Rest1 ] = Fun_String,
	[ _, _, _ | Rest2 ] = lists:reverse( Rest1 ),
	Body = lists:reverse( Rest2 ) ++ ".",
	lists:append( F_Name, Body ).
	

create_token_fun_name( Token ) ->
	string:to_lower( Token ) ++ "_token".