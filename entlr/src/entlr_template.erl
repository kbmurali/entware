%% Author: kbmurali
%% Created: Mar 12, 2012
%% Description: TODO: Add description to entlr_template
-module(entlr_template).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-compile([export_all]).

%%
%% API Functions
%%
process( [], Matched_String, [], _, Vars, true ) ->
	{true, Matched_String, [], Vars };
process( [], _, [], _, _, false ) ->
	false;
process( [], Matched_String, Input, _, Vars, true ) ->
	{true, Matched_String, Input, Vars };
process( [], _, _Input, _, _Vars, false ) ->
	false;
process( _, _, [], _, _, _ ) ->
	false;
process( _, _, _, _, _, false ) ->
	false;
process( [ Cond | Rest ], O_Matched_String, Input, Trim_Left, Vars, true ) ->
	case match( Cond, Input, Trim_Left ) of
		{true, N_Matched_String, N_Input, I_Vars } ->
			Matched_String = append_strings( O_Matched_String, N_Matched_String, Trim_Left ),
			N_Vars = resolve_append( Vars, I_Vars ),
			process( Rest, Matched_String, N_Input, Trim_Left, N_Vars, true );
		false ->
			process( Rest, "", Input, Trim_Left, Vars, false )
	end.

match( {_, token, RE}, Input, Trim_Left ) ->
	String = trim_left( Input, Trim_Left ),
	case re:run( String, RE ) of
		{match, [ {0,L} | _ ] } ->
			case length(String) == L of
				true ->
					{true, String, [], [] };
				false ->
					Matched_String = string:substr(String, 1, L),
					Remaining_Input = string:sub_string( String, L+1 ),
					{true, Matched_String, Remaining_Input, [] }
			end;
		_ ->
			false
	end;
match( {_, var_token, {Var, RE} }, Input, Trim_Left ) ->
	String = trim_left( Input, Trim_Left ),
	case re:run( String, RE ) of
		{match, [ {0,L} | _ ] } ->
			Match = string:substr(String, 1, L),
			
			case length(String) == L of
				true ->
					{true, String, [], [{Var, Match}] };
				false ->
					Matched_String = string:substr(String, 1, L),
					Remaining_Input = string:sub_string( String, L+1 ),
					{true, Matched_String, Remaining_Input, [{Var, [Match]}] }
			end;
		_ ->
			false
	end;
match( {_, expr, RE}, Input, Trim_Left ) ->
	String = trim_left( Input, Trim_Left ),
	case re:run( String, RE ) of
		{match, [ {0,L} | _ ] } ->
			case length(String) == L of
				true ->
					{true, String, [], [] };
				false ->
					Matched_String = string:substr(String, 1, L),
					Remaining_Input = string:sub_string( String, L+1 ),
					{true, Matched_String, Remaining_Input, [] }
			end;
		_ ->
			false
	end;
match( {_, var_expr, {Var, RE} }, Input, Trim_Left ) ->
	String = trim_left( Input, Trim_Left ),
	case re:run( String, RE ) of
		{match, [ {0,L} | _ ] } ->
			Match = string:substr(String, 1, L),
			
			case length(String) == L of
				true ->
					{true, String, [], [{Var, Match}] };
				false ->
					Matched_String = string:substr(String, 1, L),
					Remaining_Input = string:sub_string( String, L+1 ),
					{true, Matched_String, Remaining_Input, [{Var, [Match] }] }
			end;
		_ ->
			false
	end;
match( {_, rule, Rule_Name }, Input, _Trim_Left ) ->
	R_Fun_Name = create_rule_fun_name( Rule_Name ),
	R_Fun = list_to_atom( R_Fun_Name ),
	Mod = module_name(),
	case catch apply( Mod, R_Fun, [Input] ) of
		{error, invalid_input, _} ->
			false;
		{true, Matched_String, [], _} ->
			{true, Matched_String, [], []};
		{partial, Matched_String, Part_Remaining, _ } ->
			{true, Matched_String, Part_Remaining, [] }
	end;
match( {_, var_rule, {Var, Rule_Name} }, Input, _Trim_Left ) ->
	R_Fun_Name = create_rule_fun_name( Rule_Name ),
	R_Fun = list_to_atom( R_Fun_Name ),
	Mod = module_name(),
	case catch apply( Mod, R_Fun, [Input] ) of
		{error, invalid_input, _} ->
			false;
		{true, Matched_String, [], _} ->
			{true, Matched_String, [], [{Var, [Matched_String]}]};
		{partial, Matched_String, Part_Remaining, _ } ->
			{true, Matched_String, Part_Remaining, [{Var, [Matched_String] }] }
	end;
match( {_, and_cond, List }, Input, Trim_Left ) ->
	and_cond( List, "", Input, Trim_Left, [] );
match( {_, var_and_cond, {Var,List} }, Input, Trim_Left ) ->
	case and_cond( List, "", Input, Trim_Left, [] ) of
		{true, Matched_String, Part_Remaining, Vars } ->
			New_Vars = case lists:keyfind( Var, 1, Vars ) of
						   false ->
							   [ {Var, Matched_String} | Vars ];
						   {_, Prev_Values } ->
							   New_Values = lists:append(Prev_Values, [Matched_String] ),
							   lists:keyreplace( Var, 1, Vars, {Var, New_Values} )
					   end,
			{true, Matched_String, Part_Remaining, New_Vars };
		false ->
			false
	end;		
match( {_, or_cond, List }, Input, Trim_Left ) ->
	or_cond( List, Input, Trim_Left, [] );
match( {_, var_or_cond, {Var,List} }, Input, Trim_Left ) ->
	case or_cond( List, Input, Trim_Left, [] ) of
		{true, Matched_String, Part_Remaining, Vars } ->
			New_Vars = case lists:keyfind( Var, 1, Vars ) of
						   false ->
							   [ {Var, Matched_String} | Vars ];
						   {_, Prev_Values } ->
							   New_Values = lists:append(Prev_Values, [Matched_String] ),
							   lists:keyreplace( Var, 1, Vars, {Var, New_Values} )
					   end,
			{true, Matched_String, Part_Remaining, New_Vars };
		false ->
			false
	end;
match( {_, multi_cond, List }, Input, Trim_Left ) ->
	multi_cond( List, "", Input, Trim_Left, [], 0 );
match( {_, var_multi_cond, {Var,List} }, Input, Trim_Left ) ->
	case multi_cond( List, "", Input, Trim_Left, [], 0 ) of
		{true, Matched_String, Part_Remaining, Vars } ->
			New_Vars = case lists:keyfind( Var, 1, Vars ) of
						   false ->
							   [ {Var, Matched_String} | Vars ];
						   {_, Prev_Values } ->
							   New_Values = lists:append(Prev_Values, [Matched_String] ),
							   lists:keyreplace( Var, 1, Vars, {Var, New_Values} )
					   end,
			{true, Matched_String, Part_Remaining, New_Vars };
		false ->
			false
	end.


and_cond( [], Matched_String,  [], _, Vars ) ->
	{true, Matched_String, [], Vars };
and_cond( [], Matched_String, Input, _, Vars ) ->
	{true, Matched_String, Input, Vars };
and_cond( _, _, [], _, _ ) ->
	false;
and_cond( [ Cond | Rest ], O_Matched_String, Input, Trim_Left, Vars  ) ->
	case match( Cond, Input, Trim_Left  ) of
		{true, N_Matched_String, N_Input, I_Vars } ->
			Matched_String = append_strings( O_Matched_String, N_Matched_String, Trim_Left ),
			N_Vars = resolve_append( Vars, I_Vars ),
			and_cond( Rest, Matched_String, N_Input, Trim_Left, N_Vars );
		false ->
			false
	end.

or_cond( [], [], _, _ ) ->
	false;
or_cond( [], _, _, _ ) ->
	false;
or_cond( _, [], _, _ ) ->
	false;
or_cond( [ Cond | Rest ], Input, Trim_Left, Vars ) ->
	case match( Cond, Input, Trim_Left ) of
		{true, Matched_String, N_Input, I_Vars } ->
			N_Vars = resolve_append( Vars, I_Vars ),
			{true, Matched_String, N_Input, N_Vars};
		false ->
			or_cond( Rest, Input, Trim_Left, Vars )
	end.

multi_cond( Conditions, _, Input, Trim_Left, Vars, 0 ) ->
	case multi_cond_loop( Conditions, "", Input, Trim_Left, Vars ) of
		{true, Matched_String, N_Input, I_Vars } ->
			N_Vars = resolve_append( Vars, I_Vars ),
			multi_cond( Conditions, Matched_String, N_Input, Trim_Left, N_Vars, 1 );
		false ->
			false
	end;
multi_cond( Conditions, O_Matched_String, Input, Trim_Left, Vars, N ) ->
	case multi_cond_loop( Conditions, "", Input, Trim_Left, Vars ) of
		{true, N_Matched_String, N_Input, I_Vars } ->
			Matched_String = append_strings( O_Matched_String, N_Matched_String, Trim_Left ),
			N_Vars = resolve_append( Vars, I_Vars ),
			multi_cond( Conditions, Matched_String, N_Input, Trim_Left, N_Vars, N+1 );
		false ->
			{true, O_Matched_String, Input, Vars}
	end.

multi_cond_loop( [], Matched_String, [], _, Vars) ->
	{true, Matched_String, [], Vars };
multi_cond_loop( [], Matched_String, Input, _, Vars ) ->
	{true, Matched_String, Input, Vars };
multi_cond_loop( _, _, [], _, _ ) ->
	false;
multi_cond_loop( [ Cond | Rest ], O_Matched_String, Input, Trim_Left, Vars ) ->
	case match( Cond, Input, Trim_Left ) of
		{true, N_Matched_String, N_Input, I_Vars } ->
			Matched_String = append_strings( O_Matched_String, N_Matched_String, Trim_Left ),
			N_Vars = resolve_append( Vars, I_Vars ),
			multi_cond_loop( Rest, Matched_String, N_Input, Trim_Left, N_Vars );
		false ->
			false
	end.

trim_left( Input, true ) ->
	string:strip( Input, left );
trim_left( Input, false ) ->
	Input.

module_name() ->
	try
		throw( error )
	catch
		_:_ ->
			[ Term | _ ] = erlang:get_stacktrace(),
			case Term of
				{Mod, _, _} ->
					Mod;
				{Mod, _, _, _} ->
					Mod
			end
	end.

create_rule_match_fun_name( Rule_Name ) -> 
	string:to_lower( Rule_Name ) ++ "_match".

create_rule_fun_name( Rule_Name ) ->
	string:to_lower( Rule_Name ).

create_rule_execute_fun_name(Rule_Name) ->
	string:to_lower( Rule_Name ) ++ "_execute_fun".

resolve_append( Vars, I_Vars ) ->
	Fun = fun( {Var,Values}, Acc ) ->
				  case lists:keyfind(Var, 1, Acc ) of
					  false ->
						  [ {Var, Values} | Acc ];
					  {_, Previous_Values } ->
						  New_Values = lists:append( Previous_Values, Values ),
						  lists:keyreplace(Var, 1, Acc, {Var, New_Values} )
				  end
		  end,
	lists:foldr(Fun, Vars, I_Vars ).

append_strings( Str1, Str2, Trim_Left ) ->
	String = Str1 ++ " " ++ Str2,
	trim_left( String, Trim_Left ).