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
process( [], [], _, Vars, _, true ) ->
	{true, [], Vars };
process( [], [], _, _, _, false ) ->
	false;
process( [], Input, _, Vars, _, true ) ->
	{true, Input, Vars };
process( [], _Input, _, _Vars, _, false ) ->
	false;
process( _, [], _, _, _, _ ) ->
	false;
process( [ {_,or_cond, _} = Cond | Rest ], Input, Trim_Left, Vars, Original_Input, false ) ->
	case match( Cond, Input, Trim_Left, Original_Input ) of
		{true, N_Input, I_Vars } ->
			N_Vars = lists:append( Vars, I_Vars ),
			process( Rest, N_Input, Trim_Left, N_Vars, Original_Input, true );
		false ->
			process( Rest, Input, Trim_Left, Vars, Original_Input, false )
	end;
process( _, _, _, _, _, false ) ->
	false;
process( [ Cond | Rest ], Input, Trim_Left, Vars, Original_Input, true ) ->
	case match( Cond, Input, Trim_Left, Original_Input ) of
		{true, N_Input, I_Vars } ->
			N_Vars = lists:append( Vars, I_Vars ),
			process( Rest, N_Input, Trim_Left, N_Vars, Original_Input, true );
		false ->
			process( Rest, Input, Trim_Left, Vars, Original_Input, false )
	end.

match( {_, token, RE}, Input, Trim_Left, _Original_Input ) ->
	String = trim_left( Input, Trim_Left ),
	case re:run( String, RE ) of
		{match, [ {0,L} | _ ] } ->
			case length(String) == L of
				true ->
					{true, [], [] };
				false ->
					N_Input = string:sub_string( String, L+1 ),
					{true, N_Input, [] }
			end;
		_ ->
			false
	end;
match( {_, var_token, {Var, RE} }, Input, Trim_Left, _Original_Input ) ->
	String = trim_left( Input, Trim_Left ),
	case re:run( String, RE ) of
		{match, [ {0,L} | _ ] } ->
			Match = string:substr(String, 1, L),
			
			case length(String) == L of
				true ->
					{true, [], [{Var, Match}] };
				false ->
					N_Input = string:sub_string( String, L+1 ),
					{true, N_Input, [{Var, Match}] }
			end;
		_ ->
			false
	end;
match( {_, rule, Rule_Name }, _Input, _Trim_Left, Original_Input ) ->
	R_Fun_Name = entlr_mod_util:create_rule_fun_name( Rule_Name ),
	R_Fun = list_to_atom( R_Fun_Name ),
	Mod = module_name(),
	case catch apply( Mod, R_Fun, [Original_Input] ) of
		{error, invalid_input, _} ->
			false;
		_ ->
			throw( {rule_executed, Rule_Name} )
	end;
match( {_, and_cond, List }, Input, Trim_Left, Original_Input ) ->
	and_cond( List, Input, Trim_Left, [], Original_Input );
match( {_, or_cond, List }, Input, Trim_Left, Original_Input ) ->
	or_cond( List, Input, Trim_Left, [], Original_Input ).


and_cond( [], [], _, Vars, _ ) ->
	{true, [], Vars };
and_cond( [], Input, _, Vars, _ ) ->
	{true, Input, Vars };
and_cond( _, [], _, _, _ ) ->
	false;
and_cond( [ Cond | Rest ], Input, Trim_Left, Vars, Original_Input ) ->
	case match( Cond, Input, Trim_Left, Original_Input ) of
		{true, N_Input, I_Vars } ->
			N_Vars = lists:append( Vars, I_Vars ),
			and_cond( Rest, N_Input, Trim_Left, N_Vars, Original_Input );
		false ->
			false
	end.

or_cond( [], [], _, Vars, _ ) ->
	{false, [], Vars };
or_cond( [], Input, _, Vars, _ ) ->
	{false, Input, Vars };
or_cond( _, [], _, _, _ ) ->
	false;
or_cond( [ Cond | Rest ], Input, Trim_Left, Vars, Original_Input ) ->
	case match( Cond, Input, Trim_Left, Original_Input ) of
		{true, N_Input, I_Vars } ->
			N_Vars = lists:append( Vars, I_Vars ),
			{true, N_Input, N_Vars};
		false ->
			or_cond( Rest, Input, Trim_Left, Vars, Original_Input )
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