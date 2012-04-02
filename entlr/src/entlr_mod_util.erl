%% Author: kbmurali
%% Created: Mar 12, 2012
%% Description: TODO: Add description to entlr_mod_util
-module(entlr_mod_util).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-compile( [export_all]).

%%
%% API Functions
%%
create_mod_form( Grammar_Name, Rules, Trim_Left ) ->
	{ok, Template} = get_mod_spec( entlr_template ),
	Mod_Name = list_to_atom( Grammar_Name ),
	[_, _ | Rest ] = Template,
	M_Template = [ {attribute,4,module,Mod_Name} | Rest ],
	[ {eof,_}  | R_Template ] = lists:reverse( M_Template ),
	add_rule_funs( Rules, R_Template, Trim_Left ).

add_rule_funs( [], Template, _ ) ->
	lists:reverse( Template );
add_rule_funs( [ {Rule_Name, Rule_Tree} | Rest ], Template, Trim_Left ) ->
	Vars = get_vars( Rule_Tree, [] ),
	Match_Fun_Form = create_rule_match_fun( Rule_Name, Rule_Tree  ),
	Rule_Fun_Form = create_rule_fun(Rule_Name, Rule_Tree, Vars, Trim_Left ),
	N_Template = case lists:keyfind( code, 2, Rule_Tree ) of
					 false ->
						 [Rule_Fun_Form | [Match_Fun_Form | Template] ];
					 {_,_,Code} ->
						 Rule_Execute_Code_Fun = create_rule_execute_fun( Rule_Name, Vars, Code ),
						 [Rule_Fun_Form | [Rule_Execute_Code_Fun | [Match_Fun_Form | Template] ] ]
				 end,
	add_rule_funs( Rest, N_Template, Trim_Left ).
	

create_rule_fun( Rule_Name, Rule_Tree, Vars, Trim_Left ) ->
	Trim_Left_Var = get_unique_var( "Trim_Left", Vars, 0 ),
	Input_Var = get_unique_var( "Input", Vars, 0 ),
	Match_Vars = get_unique_var( "Vars", Vars, 0 ),
	Matched_String = get_unique_var( "Matched_String", Vars, 0 ),
	Remaining_Part = get_unique_var( "Remaining_Part", Vars, 0 ),
	
	Fun_String = entlr_template:create_rule_fun_name(Rule_Name) ++ "( " ++ Input_Var ++ ") -> \n" ++
					 Trim_Left_Var ++ " = " ++ to_str( Trim_Left ) ++ ",\n" ++
					 "case catch " ++ entlr_template:create_rule_match_fun_name( Rule_Name ) ++ "( " ++ Input_Var ++ "," ++ Trim_Left_Var ++ ") of \n", 
					 
	F_String2 = case lists:keyfind( code, 2, Rule_Tree ) of
					false ->
						Fun_String ++ "{true," ++ Matched_String ++ ", [], " ++ Match_Vars ++ "} -> \n" ++ 
								"{true," ++ Matched_String ++ ", [], " ++ Match_Vars ++ "};\n" ++
							"{true, " ++ Matched_String ++ ", " ++ Remaining_Part ++ ", " ++ Match_Vars ++ "} -> \n" ++
								"{partial, " ++ Matched_String ++ ", " ++ Remaining_Part ++ ", " ++ Match_Vars ++ "};\n" ++
							"false -> \n" ++
								"{error, invalid_input," ++  Input_Var ++ "}" ++
							"end.";
					_ ->
						Fun_String ++ "{true," ++ Matched_String ++ ", [], " ++ Match_Vars ++ "} -> \n" ++
								"try\n" ++
									entlr_template:create_rule_execute_fun_name(Rule_Name) ++ "( " ++ Match_Vars ++ ")\n" ++
								"catch\n" ++
									"_:_ -> ok\n" ++
								"end,\n" ++
								"{true, " ++ Matched_String ++ ", [], " ++ Match_Vars ++ "};\n" ++
							"{true, " ++ Matched_String ++ ", " ++ Remaining_Part ++ ", " ++ Match_Vars ++ "} -> \n" ++
								"try\n" ++
									entlr_template:create_rule_execute_fun_name(Rule_Name) ++ "( " ++ Match_Vars ++ ")\n" ++
								"catch\n" ++
									"_:_ -> ok\n" ++
								"end,\n" ++
								"{partial, " ++ Matched_String ++ ", " ++ Remaining_Part ++ ", " ++ Match_Vars ++ "};\n" ++
							"false -> \n" ++
								"{error, invalid_input," ++  Input_Var ++ "}" ++
							"end."
				end,
					
	
	{ok, FT, _} = erl_scan:string( F_String2 ),
	{ok, FF} = erl_parse:parse_form( FT ),
	FF.
	

create_rule_match_fun( Rule_Name, Rule_Tree ) ->
	Tree = lists:keydelete(code, 2, Rule_Tree),
	Fun_String = entlr_template:create_rule_match_fun_name( Rule_Name ) ++ "( Input, Trim_Left ) -> \n" ++
					 "Rules_Tree = " ++ to_str( Tree ) ++ "," ++
					 "process( Rules_Tree, \"\", Input, Trim_Left, [], true ).",
	
	{ok, FT, _} = erl_scan:string( Fun_String ),
	{ok, FF} = erl_parse:parse_form( FT ),
	FF.

create_rule_execute_fun( Rule_Name, Vars, Code ) ->
	Match_Vars = get_unique_var( "Vars", Vars, 0 ),
	
	Fun_String = entlr_template:create_rule_execute_fun_name(Rule_Name) ++ "( " ++ Match_Vars ++ ") -> \n",
	
	Fun = fun( Var, {Comma, F_String} ) ->
				  N_F_String = case Comma of
								   true ->
									   F_String ++ ",\n" ++ 
										   "{_, " ++ Var ++ "} = lists:keyfind( \"" ++ Var ++ "\", 1," ++  Match_Vars  ++ ")";
								   false ->
									   F_String ++ 
										   "{_, " ++ Var ++ "} = lists:keyfind( \"" ++ Var ++ "\", 1," ++  Match_Vars  ++ ")"
							   end,
				  {true, N_F_String }
		  end,
	
	{Comma, F_String} = lists:foldl(Fun, {false, Fun_String }, Vars),
	
	F_String2 = case Comma of
							true ->
								F_String ++ ",\n" ++ Code ++ ".";
							false ->
								F_String ++ Code ++ "."
						end,
					
	{ok, FT, _} = erl_scan:string( F_String2 ),
	{ok, FF} = erl_parse:parse_form( FT ),
	FF.


get_mod_spec( Mod ) ->
	File = lists:concat( [Mod, ".beam" ] ),
	Path = code:where_is_file( File ),
	case beam_lib:chunks( Path, [abstract_code] ) of
		{ok, {Mod, [ {abstract_code, { _, Form}} ]} } ->
			{ok, Form};
		Error ->
			{error, Error }
	end.

get_vars( [], Vars ) ->
	Vars;
get_vars( [{_,var_token, {Name, _} } | Rest ], Vars ) ->
	get_vars( Rest, [Name | Vars ] );
get_vars( [{_,var_expr, {Name, _} } | Rest ], Vars ) ->
	get_vars( Rest, [Name | Vars ] );
get_vars( [{_,and_cond, List} | Rest ], Vars ) ->
	N_Vars = get_vars( List, Vars ),
	get_vars( Rest, N_Vars );
get_vars( [{_,var_and_cond, {Name,List} } | Rest ], Vars ) ->
	N_Vars = get_vars( List, [Name | Vars] ),
	get_vars( Rest, N_Vars );
get_vars( [{_,or_cond, List} | Rest ], Vars ) ->
	N_Vars = get_vars( List, Vars ),
	get_vars( Rest, N_Vars );
get_vars( [{_,var_or_cond, {Name,List} } | Rest ], Vars ) ->
	N_Vars = get_vars( List, [Name | Vars] ),
	get_vars( Rest, N_Vars );
get_vars( [{_,multi_cond, List} | Rest ], Vars ) ->
	N_Vars = get_vars( List, Vars ),
	get_vars( Rest, N_Vars );
get_vars( [{_,var_multi_cond, {Name,List} } | Rest ], Vars ) ->
	N_Vars = get_vars( List, [Name | Vars] ),
	get_vars( Rest, N_Vars );
get_vars( [ _ | Rest ], Vars ) ->
	get_vars( Rest, Vars ).

get_unique_var( Var_Prefix, Arg_List, N ) ->
	Var = Var_Prefix ++ to_str( N ),
	case lists:keymember(Var, 1, Arg_List) of
		false ->
			Var;
		true ->
			get_unique_var( Var_Prefix, Arg_List, N + 1 )
	end.

to_str( Data ) ->
	try
		lists:flatten( io_lib:format( "~s", [Data] ))
	catch
		_C:_E ->
			lists:flatten( io_lib:format( "~p", [Data] ))
	end.

