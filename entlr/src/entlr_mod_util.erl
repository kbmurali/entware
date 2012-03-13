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
add_rule_funs( [ {Rule_Name, Tree} | Rest ], Template, Trim_Left ) ->
	Match_Fun_Form = create_rule_match_fun( Rule_Name, Tree  ),
	Rule_Fun_Form = create_rule_fun(Rule_Name, Tree, Trim_Left),
	N_Template = [Rule_Fun_Form | [Match_Fun_Form | Template] ],
	add_rule_funs( Rest, N_Template, Trim_Left ).
	
create_rule_match_fun_name( Rule_Name ) -> 
	string:to_lower( Rule_Name ) ++ "_match".

create_rule_fun_name( Rule_Name ) ->
	string:to_lower( Rule_Name ).

create_rule_fun( Rule_Name, Rule_Tree, Trim_Left ) ->
	Vars = get_vars( Rule_Tree, [] ),
	Trim_Left_Var = get_unique_var( "Trim_Left", Vars, 0 ),
	Input_Var = get_unique_var( "Input", Vars, 0 ),
	Match_Vars = get_unique_var( "Vars", Vars, 0 ),
	
	Fun_String = create_rule_fun_name(Rule_Name) ++ "( " ++ Input_Var ++ ") -> \n" ++
					 Trim_Left_Var ++ " = " ++ to_str( Trim_Left ) ++ ",\n" ++
					 "case catch " ++ create_rule_match_fun_name( Rule_Name ) ++ "( " ++ Input_Var ++ "," ++ Trim_Left_Var ++ ") of \n" ++ 
					 "{true, [], " ++ Match_Vars ++ "} -> \n",
	
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
	
	F_String2 = case lists:keyfind( code, 2, Rule_Tree ) of
					false ->
						F_String ++ "ok;\n";
					{_, _, Code } ->
						case Comma of
							true ->
								F_String ++ ",\n" ++ Code ++ ";\n";
							false ->
								F_String ++ Code ++ ";\n"
						end
				end,
					
	
	Fun_String2 = F_String2 ++
					  "{rule_executed, _} ->\n" ++
					  "ok;\n" ++
					  "false -> \n" ++
					  "throw( {error, invalid_input," ++  Input_Var ++ "} )" ++
					  "end.",
	{ok, FT, _} = erl_scan:string( Fun_String2 ),
	{ok, FF} = erl_parse:parse_form( FT ),
	FF.
	

create_rule_match_fun( Rule_Name, Rule_Tree ) ->
	Tree = lists:keydelete(code, 2, Rule_Tree),
	Fun_String = create_rule_match_fun_name( Rule_Name ) ++ "( Input, Trim_Left ) -> \n" ++
					 "Rules_Tree = " ++ to_str( Tree ) ++ "," ++
					 "process( Rules_Tree, Input, Trim_Left, [], Input, true ).",
	
	{ok, FT, _} = erl_scan:string( Fun_String ),
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
get_vars( [{_,and_cond, List} | Rest ], Vars ) ->
	N_Vars = get_vars( List, Vars ),
	get_vars( Rest, N_Vars );
get_vars( [{_,or_cond, List} | Rest ], Vars ) ->
	N_Vars = get_vars( List, Vars ),
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

