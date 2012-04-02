%% Author: kbmurali
%% Created: Mar 30, 2012
%% Description:
-module(db_table_wrapper_util).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([to_wrapper/1]).

%%
%% API Functions
%%
to_wrapper( {Name, List} ) when is_list( List ) == true ->
	Mod = create_mod( Name, List, [], [], [] ),
	
	Filename = parsing_util:to_str( Name ) ++ ".erl",
	{ok,Dev} = file:open( Filename, [write] ),
	io:put_chars( Dev, erl_prettypr:format(erl_syntax:form_list(Mod)) ),
	file:close(Dev).

create_mod( Name, [], Exports, Getters, Setters ) ->
	N_Exports = lists:append([export_new_form(), export_new_form_1() ], lists:reverse(Exports) ),
	N_Getters = lists:reverse(Getters),
	N_Setters = lists:reverse(Setters),
	Funs_1 = lists:append( [new_func_form( Name ), new_func_form_2( Name )], N_Getters),
	Funs_2 = lists:append( Funs_1, N_Setters ),
	Mod_Entries = lists:append( N_Exports, Funs_2 ),
	lists:append( [mod_form( Name )], Mod_Entries );
create_mod( Name, [ {Prop_Name, _} | Rest ], Exports, Getters, Setters ) ->
	N_Exports = [ export_setter_form( Prop_Name ) | [export_getter_form( Prop_Name ) | Exports] ],
	N_Getters = [ prop_getter_form( Name, Prop_Name ) | Getters ],
	N_Setters = [ prop_setter_form( Name, Prop_Name) | Setters ],
	create_mod( Name, Rest, N_Exports, N_Getters, N_Setters ).
	
mod_form( Name ) ->
	String = "-module(" ++ parsing_util:to_str( Name ) ++ ").",
	to_form( String ).

export_new_form() ->
	String ="-export( [new/0] ).",
	to_form( String ).

export_new_form_1() ->
	String ="-export( [new/1] ).",
	to_form( String ).

export_getter_form( Prop_Name ) ->
	String ="-export( [ " ++ parsing_util:to_str( Prop_Name ) ++ "/1] ).",
	to_form( String ).

export_setter_form( Prop_Name ) ->
	String ="-export( [ " ++ parsing_util:to_str( Prop_Name ) ++ "/2] ).",
	to_form( String ).

new_func_form( Name ) ->
	String ="new() -> { " ++ parsing_util:to_str( Name ) ++ ", [] }.",
	to_form( String ).

new_func_form_2( Name ) ->
	String ="new( TupleList) when is_list(TupleList) == true -> { " ++ parsing_util:to_str( Name ) ++ ", TupleList }.",
	to_form( String ).

prop_getter_form( Name, Prop_Name ) ->
	String = parsing_util:to_str( Prop_Name ) ++ "( { " ++ parsing_util:to_str( Name ) ++ ", TupleList } ) when is_list( TupleList) == true ->\n" ++
					 "{_, Value} = lists:keyfind(" ++ parsing_util:to_str( Prop_Name ) ++ ", 1, TupleList),\n" ++
					 "Value.",
	to_form( String ).

prop_setter_form( Name, Prop_Name ) ->
	Prop_Name_Str =  parsing_util:to_str( Prop_Name ),
	Record_Name = parsing_util:to_str( Name ),
	
	String = Prop_Name_Str ++ "( { " ++ Record_Name ++ ", TupleList }, Value ) when is_list( TupleList) == true ->\n" ++
				 "N_List = case lists:keyfind(" ++ Prop_Name_Str ++ ", 1, TupleList) of\n" ++
				 				"false ->\n" ++
									"[ {" ++  Prop_Name_Str ++ ", Value} | TupleList ];\n" ++
				 				"_ ->\n" ++
									"lists:keyreplace(" ++ Prop_Name_Str ++ ", 1, TupleList,  {" ++ Prop_Name_Str ++ ", Value} )\n" ++
			 			  "end,\n" ++
				 "{" ++ Record_Name ++ ", N_List }.",
	to_form( String ).
	

to_form( String ) ->
	{ok, Term, _} = erl_scan:string( String ),
	{ok, Form} = erl_parse:parse_form( Term ),
	Form.
	
