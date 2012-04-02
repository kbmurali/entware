%% Author: kbmurali
%% Created: Apr 1, 2012
%% Description: TODO: Add description to mysql_adapter
-module(mysql_adapter).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([get_all_tables/1]).
-export([get_all_columns/2]).

%%
%% API Functions
%%
get_all_tables( Pool_Name ) ->
	case mysql:fetch( Pool_Name, <<"Show tables">> ) of
		{data,{mysql_result,[{<<"TABLE_NAMES">>,_,_,'VAR_STRING'}],Tables_List, _,_,_}} ->
			[ binary_to_atom( Table_Name_Bin, utf8) || [Table_Name_Bin] <- Tables_List ];
		R ->
			throw( {error, R} )
	end.

get_all_columns( Pool_Name, Table_Name ) ->
	Query = "SHOW COLUMNS FROM " ++ to_str( Table_Name ),
	Q_Bin = erlang:list_to_binary( Query ),
	
	case mysql:fetch( Pool_Name, Q_Bin ) of
		{data,{mysql_result,_, _Cols_List, _,_,_}} ->
			ok;
		R ->
			throw( {error, R} )
	end.

to_str( Data ) ->
	try
		lists:flatten( io_lib:format( "~s", [Data] ))
	catch
		_C:_E ->
			lists:flatten( io_lib:format( "~p", [Data] ))
	end.