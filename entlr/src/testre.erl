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
		
		 
get_func_spec( Mod, Func ) ->
	case entlr_util:get_mod_spec(Mod) of
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