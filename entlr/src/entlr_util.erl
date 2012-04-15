%%% -------------------------------------------------------------------
%%% Author  : Murali Kashaboina
%%% -------------------------------------------------------------------
-module(entlr_util).

-define( CURLY1, "CURLY###ERLANG####BRACKET1").
-define( CURLY2, "CURLY###ERLANG####BRACKET2").

%%
%% Exported Functions
%%
-export([match/2]).
-export([to_re/2]).
-export([gen_mod/2]).
-export([gen_mod_form/2]).
-export([parse_by_line_feed/6]).

%%
%% API Functions
%%
gen_mod( file, File ) ->
	Form = gen_mod_form( file, File ),
	{ok, Mod, Bin} = compile:forms( Form ),
	code:load_binary( Mod, "nofile", Bin ),
	Mod.

gen_mod_form( file, File ) ->
	{Grammar_Name, Rules, Tokens } = entlr_grammar_parser:get_entries(file, File ),
	Fun = fun( {Rule_Name, Rule_Spec}, Acc ) ->
				  {ok, Rule_Tokens} = entlr_grammar_parser:parse_rule( Rule_Spec, [], unknown, [], [] ),
				  {ok, Resolved_Tokens } = entlr_grammar_parser:resolve_rule_tokens( Rule_Tokens, Tokens, [] ),
				  [ {Rule_Name, Resolved_Tokens} | Acc ]
		  end,
	Resolved_Rules = lists:foldr(Fun, [], Rules ),
	entlr_mod_util:create_mod_form( Grammar_Name, Resolved_Rules, true ).

match( Input, RE ) ->
	Len = length( Input ),
	case re:run(Input, RE) of
		{match, [{0,Len}] } ->
			true;
		_ ->
			false
	end.

to_re( Input, Shorten_WS ) ->
	Replace_Ts = [ { "'\\.'", "\\\\." },
				   { "'\\|'", "\\\\|" },
				   { "'\\('", ?CURLY1 },
				   { "'\\)'", ?CURLY2 },
				   { "'\\['", "\\\\[" },
				   { "'\\]'", "\\\\]" },
				   { "'\\+'", "\\\\+" },
				   { "'\\?'", "\\\\?" },
				   { "'\\*'", "\\\\*" },
				   { "'\\-'", "\\\\-" },
				   { "\\(", "[" },
				   { "\\)", "]" },
				   { "[\\.]{2}", "-" },
				   { ?CURLY1, "\\(" },
				   { ?CURLY2, "\\)" },
				   { "'", "" } ],
	
	T_Inp = string:strip( Input, both),
	R_String = replace( Replace_Ts, T_Inp ),
	
	case Shorten_WS of
		true ->
			replace( [ {"\\s+", " "}], R_String );
		false ->
			R_String
	end.
	
				   
	
replace( [], R ) ->
	R;
replace( [ {From, To} | Rest], Input ) ->
	R = re:replace( Input, From, To, [global, {return, list}] ),
	replace( Rest, R ).

parse_by_line_feed( file, Filename, Mod, Func, Ignore_Header, none ) ->
	Interceptor_Fun = fun default_line_feed_interceptor/1,
	
	parse_by_line_feed( file, Filename, Mod, Func, Ignore_Header, Interceptor_Fun );
parse_by_line_feed( file, Filename, Mod, Func, Ignore_Header, Interceptor_Fun ) ->
	{ok, Device} = file:open(Filename, [read] ),
	Result = parse_by_line_feed( iodevice, Device, Mod, Func, Ignore_Header, Interceptor_Fun ),
	file:close( Device ),
	Result;
parse_by_line_feed( iodevice, Device, Mod, Func, Ignore_Header, none ) ->
	Interceptor_Fun = fun default_line_feed_interceptor/1,
	parse_by_line_feed( iodevice, Device, Mod, Func, Ignore_Header, Interceptor_Fun );
parse_by_line_feed( iodevice, Device, Mod, Func, Ignore_Header, Interceptor_Fun ) ->
	case file:read_line( Device ) of
		eof ->
			ok;
		{ok, Line} ->
			case Ignore_Header of
				true ->
					ok;
				false ->
					F_Line = Interceptor_Fun( Line ),
					Mod:Func( F_Line )
			end,
			parse_by_line_feed( iodevice, Device, Mod, Func, false, Interceptor_Fun )
	end.

default_line_feed_interceptor( Line ) ->
	re:replace(Line, "\n$", "", [ {return, list }, global ] ).