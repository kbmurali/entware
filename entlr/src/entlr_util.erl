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

%%
%% API Functions
%%
gen_mod( file, File ) ->
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
