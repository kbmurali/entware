%%% -------------------------------------------------------------------
%%% Author  : Murali Kashaboina
%%% -------------------------------------------------------------------
-module(entlr_util).

-define( CURLY1, "CURLY###ERLANG####BRACKET1").
-define( CURLY2, "CURLY###ERLANG####BRACKET2").

%%
%% Exported Functions
%%
-export([get_mod_spec/1]).
-export([match/2]).
-export([to_re/1]).
-export([create_token_validation_fun/1]).

%%
%% API Functions
%%
get_mod_spec( Mod ) ->
	File = lists:concat( [Mod, ".beam" ] ),
	Path = code:where_is_file( File ),
	case beam_lib:chunks( Path, [abstract_code] ) of
		{ok, {Mod, [ {abstract_code, { _, Form}} ]} } ->
			{ok, Form};
		Error ->
			{error, Error }
	end.

match( Input, RE ) ->
	Len = length( Input ),
	case re:run(Input, RE) of
		{match, [{0,Len}] } ->
			true;
		_ ->
			false
	end.

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
				   { "'", "" } ],
	
	Replaced_Token = replace( Replace_Ts, Input ),
	
	case Input of
		Replaced_Token ->
			Replaced_Token ++ "{1}";
		_ ->
			"[" ++ Replaced_Token ++ "]{1}"
	end.
	


replace( [], R ) ->
	R;
replace( [ {From, To} | Rest], Input ) ->
	R = re:replace( Input, From, To, [global, {return, list}] ),
	replace( Rest, R ).

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
							[{clause,R,[{atom,R,true}],[],[{atom,S,true}]},
							 {clause,T,[{var,T,'_'}],[],[{atom,T,false}]}]}]}]}},

	{value, Fun, _ } = erl_eval:expr( G_Fun, [] ),
	Fun.