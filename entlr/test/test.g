grammar test123;

rule1: DValue=(script1 | script2)* { 
										[ DValue1 | _ ] = DValue,
										io:format( "Rule1 output: ~n ~p~n", [DValue1] )
									};

scripts: script1 | script2;

script1: RULE DName=NAME { io:format( "Script1 output: ~p~n", [DName] ) };

script2: TEST Digit=DIGIT DName=NAME { io:format( "Script2 output: ~p, ~p~n", [Digit, DName] ) };

RULE : 'rule';

TEST : 'test';

NAME : ALPHABET+DIGIT+;

DIGIT: 
		('0'..'9')
	 ;
	 
ALPHABET: 
		('a'..'z''A'..'Z')
		;
		
SYMBOL:
		';'
		;

