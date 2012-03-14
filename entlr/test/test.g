grammar test123;

rule1: (script1 | script2)*;

scripts: script1 | script2;

script1: RULE DName=NAME { io:format( "This is generated script1 output: ~p~n", [DName] ) };

script2: TEST Digit=DIGIT DName=NAME { io:format( "This is gen script2 output: ~p, ~p~n", [Digit, DName] ) };

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

