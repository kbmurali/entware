grammar test123;

script: RULE NAME { A = 100; };

RULE : 'rule';

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

