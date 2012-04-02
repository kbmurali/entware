grammar rtes_rules_grammar;

process_rule: RULE NAME
				FACTS (Fact=FACT | COMMA)*
				WHEN
					Property1=(FACT.PROPERTY OPERATOR VALUE) | Properties=( (AND | OR) FACT.PROPERTY OPERATOR VALUE )*
				THEN
					ACTIONS
				END;
				
RULE: 'rule';

NAME: ALPHABET+DIGIT+;

FACTS: 'facts'

FACT: ALPHABET+DIGIT+;

DIGIT: 
		('0'..'9')
	 ;
	 
ALPHABET: 
		('a'..'z''A'..'Z')
		; 

COMMA : ',';