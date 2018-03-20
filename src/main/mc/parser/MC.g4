/**
 * Student name: Lam Truong An
 * Student ID: 1570733
 */
grammar MC;
@lexer::header{
	package mc.parser;
}
@lexer::members{
@Override
public Token emit() {
    switch (getType()) {
    case UNCLOSE_STRING:       
        Token result = super.emit();
        // you'll need to define this method
        throw new UncloseString(result.getText());
        
    case ILLEGAL_ESCAPE:
    	result = super.emit();
    	throw new IllegalEscape(result.getText());

    case ERROR_CHAR:
    	result = super.emit();
    	throw new ErrorToken(result.getText());	

    default:
        return super.emit();
    }
}
}
@parser::header{
	package mc.parser;
}
options{
	language=Java;
}

//=================================================================================================
//RECONIZER
//=================================================================================================
program             : (varDecl | funcDecl)+ EOF ;

varDecl             : primitiveTypes varList SEMICOLON;
varList             : variable (COMMA variable)*;
variable            : ID | ID LSB INT_LIT RSB;

funcDecl            : types ID LB paraList? RB blockStmt;
paraList            : paraDecl (COMMA paraDecl)*;
paraDecl            : primitiveTypes ID (LSB RSB)?;

statement           : returnStmt | blockStmt | ifStmt | doWhileStmt
                        | forStmt | expStmt | breakStmt | continueStmt;
ifStmt              : IF LB expression RB statement (ELSE statement)?; //Sovled danglint-else by innermost if
doWhileStmt         : DO statement+ WHILE expression SEMICOLON;
forStmt             : FOR LB expression SEMICOLON expression SEMICOLON expression RB statement;
breakStmt           : BREAK SEMICOLON;
continueStmt        : CONTINUE SEMICOLON;
returnStmt          : RETURN expression? SEMICOLON;
expStmt             : expression SEMICOLON;
blockStmt           : LP varDecl* statement* RP;

types               : primitiveTypes (LSB RSB)? | VOID;
primitiveTypes      : INT | BOOLEAN | FLOAT |STRING;

expression          : expression1 ASSIGN_OP expression | expression1;
expression1         : expression1 OR_OP expression2 | expression2;
expression2         : expression2 AND_OP expression3 | expression3;
expression3         : expression4 (EQ_OP | NEQ_OP) expression4 | expression4 ;
expression4         : expression5 (LESS_OP | GREATER_OP | LESS_EQ_OP | GREATER_EQ_OP) expression5 | expression5;
expression5         : expression5 (ADD_OP | SUB_OP) expression6 | expression6;
expression6         : expression6 (MUL_OP | DIV_OP | MOD_OP) expression7 | expression7;
expression7         : (SUB_OP | NOT_OP) expression7 | expression8;
expression8         : expression9 LSB expression5 RSB | expression9; //index operator
expression9         : ID LB expList? RB | expression10; //function call
expression10        : LB expression RB | atomic;
atomic              : INT_LIT | FLT_LIT | STR_LIT | BOOL_LIT | ID;

expList             : expression (COMMA expression)*;

/**expression_temp     : LB expression RB
                     | expression LSB expression RSB  //index operator
                     | <assoc=right> (SUB_OP | NOT_OP) expression
                     | <assoc=left> expression (MUL_OP | DIV_OP | MOD_OP) expression
                     | <assoc=left> expression (ADD_OP | SUB_OP) expression
                     | expression (LESS_OP | GREATER_OP | LESS_EQ_OP | GREATER_EQ_OP) expression //Not Allow a < b < c, to be check in semantic phase
                     | expression (EQ_OP | NEQ_OP) expression //Not Allow a < b < c, to be check in semantic phase
                     | <assoc=left> expression AND_OP expression
                     | <assoc=left> expression OR_OP expression
                     | <assoc=right> expression ASSIGN_OP expression
                     | ID LB expList? RB //function call
                     | atomic;
*/
//=================================================================================================
//LEXER
//=================================================================================================

//Keywords
//-------------------------------------------------------------------
BOOLEAN			: 'boolean';
BREAK			: 'break';
CONTINUE		: 'continue';
ELSE			: 'else';
FOR				: 'for';
FLOAT			: 'float';
IF			 	: 'if';
INT				: 'int';
RETURN			: 'return';
VOID			: 'void';
DO			 	: 'do';
WHILE			: 'while';
STRING			: 'string';
//-------------------------------------------------------------------

//Operators
//-------------------------------------------------------------------
ADD_OP 			: '+';
SUB_OP          : '-';
MUL_OP 			: '*';
DIV_OP          : '/';
MOD_OP          : '%';
EQ_OP			: '==';
NEQ_OP          : '!=';
NOT_OP 			: '!';
OR_OP           : '||';
AND_OP          : '&&';
LESS_OP         : '<';
GREATER_OP      : '>';
LESS_EQ_OP      : '<=';
GREATER_EQ_OP   : '>=';
ASSIGN_OP		: '=';
//-------------------------------------------------------------------

//Separators
//-------------------------------------------------------------------
LB          	: '('; //brackets
RB          	: ')';
LP         		: '{'; //parentheses
RP          	: '}';
LSB         	: '['; //square brackets
RSB         	: ']';
SEMICOLON       : ';'; //semicolon
COMMA       	: ','; //comma
//-------------------------------------------------------------------

//fragment inline
//-------------------------------------------------------------------
fragment DECIMAL        : '.';
fragment DIGIT		    : [0-9];
fragment CHAR	        : [a-zA-Z_];
fragment EXPO		    : ('E'| 'e')('-')?;
fragment DB_QUOTE	    : '"';
fragment ESC_CHAR	    : '\\'[bfrnt\\"'];
fragment ILL_ESC_CHAR	: '\\'~[bfrnt\\"'];
fragment TRUE			: 'true';   //Keywords also
fragment FALSE			: 'false';  //Keywords also
//-------------------------------------------------------------------

//Literals
//-------------------------------------------------------------------
INT_LIT			: DIGIT+;
FLT_LIT			: DIGIT+ DECIMAL DIGIT* | DECIMAL DIGIT+ | (DIGIT* DECIMAL)? DIGIT+ EXPO DIGIT+;
BOOL_LIT		: TRUE | FALSE;
STR_LIT			: DB_QUOTE (ESC_CHAR | ~['"\\\r\n])* DB_QUOTE
                  {setText(getText().substring(1, getText().length()-1));};
//-------------------------------------------------------------------

//String Errors Handling
//-------------------------------------------------------------------
ILLEGAL_ESCAPE	: DB_QUOTE (ESC_CHAR | ILL_ESC_CHAR | ~['"\\\r\n])* DB_QUOTE
                  {setText(getText().substring(1, getText().length()-1));};
UNCLOSE_STRING	: DB_QUOTE (ESC_CHAR | ~['"\\\r\n])*
                  {setText(getText().substring(1, getText().length()));};

//Identifiers
//-------------------------------------------------------------------
ID              : CHAR (CHAR | DIGIT)*;
//-------------------------------------------------------------------

//Comments
//-------------------------------------------------------------------
LINE_COMMENT	: '//' .*? ('\n' | EOF) -> skip;
BLOCK_COMMENT	: '/*' .*?  '*/' -> skip;
//-------------------------------------------------------------------

//Characters set
//-------------------------------------------------------------------
WHITESPACE		: [ \t\r\n]+  -> skip; // skip spaces, tabs, newlines

//Errors Character Handling
//-------------------------------------------------------------------
ERROR_CHAR		: .;
//-------------------------------------------------------------------