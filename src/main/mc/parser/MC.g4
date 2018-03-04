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
program             : (INT | VOID) 'main'  LB RB LP body? RP EOF ;
body                : varDecl+ | funcDecl+ | funcall;

varDecl             : primitiveTypes variables SEMICOLON;
variables           : variable (COMMA variable)*;
variable            : ID | ID LSB INT_LIT RSB;

funcDecl            : types ID LB paraList? RB blockStmt;
paraList            : paraDecl (COMMA paraDecl)*;
paraDecl            : primitiveTypes ID (LSB RSB)?;

exp                 : funcall | INT_LIT ;
funcall             : ID LB exp? RB SEMICOLON;

returnStmt          : RETURN exp SEMICOLON;
blockStmt           : LP RP;
types               : primitiveTypes | arrayPoinerTypes | VOID;
primitiveTypes      : INT | BOOLEAN | FLOAT |STRING;
arrayPoinerTypes    : primitiveTypes LSB RSB;
arrayTypes          : primitiveTypes ID LSB RSB;

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
STR_LIT			: DB_QUOTE (ESC_CHAR | ~['"\\\r\n])* DB_QUOTE;
//-------------------------------------------------------------------
//String Errors Handling
//-------------------------------------
ILLEGAL_ESCAPE	: DB_QUOTE (ESC_CHAR | ILL_ESC_CHAR | ~['"\\\r\n])* DB_QUOTE;
UNCLOSE_STRING	: DB_QUOTE (ESC_CHAR | ~['"\\\r\n])*;

//Identifiers
//-------------------------------------------------------------------
ID              : CHAR (CHAR | DIGIT)*;
//-------------------------------------------------------------------

//Comments
//-------------------------------------------------------------------
BLOCK_COMMENT	: '/*' .*?  '*/' -> skip;
LINE_COMMENT	: '//' (~[\n])* ('\n' | EOF) -> skip;
//-------------------------------------------------------------------

//Characters set
//-------------------------------------
WHITESPACE		: [ \f\t\r\n]+  -> skip; // skip spaces, tabs, newlines

//Errors Handling
//-------------------------------------------------------------------
ERROR_CHAR		: .;
//-------------------------------------------------------------------