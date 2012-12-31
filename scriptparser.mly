%token LPAREN RPAREN

%token START END

%token IF
%token THEN
%token ELSE
%token ENDIF

%token DOT



%token QUOTE
%token COMMA
%token COLON
%token EQUALS


%token PARSERNAME
%token URLMATCHES
%token MATCH
%token SHOW_MATCH
%token PRINT
%token SHOWTYPE
%token LINKEXTRACT
%token LINKEXTRACT_XML
%token GET
%token STO
%token RCL
%token DUMMY

%token COLSELECT
%token ROWSELECT
%token SELECT

%token PRINT_STRING



%token <string> IDENTIFIER
%token <int>    INT_NUM
%token <string> STRING
%token SEMI

%token EOF
%token EOL
%token NOT
%token AND OR
%token GT ST


/* PRIORITIES */
/* ---------- */

%left NOT
%left AND OR
%left GT ST
%left PLUS MINUS
%left MULT DIV



%start main
%type <Parsetreetypes.parserdef_t> main


/* nun die Definitionen der Typen fuer non-terminals */
/* ------------------------------------------------- */
/*
%type <string list> identifier_list
%type <string list> string_list
*/
/* ---------------------------> 
 <--------------------------- */


%{
open Parsetreetypes
open Nethtml
exception Undefined_value of string


%}




%%
main: parsername urlmatches START parser_script END { { parsername = $1; urllist = $2; commands= $4 } }
    | EOF        { (*print_stringlist_endlinehash variable_hash;*) raise End_of_file }
    ;

parsername: PARSERNAME STRING COLON { $2 }
    ;

urlmatches: LPAREN string_list RPAREN { $2 }
    ;

parser_script: statement_list { $1 }
    ;




statement_list:                { []      }
    | statement                { [$1]    }
    | statement statement_list { $1 :: $2 }
    ;

statement: match_stmt           { $1 }
    |      print_stmt           { Print  }
    |      printmatch_stmt      { Show_match }
    |      print_string         { $1 }
    |      selection            { $1 }
    |      get_stmt         SEMI   { $1 }
    |      LINKEXTRACT      SEMI   { Link_extract }
    |      LINKEXTRACT_XML  SEMI   { Link_extract_xml }
    |      SHOWTYPE         SEMI   { Showtype }
    |      DUMMY            SEMI   { Dummy }
    ;

match_stmt: MATCH LPAREN STRING RPAREN SEMI { Match $3 }
    ;

print_stmt: PRINT               SEMI  { Print }
    ;

printmatch_stmt: SHOW_MATCH SEMI { Show_match }
    ;

print_string: PRINT_STRING LPAREN STRING RPAREN SEMI { Print_string $3 }
    ;

get_stmt: GET                           { Get }
    |     GET LPAREN get_args   RPAREN  { $3 }
    ;

sto_stmt: STO LPAREN STRING   RPAREN    { $3 }
    ;

rcl_stmt: RCL LPAREN STRING   RPAREN    { $3 }
    ;




selection: COLSELECT LPAREN   INT_NUM   RPAREN SEMI { ColSelect $3 }
    |      ROWSELECT LPAREN   INT_NUM   RPAREN SEMI { RowSelect $3 }
    |      SELECT    LPAREN   selection_list   RPAREN SEMI { Select    $3 }
    ;


/*

statement: statement_base SEMI { $1 } /* block ist eigentlich eine falsche bezeichnung MISNAMED */
    /*
    | error SEMI { prerr_string "Parse-Error! please try again! line: " }
    */
    ;


param_def: IDENTIFIER COLON IDENTIFIER{ ( $1, $3 ) }
    ;

    /*
condition: IF expression THEN statement_base ELSE statement_base ENDIF { Condition($2, $4, Some $6) }
    | IF expression THEN statement_base ENDIF { Condition_s( $2, $4 ) }
    ;


assignment:  IDENTIFIER EQUALS expression { Assignment( $1, $3) }
    ;

*/



/* ------------------------------------------- */
/* hier unten sind die Typen noch native OCaml */
/* also keine eigenen Sum-Types!               */
/* ------------------------------------------- */

/*
identifier_list:            { [] (* empty list allowed *) }
    | IDENTIFIER COMMA identifier_list { $1 :: $3 }
    |            IDENTIFIER { [$1] }
    ;

identifier_list: IDENTIFIER { [$1] }
    | IDENTIFIER COMMA identifier_list { $1 :: $3 }
    ;
*/

string_list:                { [] (* eigentlich quatsch; macht leere Liste Sinn fuer development phase of a parser? *)     }
    | STRING                { [$1]    }
    | STRING COMMA string_list { $1 :: $3 }
    ;


selection_list: INT_NUM                        { [ $1 ] }
    |           INT_NUM COMMA selection_list   {  $1 :: $3 }
    ;



get_args:  STRING               { Get_url ($1, "") }
    |      STRING COMMA STRING  { Get_url ($1, $3) }
    ;

%%
