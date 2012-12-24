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
%token MATCH
%token URLMATCHES
%token PRINT_MATCH
%token DUMMY


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
%type <string> main


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


            /*
            parser "ard_mediathek_get":
            urlmatches ("http://www.ardmediathek.de", "http://mediathek.daserste.de");
            start
              Match "(rtmpt{0,1}://[^\"]+).*?(mp4:[^\"]+)\"";
              Print_match;

              #Showvar;
              #Select (fun x -> [|x.(0)|]);  (* muesste andere Syntax sein für das externe Script *)
              #Print;
              #Showvar;
              #Setvar( Url_list [("http://www.first.in-berlin.de", "-"); ("http://www.google.de", "")]);
              #Showvar;
              #Get_urls;
              Dummy
            end
            */




%%
main: parsername urlmatches START parser_script END { $1 }
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

statement: match_stmt { $1 }
    |      printmatch_stmt { $1 }
    |      DUMMY SEMI      { "" }
    ;

match_stmt: MATCH STRING SEMI { $2 }
    ;

printmatch_stmt: PRINT_MATCH SEMI { "" }
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






%%
