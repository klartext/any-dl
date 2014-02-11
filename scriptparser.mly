/*
  any-dl:
  -------
  Generic Media-Downloader for any kind of Online-Mediathek.

  Author / copyright: Oliver Bandel
  Copyleft:           GNU GENERAL PUBLIC LICENSE  v3 (or higher)
*/
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


%token ANY
%token PARSERNAME
%token MATCH
%token SHOW_MATCH
%token PRINT
%token SHOW_TYPE
%token SAVE
%token LINKEXTRACT
%token LINKEXTRACT_XML
%token TAGEXTRACT
%token TITLEEXTRACT
%token GET
%token MAKE_URL
%token STORE
%token RECALL
%token DELETE
%token SHOW_VARIABLES
%token LIST_VARIABLES
%token PASTE
%token SYSTEM
%token SUBSTITUTE
%token QUOTE
%token TO_STRING
%token TO_MATCHRES
%token BASENAME
%token EXITPARSE
%token DUMP
%token DUMP_DATA
%token SHOW_TAGS
%token SHOW_TAGS_FULLPATH
%token DUMMY

%token COLSELECT
%token ROWSELECT

%token DROPCOL
%token DROPROW

%token GREP
%token GREPV
%token SELECT
%token MSELECT
%token SELECT_MATCH
%token ISELECT_MATCH

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
    |                            { [] }
    ;

parser_script: statement_list { $1 }
    ;




statement_list: statement         { [$1]    }
    | statement statement_list    { $1 :: $2 }
    ;

statement: match_stmt          SEMI   { $1 }
    |      print_stmt_simple   SEMI   { Print  }
    |      showmatch_stmt      SEMI   { Show_match }
    |      print_stmt          SEMI   { $1 }
    |      EXITPARSE           SEMI   { Exit_parse }
    |      selection           SEMI   { $1 }
    |      drop                SEMI   { $1 }
    |      get_stmt            SEMI   { $1 }
    |      make_url_stmt       SEMI   { $1 }
    |      store_stmt          SEMI   { $1 }
    |      recall_stmt         SEMI   { $1 }
    |      delete_stmt         SEMI   { $1 }
    |      paste_stmt          SEMI   { $1 }
    |      show_variables_stmt SEMI   { $1 }
    |      list_variables_stmt SEMI   { $1 }
    |      subst_stmt          SEMI   { $1 }
    |      QUOTE               SEMI   { Quote     }
    |      TO_STRING           SEMI   { To_string }
    |      TO_MATCHRES         SEMI   { To_matchres }
    |      BASENAME            SEMI   { Basename }
    |      SYSTEM              SEMI   { System }
    |      LINKEXTRACT         SEMI   { Link_extract }
    |      LINKEXTRACT_XML     SEMI   { Link_extract_xml }
    |      titleextract_stmt   SEMI   { Title_extract }
    |      SHOW_TYPE           SEMI   { Show_type }
    |      SAVE                SEMI   { Save }
    |      DUMP                SEMI   { Dump }
    |      DUMP_DATA           SEMI   { Dump_data }
    |      SHOW_TAGS           SEMI   { Show_tags }
    |      SHOW_TAGS_FULLPATH  SEMI   { Show_tags_fullpath }
    |      DUMMY               SEMI   { Dummy }
    ;

match_stmt: MATCH LPAREN STRING RPAREN { Match $3 }
    ;

print_stmt_simple: PRINT               { Print }
    ;

showmatch_stmt: SHOW_MATCH { Show_match }
    ;

print_stmt: PRINT_STRING LPAREN STRING        RPAREN { Print_string $3 }
    |       PRINT        LPAREN argument_list RPAREN { Print_args $3   }
    ;

get_stmt: GET                           { Get }
    |     GET LPAREN get_args   RPAREN  { $3 }
    ;

store_stmt: STORE LPAREN STRING  RPAREN { Store $3 }
    ;

recall_stmt: RECALL LPAREN STRING   RPAREN    { Recall $3 }
    ;

delete_stmt: DELETE LPAREN STRING   RPAREN    { Delete $3 }
    ;

paste_stmt: PASTE LPAREN argument_list RPAREN    { Paste $3 }
    ;

subst_stmt: SUBSTITUTE LPAREN STRING COMMA STRING RPAREN { Subst( $3, $5 ) }
    ;

show_variables_stmt: SHOW_VARIABLES           { Show_variables }
    ;

list_variables_stmt: LIST_VARIABLES           { List_variables }
    ;

make_url_stmt: MAKE_URL LPAREN argument_item RPAREN             { Make_url( $3, String "-") }
    | MAKE_URL LPAREN argument_item COMMA argument_item RPAREN  { Make_url( $3, $5) }
    | MAKE_URL                                                  { Make_url_tmpvar }
    ;



selection: COLSELECT LPAREN   INT_NUM   RPAREN { ColSelect $3 }
    |      ROWSELECT LPAREN   INT_NUM   RPAREN { RowSelect $3 }
    |      SELECT    LPAREN   INT_NUM   RPAREN { Select    $3 }
    |      MSELECT   LPAREN   selection_list   RPAREN { MSelect   $3 }
    |      SELECT_MATCH   LPAREN   INT_NUM COMMA STRING   RPAREN { Select_match ( $3, $5) }
    |      ISELECT_MATCH  LPAREN   INT_NUM COMMA STRING   RPAREN { I_Select_match ( $3, $5) }
    |      GREP      LPAREN   STRING RPAREN                      { Grep $3 }
    |      GREPV     LPAREN   STRING RPAREN                      { Grep_v $3 }
    ;

drop: DROPCOL LPAREN   INT_NUM   RPAREN { DropCol $3 }
    | DROPROW LPAREN   INT_NUM   RPAREN { DropRow $3 }
    ;

titleextract_stmt: TITLEEXTRACT   { Title_extract }
    ;


/* ------------------------------------------- */
/* hier unten sind die Typen noch native OCaml */
/* also keine eigenen Sum-Types!               */
/* ------------------------------------------- */

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



argument_list:                      { [] }
    | argument_item                    { [$1] }
    | argument_item  COMMA argument_list  { $1 :: $3 }
    ;


argument_item:    IDENTIFIER    { Varname $1 }
    |             STRING        { String  $1 }
    ;


%%
