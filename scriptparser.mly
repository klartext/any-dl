/*
  any-dl:
  -------
  Generic Media-Downloader for any kind of Online-Mediathek.

  Author / copyright: Oliver Bandel
  Copyleft:           GNU GENERAL PUBLIC LICENSE  v3 (or higher)
*/
%token LPAREN RPAREN

%token START END

%token IFNE   /* IF NOT EMPTY */
%token THEN
%token ELSE
%token ENDIF

%token DOT



%token QUOTE
%token COMMA
%token COLON
%token EQUALS

%token VBAR

%token PARSERNAME
%token DEF_MACRO
%token CALL_MACRO
%token MATCH
%token SHOW_MATCH
%token PRINT
%token SHOW_TYPE
%token CSV_SAVE_AS
%token CSV_SAVE
%token SAVE
%token SAVE_AS
%token LINKEXTRACT
%token LINKEXTRACT_XML
%token TAGEXTRACT
%token TITLEEXTRACT
%token REBASE

%token TAGSELECT
%token ANYTAG
%token DATA
%token DATA_SLURP
%token ARG
%token TAG
%token ARG_KEYS
%token ARG_VALS
%token ARG_PAIRS
%token HTML_STRING
%token DOCLIST

%token GET
%token DOWNLOAD
%token MAKE_URL
%token STORE
%token RECALL
%token DELETE
%token SORT
%token UNIQ
%token SHOW_VARIABLES
%token LIST_VARIABLES
%token PASTE
%token SYSTEM
%token SUBSTITUTE
%token QUOTE
%token TO_STRING
%token TO_MATCHRES
%token TRANSPOSE
%token BASENAME
%token EXITPARSE
%token DUMP
%token DUMP_DATA
%token SHOW_TAGS
%token SHOW_TAGS_FULLPATH
%token HTML_DECODE
%token URL_DECODE
%token READLINE
%token DUMMY
%token EMPTYDUMMY

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
%type <Parsetreetypes.lang_t> main


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
%}




%%
main: parsername urlmatches START parser_script END { Parserdef { parsername = $1; urllist = $2; statements = $4 } }
    | macrodef   /* args */ START parser_script END { Macrodef  ($1, $3) }
    | EOF        { (*print_stringlist_endlinehash variable_hash;*) raise End_of_file }
    ;

parsername: PARSERNAME STRING COLON { $2 }
    ;

macrodef:   DEF_MACRO  STRING COLON { $2 }
    ;

urlmatches: LPAREN string_list RPAREN { $2 }
    |                            { [] }
    ;

parser_script: statement_list { $1 }
    |                         { [] }
    ;




statement_list: command           { [ Command $1]     }
    | command statement_list      { (Command $1) :: $2 }
    | assignment                  { [ $1 ]   }
    | assignment statement_list   { $1 :: $2 }
    | conditional                  { [ $1 ]   }
    | conditional statement_list   { $1 :: $2 }
    ;

/*
command: simple_statement { $1 }
    ;
    | conditional                 { $1 }
    | conditional statement_list  { $1 }
*/

conditional: IFNE LPAREN statement_list RPAREN THEN statement_list                     ENDIF { Conditional ( $3,  $6, None ) }
    |        IFNE LPAREN statement_list RPAREN THEN statement_list ELSE statement_list ENDIF { Conditional ( $3, $6, Some $8 ) }
    ;

assignment: IDENTIFIER EQUALS command { Assignment ( $1 , $3 ) (* ($3, Store $1) *) }
    ;



command: command_base SEMI { $1 }
    ;

command_base: match_cmd         { $1 }
    /*
    */
    |      BASENAME             { Basename    }
    |      DUMMY                { Dummy       }
    |      EMPTYDUMMY           { Empty_dummy }
    |      DUMP                 { Dump        }
    |      DUMP_DATA            { Dump_data   }
    |      EXITPARSE            { Exit_parse  }
    |      HTML_DECODE          { Html_decode }
    |      LINKEXTRACT          { Link_extract  }
    |      LINKEXTRACT_XML      { Link_extract_xml }
    |      QUOTE                { Quote       }
    |      REBASE               { Rebase      }
    |      SAVE                 { Save        }
    |      SHOW_TAGS            { Show_tags   }
    |      SHOW_TAGS_FULLPATH   { Show_tags_fullpath }
    |      SHOW_TYPE            { Show_type   }
    |      SORT                 { Sort        }
    |      SYSTEM               { System      }
    |      TO_MATCHRES          { To_matchres }
    |      TO_STRING            { To_string   }
    |      TRANSPOSE            { Transpose   }
    |      UNIQ                 { Uniq        }
    |      URL_DECODE           { Url_decode  }
    |      call_macro           { $1 }
    |      csv_save             { $1 }
    |      csv_save_as          { $1 }
    |      delete_cmd           { $1 }
    |      download_cmd         { $1 }
    |      drop_cmd             { $1 }
    |      get_cmd              { $1 }
    |      list_variables_cmd   { $1 }
    |      make_url_cmd         { $1 }
    |      paste_cmd            { $1 }
    |      print_cmd            { $1 }
    |      print_cmd_simple     { Print  }
    |      readline             { $1 }
    |      recall_cmd           { $1 }
    |      save_as              { $1 }
    |      selection            { $1 }
    |      show_variables_cmd   { $1 }
    |      showmatch_cmd        { Show_match }
    |      store_cmd            { $1 }
    |      subst_cmd            { $1 }
    |      tagselect_cmd        { $1 }
    |      titleextract_cmd     { Title_extract }
    ;

call_macro: CALL_MACRO LPAREN STRING RPAREN { Call_macro $3 }
    ;

match_cmd: MATCH LPAREN STRING RPAREN { Match $3 }
    ;

print_cmd_simple: PRINT               { Print }
    ;

showmatch_cmd: SHOW_MATCH { Show_match }
    ;

print_cmd: PRINT_STRING  LPAREN STRING        RPAREN { Print_string $3 }
    |       PRINT        LPAREN argument_list RPAREN { Print_args $3   }
    ;

get_cmd: GET                           { Get }
    |    GET LPAREN get_args   RPAREN  { $3 }
    ;

download_cmd: DOWNLOAD                                { Download None }
    |         DOWNLOAD LPAREN argument_list   RPAREN  { Download (Some $3) }
    ;

store_cmd: STORE LPAREN STRING  RPAREN { Store $3 }
    ;

recall_cmd: RECALL LPAREN STRING   RPAREN    { Recall $3 }
    ;

delete_cmd: DELETE LPAREN STRING   RPAREN    { Delete $3 }
    ;

csv_save_as:    CSV_SAVE_AS  LPAREN  argument_list  RPAREN     { CSV_save_as ( $3 ) }
    ;

csv_save:    CSV_SAVE  { CSV_save        }
    ;

save_as:     SAVE_AS  LPAREN  argument_list  RPAREN     { Save_as $3 }
    ;

readline: READLINE LPAREN STRING RPAREN    { Readline (Some $3) }
    |     READLINE                         { Readline None      }
    ;

paste_cmd: PASTE LPAREN argument_list RPAREN    { Paste $3 }
    ;

subst_cmd: SUBSTITUTE LPAREN STRING COMMA STRING RPAREN { Subst( $3, $5 ) }
    ;

show_variables_cmd: SHOW_VARIABLES           { Show_variables }
    ;

list_variables_cmd: LIST_VARIABLES           { List_variables }
    ;

make_url_cmd: MAKE_URL LPAREN argument_item RPAREN                      { Make_url( $3, String "-") }
    |         MAKE_URL LPAREN argument_item COMMA argument_item RPAREN  { Make_url( $3, $5) }
    |         MAKE_URL                                                  { Make_url_tmpvar }
    ;



selection: COLSELECT LPAREN   INT_NUM   RPAREN { ColSelect $3 }
    |      ROWSELECT LPAREN   INT_NUM   RPAREN { RowSelect $3 }
    |      SELECT    LPAREN   INT_NUM   RPAREN { Select    $3 }
    |      MSELECT   LPAREN   selection_list   RPAREN { MSelect   $3 }
    |      SELECT_MATCH   LPAREN   INT_NUM COMMA STRING   RPAREN { Select_match ( $3, $5) }
    |      ISELECT_MATCH  LPAREN   INT_NUM COMMA STRING COMMA STRING  RPAREN { I_Select_match ( $3, $5, $7 ) }
    |      GREP      LPAREN   argument_list  RPAREN                      { Grep $3 }
    |      GREPV     LPAREN   argument_list  RPAREN                      { Grep_v $3 }
    ;

drop_cmd: DROPCOL LPAREN   INT_NUM   RPAREN { DropCol $3 }
    |     DROPROW LPAREN   INT_NUM   RPAREN { DropRow $3 }
    ;

titleextract_cmd: TITLEEXTRACT   { Title_extract }
    ;




tagselect_cmd: TAGSELECT LPAREN tag_selector VBAR extractor_list RPAREN { Tag_select( $3, Single_extr $5 )       }
    |          TAGSELECT LPAREN tag_selector VBAR ARG_PAIRS      RPAREN { Tag_select( $3, Pair_extr `Arg_pairs ) }
    |          TAGSELECT LPAREN tag_selector VBAR ARG_KEYS       RPAREN { Tag_select( $3, Pair_extr `Arg_keys )  }
    |          TAGSELECT LPAREN tag_selector VBAR ARG_VALS       RPAREN { Tag_select( $3, Pair_extr `Arg_vals )  }
    ;


tag_selector: ANYTAG {  Selector_any }
    | tagselect_arg_list { Specific_selector $1 }
    ;


tagselect_arg_list: tagselect_arg                            { [$1] }
    |               tagselect_arg  COMMA tagselect_arg_list  { $1 :: $3 }
    ;



tagselect_arg: tagname DOT argkey EQUALS argval      { { tag_sel = Some $1; argkey_sel = Some $3; argval_sel = Some $5 } }
    |          tagname DOT argkey                    { { tag_sel = Some $1; argkey_sel = Some $3; argval_sel = None    } }
    |          tagname DOT        EQUALS argval      { { tag_sel = Some $1; argkey_sel = None;    argval_sel = Some $4 } }
    |          tagname                               { { tag_sel = Some $1; argkey_sel = None;    argval_sel = None    } }
    |                  DOT argkey EQUALS argval      { { tag_sel = None;    argkey_sel = Some $2; argval_sel = Some $4 } }
    |                  DOT argkey                    { { tag_sel = None;    argkey_sel = Some $2; argval_sel = None    } }
    |                  DOT        EQUALS argval      { { tag_sel = None;    argkey_sel = None;    argval_sel = Some $3 } }
    ;

tagname: STRING { $1 };
argkey:  STRING { $1 };
argval:  STRING { $1 };




/* ---------------------------------------------- */
/* nextractor-list: non-empty list of extractor's */
/* ---------------------------------------------- */
extractor_list: extractor                   { [$1] }
    |           extractor COMMA extractor_list  { $1 :: $3 }

extractor: DATA                     { `Data }
    |      DATA_SLURP               { `Data_slurp  }
    |      TAG                      { `Tag }
    /*
    |      ARG_PAIRS                { `Arg_pairs }
    |      ARG_KEYS                 { `Arg_keys }
    |      ARG_VALS                 { `Arg_vals }
    */
    |      ARG LPAREN STRING RPAREN { `Arg $3 }
    |      DUMP                     { `Dump     }
    |      HTML_STRING              { `Html_string  }
    /*
    |      DOCLIST                  { `Doclist  }
    */
    ;




/* ------------------------------------------- */
/* hier unten sind die Typen noch native OCaml */
/* also keine eigenen Sum-Types!               */
/* ------------------------------------------- */

string_list:                   { []       }
    | STRING                   { [$1]     }
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
