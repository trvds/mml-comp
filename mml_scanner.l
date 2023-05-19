%option c++ prefix="mml_scanner_" outfile="mml_scanner.cpp"
%option stack noyywrap yylineno 8bit debug
%{ 
// make relevant includes before including the parser's tab file
#include <string>
#include <cdk/ast/sequence_node.h>
#include <cdk/ast/expression_node.h>
#include <cdk/ast/lvalue_node.h>
#include "mml_parser.tab.h"

// don't change this
#define yyerror LexerError
%}
%x X_STRING X_COMMENT

%%
  yydebug=1; set_debug(1);

"--".*                 ; /* ignore comments */


  /* ====================================================================== */
  /* ====[                 3.2 - Comentários                          ]==== */
  /* ====================================================================== */

[ ]*"..."\n            ;         

"//".*\n                { yyless(yyleng - 1); }

"/*"                   yy_push_state(X_COMMENT);
<X_COMMENT>"/*"        yy_push_state(X_COMMENT);
<X_COMMENT>"*/"        yy_pop_state();
<X_COMMENT>.|\n        ; /* ignore content */


  /* ====================================================================== */
  /* ====[                 3.3.2 - Declarações                        ]==== */
  /* ====[                 3.5 - Tipos                                ]==== */
  /* ====[                 3.8.4 - Ponteiros                            ]==== */
  /* ====================================================================== */

"foreign"              return tFOREIGN;
"forward"              return tFORWARD;
"public"               return tPUBLIC;
"auto"                 return tAUTO;

"int"                  return tINT_TYPE;
"double"               return tDOUBLE_TYPE;
"string"               return tSTRING_TYPE;
"void"                 return tVOID_TYPE;
[\[\]]                 return *yytext;

"null"                 return tNULLPTR;

  /* ====================================================================== */
  /* ====[                 5 - Funções                                ]==== */
  /* ====================================================================== */

[():@,]                return *yytext;
"->"                   return tIOTYPES;

"begin"                return tBEGIN; 
"end"                  return tEND; 


  /* ====================================================================== */
  /* ====[                 6.2 - Instrução condicional                ]==== */
  /* ====[                 6.3 - Instrução de iteração                ]==== */
  /* ====[                 6.4 - Instrução de terminação              ]==== */
  /* ====[                 6.5 - Instrução de continuação             ]==== */
  /* ====[                 6.6 - Instrução de retorno                 ]==== */
  /* ====[                 6.8 - Instrução de Impressão               ]==== */
  /* ====================================================================== */

"if"                   return tIF; 
"else"                 return tELSE; 
"elif"                 return tELIF;

"while"                return tWHILE; 

"stop"                 return tSTOP;

"next"                 return tNEXT;

"return"               return tRETURN;

"!"                    return tPRINT; 
"!!"                   return tPRINTLN; 


  /* ====================================================================== */
  /* ====[                 7 - Expressões                             ]==== */
  /* ====================================================================== */

[\[\]()?,]             return *yytext;
[-+*/%<>?=]            return *yytext;


">="                   return tGE; 
"<="                   return tLE; 
"=="                   return tEQ; 
"!="                   return tNE; 
"~"                    return tNOT;
"&&"                   return tAND;
"||"                   return tOR;
"sizeof"               return tSIZEOF;
"read"                 return tREAD; 



  /* ====================================================================== */
  /* ====[                 3.8.1 - Inteiros                           ]==== */
  /* ====================================================================== */

  /* ====================================================================== */
  /* ====[                 3.8.2 - Reais                              ]==== */
  /* ====================================================================== */

  /* ====================================================================== */
  /* ====[                 3.8.1 - Strings                            ]==== */
  /* ====================================================================== */




  

[A-Za-z][A-Za-z0-9_]*  yylval.s = new std::string(yytext); return tIDENTIFIER;

\'                     yy_push_state(X_STRING); yylval.s = new std::string("");
<X_STRING>\'           yy_pop_state(); return tSTRING;
<X_STRING>\\\'         *yylval.s += yytext + 1;
<X_STRING>.            *yylval.s += yytext;
<X_STRING>\n           yyerror("newline in string");

[0-9]+                 yylval.i = strtol(yytext, nullptr, 10); return tINTEGER;

[-()<>=+*/%;{}.]       return *yytext;

[ \t\n]+               ; /* ignore whitespace */

.                      yyerror("Unknown character");

%%
