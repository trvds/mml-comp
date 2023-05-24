%option c++ prefix="mml_scanner_" outfile="mml_scanner.cpp"
%option stack noyywrap yylineno 8bit debug

%{ 
// make relevant includes before including the parser's tab file
#include <string>
#include <sstream>
#include <cdk/ast/sequence_node.h>
#include <cdk/ast/expression_node.h>
#include <cdk/ast/lvalue_node.h>
#include "mml_parser.tab.h"

// don't change this
#define yyerror LexerError
%}

EXP [eE]-?[:digit:]+

%x X_STRING X_COMMENT X_BACKSLASH

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
"input"                return tINPUT;
"null"                 return tNULLPTR;

  /* ====================================================================== */
  /* ====[                 3.8.1 - Inteiros                           ]==== */
  /* ====================================================================== */

0[0-7]+              {
  try { 
    yylval.i = std::stoul(yytext, NULL, 8);
    return tINTEGER;
  } catch (const std::out_of_range&) {
    yyerror("integer overflow");
  }
}
 
0[:digit:]+              yyerror("Invalid base 8 number.");

0|[1-9][:digit:]*         {
  try { 
    yylval.i = std::stoul(yytext, NULL, 10); 
    return tINTEGER;
  } catch (const std::out_of_range&) {
    yyerror("integer overflow");
  }
}

  /* ====================================================================== */
  /* ====[                 3.8.2 - Reais                              ]==== */
  /* ====================================================================== */

[:digit:]*\.[:digit:]+{EXP}?    { yylval.d = std::stod(yytext); return tREAL; }
[:digit:]+\.[:digit:]*{EXP}?    { yylval.d = std::stod(yytext); return tREAL; }
[:digit:]+{EXP}                 { yylval.d = std::stod(yytext); return tREAL; }

  /* ====================================================================== */
  /* ====[                 3.8.1 - Strings                            ]==== */
  /* ====================================================================== */

[:alpha:][:alnum:]*           yylval.s = new std::string(yytext); return tIDENTIFIER;

"\""                          yy_push_state(X_STRING);
<X_STRING>\\                  yy_push_state(X_BACKSLASH);
<X_STRING>"\""                {
                                yylval.s = new std::string(strlit.str());
                                strlit.str("");
                                yy_pop_state();
                                return tSTRING;
                              }

<X_STRING>\0                  yyerror("NULL character in string");               
<X_STRING>\n                  yyerror("Newline in string");          
<X_STRING>.                   strlit << *yytext;
                              
<X_BACKSLASH>t                 strlit << '\t'; yy_pop_state();
<X_BACKSLASH>n                 strlit << '\n'; yy_pop_state();
<X_BACKSLASH>r                 strlit << '\r'; yy_pop_state();
<X_BACKSLASH>\"                strlit << '\"'; yy_pop_state();
<X_BACKSLASH>\\                strlit << '\\'; yy_pop_state();

<X_BACKSLASH>[0-7]{1,3}        {
                                 unsigned long res = std::stoul(yytext, NULL, 8);
                                 if (res > 255) {
                                  yyerror("Invalid 8 bit number.");
                                 }
                                 strlit << (char)(unsigned char)res;
                                 yy_pop_state();
                               }
<X_BACKSLASH>.                 strlit << *yytext; yy_pop_state();


[-()<>=+*/%;{}.]       return *yytext;

[ \t\n\r]+               ; /* ignore whitespace */

.                      yyerror("Unknown character");

%%
