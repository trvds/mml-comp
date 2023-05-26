%{
//-- don't change *any* of these: if you do, you'll break the compiler.
#include <algorithm>
#include <memory>
#include <cstring>
#include <cdk/compiler.h>
#include <cdk/types/types.h>
#include ".auto/all_nodes.h"
#define LINE                         compiler->scanner()->lineno()
#define yylex()                      compiler->scanner()->scan()
#define yyerror(compiler, s)         compiler->scanner()->error(s)
//-- don't change *any* of these --- END!
%}

%parse-param {std::shared_ptr<cdk::compiler> compiler}

%union {
  //--- don't change *any* of these: if you do, you'll break the compiler.
  YYSTYPE() : type(cdk::primitive_type::create(0, cdk::TYPE_VOID)) {}
  ~YYSTYPE() {}
  YYSTYPE(const YYSTYPE &other) { *this = other; }
  YYSTYPE& operator=(const YYSTYPE &other) { type = other.type; return *this; }

  std::shared_ptr<cdk::basic_type> type;        /* expression type */
  //-- don't change *any* of these --- END!

  int                   i;	/* integer value */
  double                d;	/* double value */
  std::string          *s;	/* symbol name or string literal */
  cdk::basic_node      *node;	/* node pointer */
  cdk::sequence_node   *sequence;
  cdk::expression_node *expression; /* expression nodes */
  cdk::lvalue_node     *lvalue;
  mml::variable_declaration_node *var_decl;
  mml::block_node      *block;
  std::shared_ptr<cdk::basic_type> vartype;
  std::vector<std::shared_ptr<cdk::basic_type>> *types;
};

%token tFOREIGN tFORWARD tPUBLIC tAUTO
%token tINT_TYPE tDOUBLE_TYPE tSTRING_TYPE tVOID_TYPE
%token tNULLPTR

%token tIOTYPES tBEGIN tEND

%token tIF tELSE tELIF tTHEN
%token tWHILE tSTOP tNEXT tRETURN tPRINT tPRINTLN

%token tSIZEOF tINPUT

%token <i> tINTEGER 
%token<d> tDOUBLE
%token <s> tIDENTIFIER tSTRING

%nonassoc tIF tWHILE
%nonassoc tTHEN tDO
%nonassoc tELIF tELSE

%right '='
%left tOR
%left tAND
%right tNOT
%left tNE tEQ
%left '<' tLE tGE '>'
%left '+' '-'
%left '*' '/' '%'
%right tUNARY
%nonassoc '(' '['


%{
//-- The rules below will be included in yyparse, the main parsing function.
%}
%%

file                : /* empty */             {  }
                    | global_decls            {  }
                    | mainfundef              {  }
                    | global_decls mainfundef {  }
                    ;
  
global_decls        : global_decls                {  }
                    | global_decls global_decl    {  }
                    ;

global_decl         : tPUBLIC  type tIDENTIFIER opt_expr_assig   {  }   
                    | tFORWARD type tIDENTIFIER ';'              {  }
                    | tFOREIGN type tIDENTIFIER ';'              {  }
                    | tPUBLIC       tIDENTIFIER expr_assignment  {  } 
                    | declaration                                {  }
                    ; 

declarations        :  declaration                {  }
                    |  declaration declarations   {  }
                    ;

declaration         : type tIDENTIFIER opt_expr_assig       {  }
                    | tAUTO tIDENTIFIER expr_assignment     {  }
                    ;

opt_expr_assig      : expr_assignment   {  }
                    | ';'               {  }
                    ;

expr_assignment     : '=' expr ';'     {  }
                    ;

mainfundef          : tBEGIN innerblock tEND {  }
	               ;

fundef              : '(' opt_vars ')' tIOTYPES return_type block     {  }
                    ;

opt_vars            :  /* empty */ {  }
                    |  vars        {  }
                    ;

vars                : vars ',' tIDENTIFIER {  }
                    | tIDENTIFIER          {  }
                    ;

var                 : type tIDENTIFIER  {  }
                    ;

type                : tINT_TYPE         {  }
                    | tDOUBLE_TYPE      {  }
                    | tSTRING_TYPE      {  }
                    | tVOID_TYPE        {  } 
                    | '[' type ']'      {  }
                    | function_type     {  }
                    ;

function_type       : tVOID_TYPE '<' '>'          {  }
                    | tVOID_TYPE '<' types '>'    {  }
                    | type '<' '>'                {  }
                    | type '<' types '>'          {  }
                    ;

types               : type              {  }
                    | types ',' type    {  }
                    ;

block               : '{' innerblock'}' {  }
                    ;

innerblock          : instructions                {  }
                    | declarations                {  }
                    | declarations instructions   {  }
                    | ';'                         {  };

instructions        : instruction                     {  }
                    | instruction ';' instructions    {  }
                    ;

instruction         :  expr ';'               {  }
                    |  exprs tPRINT           {  }
                    |  expr tPRINTLN          {  }  
                    |  tNEXT opt_int ';'     {  }       
                    |  tSTOP opt_int ';'      {  }
                    |  tRETURN opt_expr_assig {  }
                    |  if_instr               {  }
                    |  while_instr            {  }
                    |  block                  {  }   
                    ;

if_instr            : tIF '(' expr ')' instruction opt_elif_instr then_instr  {  }
                    ;

elif_instr          : tELIF '(' expr ')' instruction  {  }
                    ;

opt_elif_instr      : /* empty */                          {  }
                    | elif_instr                           {  }
                    | opt_elif_instr elif_instr            {  }
                    ;

then_instr          : /* empty */       {  }
                    | tTHEN instruction {  }
                    ;

while_instr         : tWHILE '(' expr ')' tDO instruction {  }
               
expr                : integer                     {  }
                    | real                        {  }
	               | string                      {  }
                    | tNULLPTR                    {  }
                    | '(' expr ')'                {  }
                    | '[' expr ']'                {  }
                    | lval '?'                    {  }  
                    | lval                        {  }  
                    | expr  '(' opt_exprs ')'     {  }
                    | '@' '(' opt_exprs ')'       {  }
                    | tSIZEOF '(' expr ')'        {  }
                    | tINPUT                      {  }
                    | '+' expr %prec tUNARY       {  }
                    | '-' expr %prec tUNARY       {  }
                    | expr '+' expr               {  }
                    | expr '-' expr               {  }
                    | expr '*' expr               {  }
                    | expr '/' expr               {  }  
                    | expr '%' expr               {  }
                    | expr '<' expr               {  }
                    | expr '>' expr               {  }
                    | expr tGE expr               {  }
                    | expr tLE expr               {  }
                    | expr tNE expr               {  }
                    | expr tEQ expr               {  }
                    | expr '*' expr               {  }
                    | expr '/' expr               {  }
                    | expr '%' expr               {  }
                    | expr '<' expr               {  }
                    | expr '>' expr               {  }
                    | expr tGE expr               {  }
                    | expr tLE expr               {  }
                    | expr tNE expr               {  }
                    | expr tEQ expr               {  }
                    | tNOT expr                   {  }
                    | expr tAND expr              {  }
                    | expr tOR expr               {  }
                    | expr tAND expr              {  }
                    | expr tOR expr               {  }
                    | lval '=' expr               {  }
                    ;

opt_exprs      :  /* empty */ {  }
               |  exprs       {  }
               ;

exprs               : expr           {  }
                    | expr ',' exprs {  }
                    ;

return_type         :  type        {  }
                    |  tVOID_TYPE  {  }
                    ;
              
integer             : tINTEGER     {  }
                    ;

opt_int             : /* empty */  {  }
                    | integer {  }
                    ;

real                : tDOUBLE   {  }
                    ;

string              : tSTRING           {  }
                    | string tSTRING    {  }
                    ;

lval                :  tIDENTIFIER        {  }
                    |  expr  '[' expr ']' {  }    
                    ;
%%
