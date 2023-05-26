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
  l22::variable_declaration_node *var_decl;
  l22::block_node      *block;
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

file           : /* empty */                {  }
               | globaldecls                {  }
               |             mainfundef     {  }
               | globaldecls mainfundef     {  }
               ;
  
global_decls   : global_decls {  }
               | global_decls global_decl {  }
               ;

global_decl    : tPUBLIC  type tIDENTIFIER opt_expr        {  }   
               | tFORWARD type tIDENTIFIER ';'             {  }
               | tFOREIGN type tIDENTIFIER ';'             {  }
               | tPUBLIC       tIDENTIFIER expr_assignment {  } 
               | declaration                               {  }
               ; 

declarations   :               declaration  {  }
               |  declarations declaration  {  }
               ;

declaration    : type tIDENTIFIER opt_expr            {  }
               | tAUTO tIDENTIFIER expr_assignment    {  }
               ;

opt_expr       : expr_assignment { $$ = $1; }
               | ';'             { $$ = nullptr; }
               ;

expr_assignment     : '=' simple_expr ';'   { $$ = $2; }
                    | '=' block_expr        { $$ = $2; }
                    ;

mainfundef     : tBEGIN innerblock tEND     {  }
	          ;

fundef         : '(' opt_vars ')' tIOTYPES return_type block     {  }
               ;

opt_vars       :  /* empty */                    { $$ = new cdk::sequence_node(LINE); }
               |  vars                           { $$ = $1; }
               ;

vars           : vars ',' tIDENTIFIER {  }
               | tIDENTIFIER          {  }
               ;

var            : type tIDENTIFIER                {  }
               ;

type      :    tINT_TYPE                     {  }
          |    tREAL_TYPE                    {  }
          |    tSTRING_TYPE                  {  }
          |    tVOID_TYPE                    {  } 
          |    '[' type ']'                  {  }
          |    function_type                 {  }
          ;

function_type  : tVOID_TYPE '<' '>'          {  }
               | tVOID_TYPE '<' types '>'    {  }
               | type '<' '>'                {  }
               | type '<' types '>'          {  }
               ;

types          : type                        {  }
               | types ',' type              {  }
               ;

block           : '{' innerblock'}' { $$ = $2; };

innerblock     : instructions               {  }
               | declarations               {  }
               | declarations instructions  {  }
               | ';'                        {  };

// TODO instruction

// TODO conditional instruction

// TODO iteration instruction

// TODO expressions

expression : expression {  }
               | expression; expressions {  }
               ;
%%
