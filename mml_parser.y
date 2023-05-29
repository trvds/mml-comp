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
  mml::variable_declaration_node *decl;
  mml::block_node      *block;
  std::shared_ptr<cdk::basic_type> vartype;
  std::vector<std::shared_ptr<cdk::basic_type>> *types;
  mml::function_definition_node *fndef;
};

%token tFOREIGN tFORWARD tPUBLIC tAUTO tPRIVATE
%token tINT_TYPE tDOUBLE_TYPE tSTRING_TYPE tVOID_TYPE
%token tNULLPTR
%token tIOTYPES tBEGIN tEND
%token tIF tELSE tELIF
%token tWHILE tSTOP tNEXT tRETURN tPRINT tPRINTLN
%token tSIZEOF tINPUT
%token <i> tINTEGER 
%token <d> tDOUBLE
%token <s> tIDENTIFIER tSTRING

%nonassoc tIFX
%nonassoc tIF tWHILE
%nonassoc tELIF tELSE

%nonassoc '(' '['
%nonassoc tUNARY '?'
%left '*' '/' '%'
%left '+' '-'
%left '<' tLE tGE '>'
%left tNE tEQ
%nonassoc tNOT
%left tAND
%left tOR
%right '='


%type <node> instruction if_instr elif_instr while_instr
%type <decl> declaration var global_decl
%type <sequence> file declarations instructions opt_exprs exprs opt_vars vars global_decls
%type <expression> expr integer real opt_expr_assig expr_assignment
%type <lvalue> lval 
%type <block> block innerblock
%type <vartype> type function_type return_type
%type <types> types
%type <fndef> fundef mainfundef
%type <s> string

%{
//-- The rules below will be included in yyparse, the main parsing function.
%}
%%

file                : /* empty */                  { compiler->ast($$ = new cdk::sequence_node(LINE)); }
                    | global_decls                 { compiler->ast($$ = $1); }
                    | mainfundef                   { compiler->ast($$ = new cdk::sequence_node(LINE, $1)); }
                    | global_decls mainfundef      { compiler->ast($$ = new cdk::sequence_node(LINE, $2, $1)); }
                    ;

global_decls        : global_decl                { $$ = new cdk::sequence_node(LINE, $1); }
                    | global_decls global_decl    { $$ = new cdk::sequence_node(LINE, $2, $1); }
                    ;

global_decl         : tPUBLIC type tIDENTIFIER opt_expr_assig   { $$ = new mml::variable_declaration_node(LINE, tPUBLIC, $2, *$3, $4); delete $3; }
                    | tFORWARD type tIDENTIFIER ';'              { $$ = new mml::variable_declaration_node(LINE, tFORWARD, $2, *$3, nullptr); delete $3; }
                    | tFOREIGN type tIDENTIFIER ';'              { $$ = new mml::variable_declaration_node(LINE, tFOREIGN, $2, *$3, nullptr); delete $3; }
                    | tPUBLIC tAUTO tIDENTIFIER expr_assignment         { $$ = new mml::variable_declaration_node(LINE, tPUBLIC, nullptr, *$3, $4); delete $3; }
                    | tPUBLIC tIDENTIFIER expr_assignment       { $$ = new mml::variable_declaration_node(LINE, tPUBLIC, nullptr, *$2, $3); delete $2; } 
                    | declaration                                { $$ = $1; }
                    ; 

declarations        :  declaration                { $$ = new cdk::sequence_node(LINE, $1); }
                    |  declarations declaration   { $$ = new cdk::sequence_node(LINE, $2, $1); }
                    ;

declaration         : type tIDENTIFIER opt_expr_assig       { $$ = new mml::variable_declaration_node(LINE, tPRIVATE, $1, *$2, $3); delete $2; }
                    | tAUTO tIDENTIFIER expr_assignment     { $$ = new mml::variable_declaration_node(LINE, tPRIVATE, nullptr, *$2, $3); delete $2; }
                    ;

opt_expr_assig      : ';'               { $$ = nullptr; }
                    | expr_assignment   { $$ = $1; }
                    ;

expr_assignment     : '=' expr ';'     { $$ = $2; }
                    ;

mainfundef          : tBEGIN innerblock tEND {$$ = new mml::function_definition_node(LINE, cdk::primitive_type::create(4, cdk::TYPE_INT), new cdk::sequence_node(LINE), $2, true); }
                    ;

fundef              : '(' opt_vars ')' tIOTYPES return_type block     { $$ = new mml::function_definition_node(LINE, $5, $2, $6); }
                    ;

opt_vars            :  /* empty */ { $$ = new cdk::sequence_node(LINE); }
                    |  vars        { $$ = $1; }
                    ;

vars                : var           { $$ = new cdk::sequence_node(LINE, $1); }
                    | vars ',' var  { $$ = new cdk::sequence_node(LINE, $3, $1); }
                    ;

var                 : type tIDENTIFIER  { $$ = new mml::variable_declaration_node(LINE, tPRIVATE, $1, *$2, nullptr); delete $2; }
                    ;

type                : tINT_TYPE         { $$ = cdk::primitive_type::create(4, cdk::TYPE_INT); }
                    | tDOUBLE_TYPE      { $$ = cdk::primitive_type::create(8, cdk::TYPE_DOUBLE); }
                    | tSTRING_TYPE      { $$ = cdk::primitive_type::create(4, cdk::TYPE_STRING); }
                    | '[' tVOID_TYPE ']' { $$ = cdk::reference_type::create(4, cdk::primitive_type::create(4, cdk::TYPE_VOID)); } 
                    | '[' type ']'      { if ($2->name() == cdk::TYPE_POINTER && 
                                                  cdk::reference_type::cast($2)->referenced()->name() == cdk::TYPE_VOID) {
                                                    $$ = cdk::reference_type::create(4, cdk::primitive_type::create(4, cdk::TYPE_VOID)); 
                                                  } 
                                               else {
                                                $$ = cdk::reference_type::create(4, $2);     
                                               } }
                    | function_type     { $$ = $1; }
                    ;

function_type       : type '<' '>'                { $$ = cdk::functional_type::create($1); }
                    | type '<' types '>'          { $$ = cdk::functional_type::create(*$3, $1); delete $3; }
                    | tVOID_TYPE '<' '>'          { $$ = cdk::functional_type::create(cdk::primitive_type::create(4, cdk::TYPE_VOID)); }
                    | tVOID_TYPE '<' types '>'    { $$ = cdk::functional_type::create(*$3, cdk::primitive_type::create(4, cdk::TYPE_VOID)); delete $3; }
                    ;

types               : type              { $$ = new std::vector<std::shared_ptr<cdk::basic_type>>(); $$->push_back($1); }
                    | types ',' type    { $$ = $1; $$->push_back($3); }
                    ;

block               : '{' innerblock '}' { $$ = $2; }
                    ;

innerblock          : instructions                { $$ = new mml::block_node(LINE, new cdk::sequence_node(LINE), $1); }
                    | declarations                { $$ = new mml::block_node(LINE, $1, new cdk::sequence_node(LINE)); }
                    | declarations instructions   { $$ = new mml::block_node(LINE, $1, $2); }
                    | /* empty */                 { $$ = new mml::block_node(LINE, new cdk::sequence_node(LINE), new cdk::sequence_node(LINE)); }
                    ;

instructions        : instruction                     { $$ = new cdk::sequence_node(LINE, $1); }
                    | instruction instructions        { std::reverse($2->nodes().begin(), $2->nodes().end()); $$ = new cdk::sequence_node(LINE, $1, $2); std::reverse($$->nodes().begin(), $$->nodes().end()); }
                    ;

instruction         :  expr ';'                       { $$ = new mml::evaluation_node(LINE, $1); }
                    |  exprs tPRINT                   { $$ = new mml::print_node(LINE, $1); }
                    |  exprs tPRINTLN                 { $$ = new mml::print_node(LINE, $1, true); }  
                    |  tNEXT ';'                      { $$ = new mml::next_node(LINE); }
                    |  tNEXT tINTEGER ';'             { $$ = new mml::next_node(LINE, $2); }
                    |  tSTOP ';'                      { $$ = new mml::stop_node(LINE); }
                    |  tSTOP tINTEGER ';'             { $$ = new mml::stop_node(LINE, $2); }
                    |  tRETURN ';'                    { $$ = new mml::return_node(LINE); }
                    |  tRETURN expr ';'               { $$ = new mml::return_node(LINE, $2); }
                    |  if_instr                       { $$ = $1; }
                    |  while_instr                    { $$ = $1; }
                    |  block                          { $$ = $1; }   
                    ;

if_instr            : tIF '(' expr ')' instruction %prec tIFX { $$ = new mml::if_node(LINE, $3, $5); }
                    | tIF '(' expr ')' instruction elif_instr { $$ = new mml::if_else_node(LINE, $3, $5, $6); }
                    ;

elif_instr          : tELSE instruction                         { $$ = $2; }
                    | tELIF '(' expr ')' instruction %prec tIFX { $$ = new mml::if_node(LINE, $3, $5); }
                    | tELIF '(' expr ')' instruction elif_instr { $$ = new mml::if_else_node(LINE, $3, $5, $6); }
                    ;

while_instr         : tWHILE '(' expr ')' instruction { $$ = new mml::while_node(LINE, $3, $5); }
                    ;
               
expr                : integer                     { $$ = $1; } // literais 
                    | real                        { $$ = $1; } 
	                  | string                      { $$ = new cdk::string_node(LINE, $1); } 
                    | tNULLPTR                    { $$ = new mml::nullptr_node(LINE); } 
                    | '+' expr %prec tUNARY       { $$ = new mml::identity_node(LINE, $2); } // primárias
                    | '-' expr %prec tUNARY       { $$ = new cdk::neg_node(LINE, $2); } 
                    | expr '*' expr               { $$ = new cdk::mul_node(LINE, $1, $3); } // multiplicativas
                    | expr '/' expr               { $$ = new cdk::div_node(LINE, $1, $3); }   
                    | expr '%' expr               { $$ = new cdk::mod_node(LINE, $1, $3); } 
                    | expr '+' expr               { $$ = new cdk::add_node(LINE, $1, $3); } // aditiva
                    | expr '-' expr               { $$ = new cdk::sub_node(LINE, $1, $3); } 
                    | expr '<' expr               { $$ = new cdk::lt_node(LINE, $1, $3); } // comparativa
                    | expr '>' expr               { $$ = new cdk::gt_node(LINE, $1, $3); } 
                    | expr tGE expr               { $$ = new cdk::ge_node(LINE, $1, $3); } 
                    | expr tLE expr               { $$ = new cdk::le_node(LINE, $1, $3); } 
                    | expr tNE expr               { $$ = new cdk::ne_node(LINE, $1, $3); }
                    | expr tEQ expr               { $$ = new cdk::eq_node(LINE, $1, $3); }
                    | tNOT expr                   { $$ = new cdk::not_node(LINE, $2); } // não lógica
                    | expr tAND expr              { $$ = new cdk::and_node(LINE, $1, $3); } // e lógica
                    | expr tOR expr               { $$ = new cdk::or_node (LINE, $1, $3); } // ou lógica
                    | lval '=' expr               { $$ = new cdk::assignment_node(LINE, $1, $3); } // de atribuição
                    | tINPUT                      { $$ = new mml::input_node(LINE); } // leitura
                    | lval                        { $$ = new cdk::rvalue_node(LINE, $1); } // identificador
                    | '(' expr ')'                { $$ = $2; } // parêntesis curvos
                    | fundef                      { $$ = $1; } // definição de função
                    | '[' expr ']'                { $$ = new mml::stack_alloc_node(LINE, $2); } // reserva de memória
                    | lval '?'                    { $$ = new mml::address_of_node(LINE, $1); } // indicação de posição 
                    | expr  '(' opt_exprs ')'     { $$ = new mml::function_call_node(LINE, $1, $3); } // invocação de função
                    | '@' '(' opt_exprs ')'       { $$ = new mml::function_call_node(LINE, nullptr, $3); } // invocação de função recursiva
                    | tSIZEOF '(' expr ')'        { $$ = new mml::sizeof_node(LINE, $3); } // dimensão
                    ;

opt_exprs           :  /* empty */ { $$ = new cdk::sequence_node(LINE); }
                    |  exprs       { $$ = $1; }
                    ;

exprs               : expr           { $$ = new cdk::sequence_node(LINE, $1); }
                    | exprs ',' expr { $$ = new cdk::sequence_node(LINE, $3, $1); }
                    ;

return_type         :  type        { $$ = $1; }
                    |  tVOID_TYPE  { $$ = cdk::primitive_type::create(4, cdk::TYPE_VOID); }
                    ;
              
integer             : tINTEGER     { $$ = new cdk::integer_node(LINE, $1); }
                    ;

real                : tDOUBLE   { $$ = new cdk::double_node(LINE, $1); }
                    ;

string              : tSTRING           { $$ = $1; }
                    | string tSTRING    { $$ = $1; $$->append(*$2); delete $2; }
                    ;

lval                :  tIDENTIFIER        { $$ = new cdk::variable_node(LINE, $1); }
                    |  expr  '[' expr ']' { $$ = new mml::index_node(LINE, $1, $3); }    
                    ;
%%
