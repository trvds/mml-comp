#ifndef __MML_AST_FUNCTION_CALL_H__
#define __MML_AST_FUNCTION_CALL_H__

#include <cdk/ast/expression_node.h>
#include <cdk/ast/sequence_node.h>
#include <string>

namespace mml {

  class function_call_node : public cdk::expression_node {
    cdk::expression_node *_identifier;
    cdk::sequence_node *_arguments;

  public:
    function_call_node(int lineno, cdk::expression_node *identifier)
        : cdk::expression_node(lineno), _identifier(identifier),
          _arguments(new cdk::sequence_node(lineno)) {}

    function_call_node(int lineno, cdk::expression_node *identifier,
                       cdk::sequence_node *arguments)
        : cdk::expression_node(lineno), _identifier(identifier),
          _arguments(arguments) {}

  public:
    cdk::expression_node *identifier() { return _identifier; }
    cdk::sequence_node *arguments() { return _arguments; }
    cdk::expression_node *argument(size_t ix) {
      return dynamic_cast<cdk::expression_node *>(_arguments->node(ix));
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_function_call_node(this, level);
    }
  };

} // namespace mml

#endif