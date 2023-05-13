#ifndef __MML_AST_FUNCTION_H__
#define __MML_AST_FUNCTION_H__

#include <cdk/ast/expression_node.h>

namespace mml {

  class function_definition_node : public cdk::expression_node {
    cdk::sequence_node *_arguments;
    mml::block_node *_block;
    std::shared_ptr<cdk::basic_type> _outputType;

  public:
    function_definition_node(int lineno, cdk::sequence_node *arguments,
                             mml::block_node *block)
        : cdk::expression_node(lineno), _arguments(arguments), _block(block),
          _outputType(cdk::primitive_type::create(0, cdk::TYPE_VOID)) {}

    function_definition_node(int lineno,
                             std::shared_ptr<cdk::basic_type> funType,
                             cdk::sequence_node *arguments,
                             mml::block_node *block)
        : cdk::expression_node(lineno), _arguments(arguments), _block(block),
          _outputType(funType) {}

  public:
    cdk::sequence_node *arguments() { return _arguments; }
    cdk::typed_node *argument(size_t ax) {
      return dynamic_cast<cdk::typed_node *>(_arguments->node(ax));
    }
    mml::block_node *block() { return _block; }
    std::shared_ptr<cdk::basic_type> outputType() { return _outputType; }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_function_definition_node(this, level);
    }
  };

} // namespace mml

#endif