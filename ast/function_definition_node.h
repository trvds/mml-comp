#ifndef __MML_AST_FUNCTION_H__
#define __MML_AST_FUNCTION_H__

#include <cdk/ast/expression_node.h>
#include <cdk/ast/typed_node.h>

namespace mml {

  class function_definition_node : public cdk::expression_node {
    cdk::sequence_node *_arguments;
    mml::block_node *_block;
    bool _isMain = false;

  public:
    function_definition_node(int lineno,
                             std::shared_ptr<cdk::basic_type> outputType,
                             cdk::sequence_node *arguments,
                             mml::block_node *block, bool isMain = false)
        : cdk::expression_node(lineno), _arguments(arguments), _block(block),
          _isMain(isMain) {
      std::vector<std::shared_ptr<cdk::basic_type>> types;
      for (size_t ax = 0; ax < _arguments->size(); ax++) {
        types.push_back(argument(ax)->type());
      }
      type(cdk::functional_type::create(types, outputType));
    }

  public:
    cdk::sequence_node *arguments() { return _arguments; }
    cdk::typed_node *argument(size_t ax) {
      return dynamic_cast<cdk::typed_node *>(_arguments->node(ax));
    }
    mml::block_node *block() { return _block; }
    bool isMain() { return _isMain; }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_function_definition_node(this, level);
    }
  };

} // namespace mml

#endif