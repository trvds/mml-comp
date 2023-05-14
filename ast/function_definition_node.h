#ifndef __MML_AST_FUNCTION_H__
#define __MML_AST_FUNCTION_H__

#include <cdk/ast/typed_node.h>

namespace mml {

  class function_definition_node : public cdk::typed_node {
    cdk::sequence_node *_arguments;
    mml::block_node *_block;
    std::shared_ptr<cdk::basic_type> _outputType;
    bool _isMain = false;

  public:
    // Constructor for main function
    function_definition_node(int lineno, mml::block_node *block)
        : cdk::typed_node(lineno), _block(block),
          _outputType(cdk::primitive_type::create(0, cdk::TYPE_INT)) {
      type(cdk::functional_type::create(_outputType));
      _isMain = true;
    }

    function_definition_node(int lineno,
                             std::shared_ptr<cdk::basic_type> outputType,
                             cdk::sequence_node *arguments,
                             mml::block_node *block)
        : cdk::typed_node(lineno), _arguments(arguments), _block(block),
          _outputType(outputType) {
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
    std::shared_ptr<cdk::basic_type> outputType() { return _outputType; }
    bool isMain() { return _isMain; }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_function_definition_node(this, level);
    }
  };

} // namespace mml

#endif