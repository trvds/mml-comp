#ifndef __MML_AST_STOP_H__
#define __MML_AST_STOP_H__

#include <cdk/ast/basic_node.h>

namespace mml {

  class stop_node : public cdk::basic_node {
    int _level;

  public:
    stop_node(int lineno) : cdk::basic_node(lineno) {}

  public:
    int level() const { return _level; }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_stop_node(this, level);
    }
  };

} // namespace mml

#endif