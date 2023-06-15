#ifndef __MML_TARGETS_POSTFIX_WRITER_H__
#define __MML_TARGETS_POSTFIX_WRITER_H__

#include "targets/basic_ast_visitor.h"
#include "targets/symbol.h"
#include <cdk/emitters/basic_postfix_emitter.h>
#include <cdk/symbol_table.h>
#include <cdk/types/functional_type.h>

#include <set>
#include <sstream>
#include <stack>
#include <vector>

namespace mml {

  //!
  //! Traverse syntax tree and generate the corresponding assembly code.
  //!
  class postfix_writer : public basic_ast_visitor {
    cdk::symbol_table<mml::symbol> &_symtab;
    cdk::basic_postfix_emitter &_pf;
    int _lbl;

    int _offset = 0;
    bool _function = false;
    bool _function_args = false;
    std::string _function_lbl;
    std::string _external_lbl;
    std::stack<int> _while_cond, _while_end;
    std::set<std::string> _external_funcs;
    std::set<std::string> _declare_symbols;
    std::vector<std::string> _return_lbls;
    std::vector<std::shared_ptr<mml::symbol>> _function_symbols;

  public:
    postfix_writer(std::shared_ptr<cdk::compiler> compiler, cdk::symbol_table<mml::symbol> &symtab,
                   cdk::basic_postfix_emitter &pf)
        : basic_ast_visitor(compiler), _symtab(symtab), _pf(pf), _lbl(0) {}

  public:
    ~postfix_writer() { os().flush(); }

  protected:
    void do_initializer(cdk::expression_node *const node, int lvl, std::shared_ptr<mml::symbol> const symbol);

  private:
    /** Method used to generate sequential labels. */
    inline std::string mklbl(int lbl) {
      std::ostringstream oss;
      if (lbl < 0)
        oss << ".L" << -lbl;
      else
        oss << "_L" << lbl;
      return oss.str();
    }

  public:
    // do not edit these lines
#define __IN_VISITOR_HEADER__
#include ".auto/visitor_decls.h" // automatically generated
#undef __IN_VISITOR_HEADER__
    // do not edit these lines: end
  };

} // namespace mml

#endif
