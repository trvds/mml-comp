#ifndef __MML_TARGETS_TYPE_CHECKER_H__
#define __MML_TARGETS_TYPE_CHECKER_H__

#include "targets/basic_ast_visitor.h"

namespace mml {

  /**
   * Print nodes as XML elements to the output stream.
   */
  class type_checker : public basic_ast_visitor {
    cdk::symbol_table<mml::symbol> &_symtab;

    basic_ast_visitor *_parent;

  public:
    type_checker(std::shared_ptr<cdk::compiler> compiler, cdk::symbol_table<mml::symbol> &symtab,
                 basic_ast_visitor *parent)
        : basic_ast_visitor(compiler), _symtab(symtab), _parent(parent) {}

  public:
    ~type_checker() { os().flush(); }

  protected:
    void process_expr(cdk::binary_operation_node *const node, bool process_pointers, int lvl);
    void check_operand_type(cdk::binary_operation_node *const node, int lvl, std::vector<cdk::typename_type> valid_types);
    void check_same_type(cdk::binary_operation_node *const node, int lvl);

  public:
    // do not edit these lines
#define __IN_VISITOR_HEADER__
#include ".auto/visitor_decls.h" // automatically generated
#undef __IN_VISITOR_HEADER__
    // do not edit these lines: end
  };

} // namespace mml

//---------------------------------------------------------------------------
//     HELPER MACRO FOR TYPE CHECKING
//---------------------------------------------------------------------------

#define CHECK_TYPES(compiler, symtab, node)                                                                            \
  {                                                                                                                    \
    try {                                                                                                              \
      mml::type_checker checker(compiler, symtab, this);                                                               \
      (node)->accept(&checker, 0);                                                                                     \
    } catch (const std::string &problem) {                                                                             \
      std::cerr << (node)->lineno() << ": " << problem << std::endl;                                                   \
      return;                                                                                                          \
    }                                                                                                                  \
  }

#define ASSERT_SAFE_EXPRESSIONS CHECK_TYPES(_compiler, _symtab, node)

#endif
