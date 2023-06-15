#include "targets/postfix_writer.h"
#include ".auto/all_nodes.h" // all_nodes.h is automatically generated
#include "targets/frame_size_calculator.h"
#include "targets/symbol.h"
#include "targets/type_checker.h"
#include <iostream>
#include <sstream>
#include <string>

#include <mml_parser.tab.h>

//--------------------------------------------------------------------------//
//                                CDK                                       //
//--------------------------------------------------------------------------//

void mml::postfix_writer::do_sequence_node(cdk::sequence_node *const node, int lvl) {
  for (size_t i = 0; i < node->size(); i++) {
    node->node(i)->accept(this, lvl);
  }
}

void mml::postfix_writer::do_nil_node(cdk::nil_node *const node, int lvl) {
  // EMPTY
}
void mml::postfix_writer::do_data_node(cdk::data_node *const node, int lvl) {
  // EMPTY
}

//--------------------------------------------------------------------------//
//                                LITERALS                                  //
//--------------------------------------------------------------------------//

void mml::postfix_writer::do_integer_node(cdk::integer_node *const node, int lvl) {
  if (_function) {
    _pf.INT(node->value());
  } else {
    _pf.SINT(node->value());
  }
}

void mml::postfix_writer::do_double_node(cdk::double_node *const node, int lvl) {
  std::string lbl = mklbl(++_lbl);
  if (_function) {
    _pf.DOUBLE(node->value());
  } else {
    _pf.SDOUBLE(node->value());
  }
}

void mml::postfix_writer::do_string_node(cdk::string_node *const node, int lvl) {
  std::string lbl = mklbl(++_lbl);
  _pf.RODATA();
  _pf.ALIGN();
  _pf.LABEL(lbl);
  _pf.SSTRING(node->value());
  if (_function) {
    _pf.TEXT(_return_lbls.back());
    _pf.ADDR(lbl);
  } else {
    _pf.DATA();
    _pf.SADDR(lbl);
  }
}

void mml::postfix_writer::do_nullptr_node(mml::nullptr_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  if (_function) {
    _pf.INT(0);
  } else {
    _pf.SINT(0);
  }
}

//--------------------------------------------------------------------------//
//                           PRINT AND INPUT                                //
//--------------------------------------------------------------------------//

void mml::postfix_writer::do_print_node(mml::print_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  for (size_t i = 0; i < node->arguments()->size(); i++) {
    auto arg = dynamic_cast<cdk::expression_node *>(node->arguments()->node(i));
    arg->accept(this, lvl);
    if (arg->is_typed(cdk::TYPE_INT)) {
      _external_funcs.insert("printi");
      _pf.CALL("printi");
      _pf.TRASH(4);
    } else if (arg->is_typed(cdk::TYPE_DOUBLE)) {
      _external_funcs.insert("printd");
      _pf.CALL("printd");
      _pf.TRASH(8);
    } else if (arg->is_typed(cdk::TYPE_STRING)) {
      _external_funcs.insert("prints");
      _pf.CALL("prints");
      _pf.TRASH(4);
    }
  }

  if (node->newline()) {
    _external_funcs.insert("println");
    _pf.CALL("println");
  }
}

void mml::postfix_writer::do_input_node(mml::input_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  if (node->is_typed(cdk::TYPE_INT)) {
    _external_funcs.insert("readi");
    _pf.CALL("readi");
    _pf.LDFVAL32();
  } else if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _external_funcs.insert("readd");
    _pf.CALL("readd");
    _pf.LDFVAL64();
  } else {
    std::cerr << (node)->lineno() << ": error -> invalid input" << std::endl;
    return;
  }
}

//--------------------------------------------------------------------------//
//                           UNARY ARITHMETIC EXPRESSIONS                   //
//--------------------------------------------------------------------------//

void mml::postfix_writer::do_identity_node(mml::identity_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->argument()->accept(this, lvl + 2);
}

void mml::postfix_writer::do_neg_node(cdk::neg_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->argument()->accept(this, lvl);
  _pf.NEG();
}

void mml::postfix_writer::do_not_node(cdk::not_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->argument()->accept(this, lvl);
  _pf.INT(0);
  _pf.EQ();
}

//--------------------------------------------------------------------------//
//                           BINARY ARITHMETIC EXPRESSIONS                  //
//--------------------------------------------------------------------------//

void mml::postfix_writer::do_add_node(cdk::add_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->left()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  } else if (node->is_typed(cdk::TYPE_POINTER) && node->left()->is_typed(cdk::TYPE_INT)) {
    auto ref = cdk::reference_type::cast(node->right()->type())->referenced();
    _pf.INT(ref->size());
    _pf.MUL();
  }

  node->right()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  } else if (node->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_INT)) {
    auto ref = cdk::reference_type::cast(node->left()->type())->referenced();
    _pf.INT(ref->size());
    _pf.MUL();
  }

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DADD();
  } else {
    _pf.ADD();
  }
}

void mml::postfix_writer::do_sub_node(cdk::sub_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->left()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  } else if (node->type()->name() == cdk::TYPE_POINTER && node->right()->type()->name() == cdk::TYPE_INT) {
    auto ref = cdk::reference_type::cast(node->right()->type())->referenced();
    _pf.INT(ref->size());
    _pf.MUL();
  }

  node->right()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  } else if (node->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_INT)) {
    auto ref = cdk::reference_type::cast(node->right()->type())->referenced();
    _pf.INT(ref->size());
    _pf.MUL();
  }

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DSUB();
  } else {
    _pf.SUB();
  }
}

void mml::postfix_writer::do_mul_node(cdk::mul_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->left()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  node->right()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DMUL();
  } else {
    _pf.MUL();
  }
}

void mml::postfix_writer::do_div_node(cdk::div_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->left()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  node->right()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DDIV();
  } else {
    _pf.DIV();
  }
}

void mml::postfix_writer::do_mod_node(cdk::mod_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);

  _pf.MOD();
}

//--------------------------------------------------------------------------//
//                           LOGICAL EXPRESSIONS                            //
//--------------------------------------------------------------------------//

void mml::postfix_writer::do_and_node(cdk::and_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  std::string lbl = mklbl(++_lbl);

  node->left()->accept(this, lvl + 2);
  _pf.DUP32();
  _pf.JZ(lbl);

  node->right()->accept(this, lvl + 2);
  _pf.AND();
  _pf.ALIGN();
  _pf.LABEL(lbl);
}

void mml::postfix_writer::do_or_node(cdk::or_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  std::string lbl = mklbl(++_lbl);

  node->left()->accept(this, lvl + 2);
  _pf.DUP32();
  _pf.JNZ(lbl);

  node->right()->accept(this, lvl + 2);
  _pf.OR();
  _pf.ALIGN();
  _pf.LABEL(lbl);
}

void mml::postfix_writer::do_lt_node(cdk::lt_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->left()->accept(this, lvl);
  if (node->right()->is_typed(cdk::TYPE_DOUBLE) && !node->left()->is_typed(cdk::TYPE_DOUBLE))
    _pf.I2D();

  node->right()->accept(this, lvl);
  if (node->left()->is_typed(cdk::TYPE_DOUBLE) && !node->right()->is_typed(cdk::TYPE_DOUBLE))
    _pf.I2D();

  if (node->left()->is_typed(cdk::TYPE_DOUBLE) || node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DCMP();
    _pf.INT(0);
  }

  _pf.LT();
}

void mml::postfix_writer::do_le_node(cdk::le_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->left()->accept(this, lvl);
  if (node->right()->is_typed(cdk::TYPE_DOUBLE) && !node->left()->is_typed(cdk::TYPE_DOUBLE))
    _pf.I2D();

  node->right()->accept(this, lvl);
  if (node->left()->is_typed(cdk::TYPE_DOUBLE) && !node->right()->is_typed(cdk::TYPE_DOUBLE))
    _pf.I2D();

  if (node->left()->is_typed(cdk::TYPE_DOUBLE) || node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DCMP();
    _pf.INT(0);
  }

  _pf.LE();
}

void mml::postfix_writer::do_ge_node(cdk::ge_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->left()->accept(this, lvl);
  if (node->right()->is_typed(cdk::TYPE_DOUBLE) && !node->left()->is_typed(cdk::TYPE_DOUBLE))
    _pf.I2D();

  node->right()->accept(this, lvl);
  if (node->left()->is_typed(cdk::TYPE_DOUBLE) && !node->right()->is_typed(cdk::TYPE_DOUBLE))
    _pf.I2D();

  if (node->left()->is_typed(cdk::TYPE_DOUBLE) || node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DCMP();
    _pf.INT(0);
  }

  _pf.GE();
}

void mml::postfix_writer::do_gt_node(cdk::gt_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->left()->accept(this, lvl);
  if (node->right()->is_typed(cdk::TYPE_DOUBLE) && !node->left()->is_typed(cdk::TYPE_DOUBLE))
    _pf.I2D();

  node->right()->accept(this, lvl);
  if (node->left()->is_typed(cdk::TYPE_DOUBLE) && !node->right()->is_typed(cdk::TYPE_DOUBLE))
    _pf.I2D();

  if (node->left()->is_typed(cdk::TYPE_DOUBLE) || node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DCMP();
    _pf.INT(0);
  }

  _pf.GT();
}

void mml::postfix_writer::do_ne_node(cdk::ne_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->left()->accept(this, lvl);
  if (node->right()->is_typed(cdk::TYPE_DOUBLE) && !node->left()->is_typed(cdk::TYPE_DOUBLE))
    _pf.I2D();

  node->right()->accept(this, lvl);
  if (node->left()->is_typed(cdk::TYPE_DOUBLE) && !node->right()->is_typed(cdk::TYPE_DOUBLE))
    _pf.I2D();

  if (node->left()->is_typed(cdk::TYPE_DOUBLE) || node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DCMP();
    _pf.INT(0);
  }

  _pf.NE();
}

void mml::postfix_writer::do_eq_node(cdk::eq_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->left()->accept(this, lvl);
  if (node->right()->is_typed(cdk::TYPE_DOUBLE) && !node->left()->is_typed(cdk::TYPE_DOUBLE))
    _pf.I2D();

  node->right()->accept(this, lvl);
  if (node->left()->is_typed(cdk::TYPE_DOUBLE) && !node->right()->is_typed(cdk::TYPE_DOUBLE))
    _pf.I2D();

  if (node->left()->is_typed(cdk::TYPE_DOUBLE) || node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DCMP();
    _pf.INT(0);
  }

  _pf.EQ();
}

//---------------------------------------------------------------------------
//                             CONDITIONAL AND LOOP INSTRUCTIONS           //
//---------------------------------------------------------------------------

void mml::postfix_writer::do_while_node(mml::while_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  _symtab.push();
  _while_cond.push(++_lbl);
  _while_end.push(++_lbl);

  _pf.ALIGN();
  _pf.LABEL(mklbl(_while_cond.top()));

  node->condition()->accept(this, lvl + 2);

  _pf.JZ(mklbl(_while_end.top()));

  node->block()->accept(this, lvl + 2);

  _pf.JMP(mklbl(_while_cond.top()));

  _pf.ALIGN();
  _pf.LABEL(mklbl(_while_end.top()));

  _while_end.pop();
  _while_cond.pop();
  _symtab.pop();
}

void mml::postfix_writer::do_if_node(mml::if_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  std::string lbl_endif = mklbl(++_lbl);

  node->condition()->accept(this, lvl);
  _pf.JZ(lbl_endif);

  node->block()->accept(this, lvl + 2);
  _pf.LABEL(lbl_endif);
}

void mml::postfix_writer::do_if_else_node(mml::if_else_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  std::string lbl_else = mklbl(++_lbl);
  std::string lbl_endif = mklbl(++_lbl);

  node->condition()->accept(this, lvl);
  _pf.JZ(lbl_else);

  node->thenblock()->accept(this, lvl + 2);
  _pf.JMP(lbl_endif);
  _pf.LABEL(lbl_else);

  node->elseblock()->accept(this, lvl + 2);
  _pf.LABEL(lbl_endif);
}

void mml::postfix_writer::do_evaluation_node(mml::evaluation_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->argument()->accept(this, lvl);
}

//---------------------------------------------------------------------------
//                             CALL FUNCTION                               //
//---------------------------------------------------------------------------

void mml::postfix_writer::do_function_call_node(mml::function_call_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  // Determine input types based on if it's a recursive or non-recursive case
  std::vector<std::shared_ptr<cdk::basic_type>> input_types =
      node->identifier() ? cdk::functional_type::cast(node->identifier()->type())->input()->components()
                         : cdk::functional_type::cast(_function_symbols.back()->type())->input()->components();

  size_t args_size = 0;
  if (node->arguments()) {
    for (int i = node->arguments()->size() - 1; i >= 0; --i) {
      auto arg = dynamic_cast<cdk::expression_node *>(node->arguments()->node(i));
      arg->accept(this, lvl + 2);
      args_size += arg->type()->size();
      if (input_types.at(i)->name() == cdk::TYPE_DOUBLE && arg->is_typed(cdk::TYPE_INT)) {
        _pf.I2D();
        args_size += arg->type()->size();
      }
    }
  }

  _external_lbl.clear();

  if (node->identifier()) {
    node->identifier()->accept(this, lvl + 2);
    if (_external_lbl.empty()) {
      _pf.BRANCH();
    } else {
      _pf.CALL(_external_lbl);
    }
  } else {
    _pf.CALL(_return_lbls.back());
  }

  if (args_size != 0) {
    _pf.TRASH(args_size);
  }

  if (node->is_typed(cdk::TYPE_INT)) {
    if (_external_lbl.empty()) {
      _pf.LDFVAL64();
      _pf.D2I();
    } else {
      _pf.LDFVAL32();
    }
  } else if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.LDFVAL64();
  } else if (node->is_typed(cdk::TYPE_STRING) || node->is_typed(cdk::TYPE_POINTER) ||
             node->is_typed(cdk::TYPE_FUNCTIONAL)) {
    _pf.LDFVAL32();
  }

  _external_lbl.clear();
}

//---------------------------------------------------------------------------
//                            FUNCTION DEFINITION                          //
//---------------------------------------------------------------------------

void mml::postfix_writer::do_function_definition_node(mml::function_definition_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  std::string lbl = node->isMain() ? "_main" : mklbl(++_lbl);
  std::shared_ptr<mml::symbol> symbol = new_symbol();

  _return_lbls.push_back(lbl);

  if (node->isMain()) {
    _function_symbols.push_back(symbol);
    reset_new_symbol();

    for (std::string symbol_name : _declare_symbols) {
      auto symbol = _symtab.find(symbol_name, 0);
      if (symbol->is_foreign()) {
        _external_funcs.insert(symbol_name);
      } else {
        _pf.BSS();
        _pf.ALIGN();
        _pf.LABEL(symbol_name);
        _pf.SALLOC(symbol->type()->size());
      }
    }
    _symtab.push();
  }

  _pf.TEXT(_return_lbls.back());
  _pf.ALIGN();

  if (node->isMain()) {
    _pf.GLOBAL(lbl, _pf.FUNC());
  }

  _pf.LABEL(lbl);

  if (!node->isMain()) {
    if (symbol) {
      _function_symbols.push_back(symbol);
      reset_new_symbol();
    }
    _offset = 8;
    _symtab.push();
    if (node->arguments()) {
      _function_args = true;
      for (size_t i = 0; i < node->arguments()->size(); i++) {
        cdk::basic_node *argument = node->arguments()->node(i);
        if (argument) {
          argument->accept(this, 0);
          continue;
        }
        break;
      }
      _function_args = false;
    }
  }

  frame_size_calculator lsc(_compiler, _symtab, symbol);
  _symtab.push();
  node->accept(&lsc, lvl);
  _symtab.pop();
  _pf.ENTER(lsc.localsize());

  if (!node->isMain()) {
    _offset = 0;
  }

  bool function_context = _function;
  _function = true;
  if (node->block()) {
    node->block()->accept(this, lvl + 4);
  }
  _function = function_context;

  _symtab.pop();

  if (node->isMain()) {
    _return_lbls.pop_back();
    _pf.INT(0);
    _pf.STFVAL32();
  }
  _pf.LEAVE();
  _pf.RET();

  if (node->isMain()) {
    for (std::string external_func : _external_funcs) {
      _pf.EXTERN(external_func);
    }
    _external_funcs.clear();
  } else {
    _return_lbls.pop_back();
    if (symbol) {
      _function_symbols.pop_back();
    }
    if (_function) {
      _pf.TEXT(_return_lbls.back());
      _pf.ADDR(lbl);
    }
    _function_lbl = lbl;
  }
}

void mml::postfix_writer::do_return_node(mml::return_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  auto function = _function_symbols.back();
  auto output = cdk::functional_type::cast(function->type())->output(0);

  if (output->name() != cdk::TYPE_VOID) {
    node->retval()->accept(this, lvl + 2);
  }

  if (output->name() == cdk::TYPE_INT) {
    if (function->is_main()) {
      _pf.STFVAL32();
    } else {
      _pf.I2D();
      _pf.STFVAL64();
    }
  } else if (output->name() == cdk::TYPE_DOUBLE) {
    if (node->retval()->type()->name() == cdk::TYPE_INT) {
      _pf.I2D();
    }
    _pf.STFVAL64();
  } else if (output->name() == cdk::TYPE_STRING || output->name() == cdk::TYPE_POINTER ||
             output->name() == cdk::TYPE_FUNCTIONAL) {
    _pf.STFVAL32();
  }

  _pf.LEAVE();
  _pf.RET();
}

//--------------------------------------------------------------------------//
//                           MEMORY                                         //
//--------------------------------------------------------------------------//

void mml::postfix_writer::do_address_of_node(mml::address_of_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->lvalue()->accept(this, lvl + 2);
}

void mml::postfix_writer::do_index_node(mml::index_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->base()->accept(this, lvl);
  node->index()->accept(this, lvl);

  _pf.INT(node->type()->size());
  _pf.MUL();
  _pf.ADD();
}

void mml::postfix_writer::do_sizeof_node(mml::sizeof_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  if (_function) {
    _pf.INT(node->expression()->type()->size());
  } else {
    _pf.SINT(node->expression()->type()->size());
  }
}

void mml::postfix_writer::do_stack_alloc_node(mml::stack_alloc_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  auto ref = cdk::reference_type::cast(node->type())->referenced();

  node->argument()->accept(this, lvl);

  _pf.INT(ref->size());
  _pf.MUL();
  _pf.ALLOC();
  _pf.SP();
}

//--------------------------------------------------------------------------//
//                           NEXT AND STOP INSTRUCTIONS                     //
//--------------------------------------------------------------------------//

void mml::postfix_writer::do_next_node(mml::next_node *const node, int lvl) {
  if (_while_cond.size() > 0 && _while_cond.size() >= (size_t)node->level()) {
    std::stack<int> temp_while_cond;
    for (int i = 1; i < node->level(); ++i) {
      temp_while_cond.push(_while_cond.top());
      _while_cond.pop();
    }
    _pf.JMP(mklbl(_while_cond.top()));
    while (temp_while_cond.size() > 0) {
      _while_cond.push(temp_while_cond.top());
      temp_while_cond.pop();
    }
  } else {
    std::cerr << (node)->lineno() << ":  error -> next outside or goes to outside of cycle" << std::endl;
    return;
  }
}

void mml::postfix_writer::do_stop_node(mml::stop_node *const node, int lvl) {
  if (_while_end.size() > 0 && _while_end.size() >= (size_t)node->level()) {
    std::stack<int> temp_while_end;
    for (int i = 1; i < node->level(); ++i) {
      temp_while_end.push(_while_end.top());
      _while_end.pop();
    }
    _pf.JMP(mklbl(_while_end.top()));
    while (temp_while_end.size() > 0) {
      _while_end.push(temp_while_end.top());
      temp_while_end.pop();
    }
  } else {
    std::cerr << (node)->lineno() << ": error -> stop outside or goes to outside of cycle" << std::endl;
    return;
  }
}

//--------------------------------------------------------------------------//
//                           BLOCK                                          //
//--------------------------------------------------------------------------//

void mml::postfix_writer::do_block_node(mml::block_node *const node, int lvl) {
  _symtab.push();

  if (node->declarations())
    node->declarations()->accept(this, lvl + 2);

  if (node->instructions())
    node->instructions()->accept(this, lvl + 2);

  _symtab.pop();
}

//--------------------------------------------------------------------------//
//                           VARIABLE                                       //
//--------------------------------------------------------------------------//

void mml::postfix_writer::do_variable_declaration_node(mml::variable_declaration_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  int offset = 0;
  int size_of_type = node->type()->size();

  if (_function_args) {
    offset = _offset;
    _offset += size_of_type;
  } else if (_function) {
    _offset -= size_of_type;
    offset = _offset;
  }

  std::shared_ptr<mml::symbol> symbol = new_symbol();
  if (symbol) {
    symbol->set_offset(offset);
    reset_new_symbol();
  }

  if (!_function_args && !_function) {
    _declare_symbols.insert(symbol->name());
  }

  if (node->initializer()) {
    auto init_node = node->initializer();
    if (_function) {
      init_node->accept(this, lvl);
      if (symbol->is_typed(cdk::TYPE_INT) || symbol->is_typed(cdk::TYPE_STRING) ||
          symbol->is_typed(cdk::TYPE_POINTER) || symbol->is_typed(cdk::TYPE_FUNCTIONAL)) {
        _pf.LOCAL(symbol->offset());
        _pf.STINT();
      } else if (symbol->is_typed(cdk::TYPE_DOUBLE)) {
        if (init_node->is_typed(cdk::TYPE_INT)) {
          _pf.I2D();
        }
        _pf.LOCAL(symbol->offset());
        _pf.STDOUBLE();
      } else {
        std::cerr << (node)->lineno() << ": error -> unknown declaration type" << std::endl;
        return;
      }
    } else {
      if (symbol->is_typed(cdk::TYPE_INT) || symbol->is_typed(cdk::TYPE_DOUBLE) ||
          symbol->is_typed(cdk::TYPE_POINTER)) {
        _pf.DATA();
        _pf.ALIGN();
        _pf.LABEL(symbol->name());
        if (symbol->is_typed(cdk::TYPE_INT)) {
          init_node->accept(this, lvl);
        } else if (symbol->is_typed(cdk::TYPE_DOUBLE) && init_node->is_typed(cdk::TYPE_INT)) {
          cdk::integer_node *dclini = dynamic_cast<cdk::integer_node *>(init_node);
          cdk::double_node ddi(dclini->lineno(), dclini->value());
          ddi.accept(this, lvl);
        } else if (symbol->is_typed(cdk::TYPE_DOUBLE) || init_node->is_typed(cdk::TYPE_DOUBLE)) {
          init_node->accept(this, lvl);
        }
      } else if (symbol->is_typed(cdk::TYPE_POINTER)) {
        init_node->accept(this, lvl);
      } else if (symbol->is_typed(cdk::TYPE_STRING)) {
        _pf.DATA();
        _pf.ALIGN();
        _pf.LABEL(symbol->name());
        init_node->accept(this, lvl);
      } else if (symbol->is_typed(cdk::TYPE_FUNCTIONAL)) {
        _function_symbols.push_back(symbol);
        reset_new_symbol();
        init_node->accept(this, lvl);
        _pf.DATA();
        if (_function_symbols.back()->qualifier() == tPUBLIC) {
          _pf.GLOBAL(_function_symbols.back()->name(), _pf.OBJ());
        }
        _pf.ALIGN();
        _pf.LABEL(symbol->name());
        _pf.SADDR(_function_lbl);
        _function_lbl.clear();
      } else {
        std::cerr << (node)->lineno() << ": error -> unexpected initializer" << std::endl;
        return;
      }
    }
    _declare_symbols.erase(symbol->name());
  }
}

void mml::postfix_writer::do_variable_node(cdk::variable_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  auto symbol = _symtab.find(node->name());

  if (symbol->is_foreign()) {
    _external_lbl = symbol->name();
  } else if (symbol->global()) {
    _pf.ADDR(symbol->name());
  } else {
    _pf.LOCAL(symbol->offset());
  }
}

void mml::postfix_writer::do_rvalue_node(cdk::rvalue_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->lvalue()->accept(this, lvl);

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.LDDOUBLE();
  } else {
    if (_external_lbl.empty()) {
      _pf.LDINT();
    }
  }
}

void mml::postfix_writer::do_assignment_node(cdk::assignment_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->rvalue()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    if (node->rvalue()->is_typed(cdk::TYPE_INT)) {
      _pf.I2D();
    }
    _pf.DUP64();
  } else {
    _pf.DUP32();
  }

  node->lvalue()->accept(this, lvl + 2);
  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.STDOUBLE();
  } else {
    _pf.STINT();
  }
}
