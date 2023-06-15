#include "targets/type_checker.h"
#include ".auto/all_nodes.h" // automatically generated
#include <cdk/types/functional_type.h>
#include <sstream>
#include <string>

#include <mml_parser.tab.h>

#define ASSERT_UNSPEC                                                                                                  \
  {                                                                                                                    \
    if (node->type() != nullptr && !node->is_typed(cdk::TYPE_UNSPEC))                                                  \
      return;                                                                                                          \
  }

static bool pointers_compatibility(std::shared_ptr<cdk::basic_type> lType, std::shared_ptr<cdk::basic_type> rType) {
  auto lt = lType;
  auto rt = rType;

  while (rt != nullptr && lt->name() == cdk::TYPE_POINTER && rt->name() == cdk::TYPE_POINTER) {
    lt = cdk::reference_type::cast(lt)->referenced();
    rt = cdk::reference_type::cast(rt)->referenced();
  }

  return rType == nullptr || lType->name() == rType->name();
}

static bool function_compatibility(std::shared_ptr<cdk::functional_type> ltype,
                                   std::shared_ptr<cdk::functional_type> rtype) {
  auto lname = ltype->output(0)->name();
  auto rname = rtype->output(0)->name();

  if (ltype->input_length() != rtype->input_length())
    return false;

  if (lname == cdk::TYPE_DOUBLE) {
    if (rname != cdk::TYPE_INT && rname != cdk::TYPE_DOUBLE)
      return false;
  } else if (lname == cdk::TYPE_POINTER) {
    if (rname != cdk::TYPE_POINTER || !pointers_compatibility(ltype->output(0), rtype->output(0)))
      return false;
  } else if (lname == cdk::TYPE_FUNCTIONAL) {
    if (rname != cdk::TYPE_FUNCTIONAL || !function_compatibility(cdk::functional_type::cast(ltype->output(0)),
                                                                 cdk::functional_type::cast(rtype->output(0))))
      return false;
  } else if (lname != rname) {
    return false;
  }

  for (size_t i = 0; i < ltype->input_length(); i++) {
    auto lname = ltype->input(i)->name();
    auto rname = rtype->input(i)->name();

    if (rname == cdk::TYPE_DOUBLE) {
      if (lname != cdk::TYPE_INT && lname != cdk::TYPE_DOUBLE)
        return false;
    } else if (lname == cdk::TYPE_POINTER) {
      if (rname != cdk::TYPE_POINTER || !pointers_compatibility(ltype->input(i), rtype->input(i)))
        return false;
    } else if (lname == cdk::TYPE_FUNCTIONAL) {
      if (rname != cdk::TYPE_FUNCTIONAL || !function_compatibility(cdk::functional_type::cast(ltype->input(i)),
                                                                   cdk::functional_type::cast(rtype->input(i))))
        return false;
    } else if (lname != rname) {
      return false;
    }
  }

  return true;
}

//--------------------------------------------------------------------------//
//                                CDK                                       //
//--------------------------------------------------------------------------//

void mml::type_checker::do_sequence_node(cdk::sequence_node *const node, int lvl) {
  for (size_t i = 0; i < node->size(); i++)
    node->node(i)->accept(this, lvl);
}

void mml::type_checker::do_nil_node(cdk::nil_node *const node, int lvl) {
  // EMPTY
}

void mml::type_checker::do_data_node(cdk::data_node *const node, int lvl) {
  // EMPTY
}

//--------------------------------------------------------------------------//
//                                LITERALS                                  //
//--------------------------------------------------------------------------//

void mml::type_checker::do_integer_node(cdk::integer_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::do_double_node(cdk::double_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
}

void mml::type_checker::do_string_node(cdk::string_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
}

void mml::type_checker::do_nullptr_node(mml::nullptr_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::reference_type::create(4, nullptr));
}

//--------------------------------------------------------------------------//
//                           PRINT AND INPUT                                //
//--------------------------------------------------------------------------//

void mml::type_checker::do_print_node(mml::print_node *const node, int lvl) {
  node->arguments()->accept(this, lvl + 2);
}

void mml::type_checker::do_input_node(mml::input_node *const node, int lvl) {
  node->type(cdk::primitive_type::create(0, cdk::TYPE_UNSPEC));
}

//--------------------------------------------------------------------------//
//                        UNARY ARITHMETIC EXPRESSIONS                      //
//--------------------------------------------------------------------------//

void mml::type_checker::do_identity_node(mml::identity_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);
  if (auto arg_type = node->argument()->type()->name(); arg_type != cdk::TYPE_INT && arg_type != cdk::TYPE_DOUBLE)
    throw std::string((node)->lineno() + ": error -> wrong type in argument of identity expression");
  node->type(node->argument()->type());
}

void mml::type_checker::do_neg_node(cdk::neg_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);
  if (auto arg_type = node->argument()->type()->name(); arg_type != cdk::TYPE_INT && arg_type != cdk::TYPE_DOUBLE)
    throw std::string((node)->lineno() + ": error -> wrong type in argument of negation expression");
    
  node->type(node->argument()->type());
}

void mml::type_checker::do_not_node(cdk::not_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);
  if (auto arg_type = node->argument()->type()->name(); arg_type != cdk::TYPE_INT)
    throw std::string((node)->lineno() + ": error -> wrong type in argument of not expression");
  node->type(node->argument()->type());
}

//--------------------------------------------------------------------------//
//                         BINARY ARITHMETIC EXPRESSIONS                    //
//--------------------------------------------------------------------------//

void mml::type_checker::do_add_node(cdk::add_node *const node, int lvl) { process_expr(node, true, lvl); }

void mml::type_checker::do_sub_node(cdk::sub_node *const node, int lvl) { process_expr(node, true, lvl); }

void mml::type_checker::do_mul_node(cdk::mul_node *const node, int lvl) { process_expr(node, false, lvl); }

void mml::type_checker::do_div_node(cdk::div_node *const node, int lvl) { process_expr(node, false, lvl); }

void mml::type_checker::do_mod_node(cdk::mod_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  if (!node->left()->is_typed(cdk::TYPE_INT)) {
    throw std::string((node)->lineno() + ": error -> mod left expression not integer");
  }
  node->right()->accept(this, lvl + 2);
  if (!node->right()->is_typed(cdk::TYPE_INT)) {
    throw std::string((node)->lineno() + ": error -> mod right expression not integer");
  }
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::process_expr(cdk::binary_operation_node *const node, bool process_pointers, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  auto leftType = node->left()->type();
  auto rightType = node->right()->type();

  if (leftType->name() == cdk::TYPE_DOUBLE && rightType->name() == cdk::TYPE_DOUBLE) {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  } else if ((leftType->name() == cdk::TYPE_DOUBLE || rightType->name() == cdk::TYPE_DOUBLE) &&
             (rightType->name() == cdk::TYPE_INT || leftType->name() == cdk::TYPE_INT)) {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  } else if (leftType->name() == cdk::TYPE_INT && rightType->name() == cdk::TYPE_INT) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else if (process_pointers && leftType->name() == cdk::TYPE_INT && rightType->name() == cdk::TYPE_POINTER) {
    node->type(rightType);
  } else if (process_pointers && leftType->name() == cdk::TYPE_POINTER && rightType->name() == cdk::TYPE_INT) {
    node->type(leftType);
  } else if (leftType->name() == cdk::TYPE_UNSPEC && rightType->name() == cdk::TYPE_UNSPEC) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else {
    throw std::string((node)->lineno() + ": error -> wrong types in binary expression");
  }
}

//--------------------------------------------------------------------------//
//                            LOGICAL EXPRESSIONS                           //
//--------------------------------------------------------------------------//

void mml::type_checker::do_lt_node(cdk::lt_node *const node, int lvl) {
  check_operand_type(node, lvl, {cdk::TYPE_INT, cdk::TYPE_DOUBLE});
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::do_le_node(cdk::le_node *const node, int lvl) {
  check_operand_type(node, lvl, {cdk::TYPE_INT, cdk::TYPE_DOUBLE});
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::do_ge_node(cdk::ge_node *const node, int lvl) {
  check_operand_type(node, lvl, {cdk::TYPE_INT, cdk::TYPE_DOUBLE});
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::do_gt_node(cdk::gt_node *const node, int lvl) {
  check_operand_type(node, lvl, {cdk::TYPE_INT, cdk::TYPE_DOUBLE});
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::do_and_node(cdk::and_node *const node, int lvl) {
  check_operand_type(node, lvl, {cdk::TYPE_INT});
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::do_or_node(cdk::or_node *const node, int lvl) {
  check_operand_type(node, lvl, {cdk::TYPE_INT});
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::do_ne_node(cdk::ne_node *const node, int lvl) {
  check_same_type(node, lvl);
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::do_eq_node(cdk::eq_node *const node, int lvl) {
  check_same_type(node, lvl);
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::check_operand_type(cdk::binary_operation_node *const node, int lvl,
                                           std::vector<cdk::typename_type> valid_types) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  bool is_left_type_valid = false;
  for (cdk::typename_type type : valid_types) {
    if (node->left()->type()->name() == type) {
      is_left_type_valid = true;
      break;
    }
  }
  if (!is_left_type_valid) {
    throw std::string((node)->lineno() + ": error -> unexpected type in binary logical expression (left)");
  }

  node->right()->accept(this, lvl + 2);
  bool is_right_type_valid = false;
  for (cdk::typename_type type : valid_types) {
    if (node->right()->type()->name() == type) {
      is_right_type_valid = true;
      break;
    }
  }
  if (!is_right_type_valid) {
    throw std::string((node)->lineno() + ": error -> unexpected type in binary logical expression (right)");
  }
}

void mml::type_checker::check_same_type(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);
  if (node->left()->type() != node->right()->type()) {
    throw std::string((node)->lineno() + ": error -> different types in binary logical expression");
  }
}

//---------------------------------------------------------------------------
//                     CONDITIONAL AND LOOP INSTRUCTIONS                   //
//---------------------------------------------------------------------------

void mml::type_checker::do_while_node(mml::while_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
  if (!node->condition()->is_typed(cdk::TYPE_INT))
    throw std::string((node)->lineno() + ": error -> expected integer condition");
}

void mml::type_checker::do_if_node(mml::if_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
  if (!node->condition()->is_typed(cdk::TYPE_INT))
    throw std::string((node)->lineno() + ": error -> expected integer condition");
}

void mml::type_checker::do_if_else_node(mml::if_else_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
  if (!node->condition()->is_typed(cdk::TYPE_INT))
    throw std::string((node)->lineno() + ": error -> expected integer condition");
}

void mml::type_checker::do_evaluation_node(mml::evaluation_node *const node, int lvl) {
  node->argument()->accept(this, lvl + 2);
}

//---------------------------------------------------------------------------
//                             CALL FUNCTION                               //
//---------------------------------------------------------------------------

void mml::type_checker::do_function_call_node(mml::function_call_node *const node, int lvl) {
  ASSERT_UNSPEC;

  std::vector<std::shared_ptr<cdk::basic_type>> input_types;
  std::shared_ptr<cdk::basic_type> output_type;

  if (!node->identifier()) {
    auto symbol = _symtab.find("@", 1);
    if (symbol == nullptr) {
      throw std::string((node)->lineno() + ": error -> function not declared");
    }
    if (symbol->is_main()) {
      throw std::string((node)->lineno() + ": error -> recursive call in main function");
    }
    input_types = cdk::functional_type::cast(symbol->type())->input()->components();
    output_type = cdk::functional_type::cast(symbol->type())->output(0);
  } else {
    node->identifier()->accept(this, lvl + 2);
    if (!(node->identifier()->type()->name() == cdk::TYPE_FUNCTIONAL)) {
      throw std::string((node)->lineno() + ": error -> expected function pointer on function call");
    }
    input_types = cdk::functional_type::cast(node->identifier()->type())->input()->components();
    output_type = cdk::functional_type::cast(node->identifier()->type())->output(0);
  }

  if (node->arguments()->size() != input_types.size()) {
    throw std::string((node)->lineno() + ": error -> argument count does not match function declaration");
  }

  node->type(output_type);
  node->arguments()->accept(this, lvl + 4);
  for (size_t i = 0; i < node->arguments()->size(); i++) {
    auto arg_type = node->argument(i)->type()->name();
    auto input_type = input_types[i]->name();
    if (arg_type == input_type || (input_type == cdk::TYPE_DOUBLE && arg_type == cdk::TYPE_INT)) {
      continue;
    }
    throw std::string((node)->lineno() + ": error -> argument type mismatch for argument " + i + 1);
  }
}

//---------------------------------------------------------------------------
//                            FUNCTION DEFINITION                          //
//---------------------------------------------------------------------------

void mml::type_checker::do_function_definition_node(mml::function_definition_node *const node, int lvl) {

  auto function = mml::make_symbol(node->type(), "@", 0, tPRIVATE);

  if (node->isMain()) {
    auto mainfun = mml::make_symbol(node->type(), "_main", 0, tPRIVATE);
    mainfun->set_main(true);
    function->set_main(true);
  }

  if (_symtab.find_local(function->name())) {
    _symtab.replace(function->name(), function);
  } else {
    if (!_symtab.insert(function->name(), function)) {
      throw std::string((node)->lineno() + ": error -> failed inserting function");
      return;
    }
  }
  _parent->set_new_symbol(function);
}

void mml::type_checker::do_return_node(mml::return_node *const node, int lvl) {
  auto symbol = _symtab.find("@", 1);
  std::shared_ptr<cdk::functional_type> rettype = nullptr;

  if (symbol == nullptr) {
    symbol = _symtab.find("_main", 0);
    if (symbol == nullptr || !node->retval() || !node->retval()->is_typed(cdk::TYPE_INT)) {
      throw std::string((node)->lineno() + ": error -> return statement must be inside a function or expected int value for return");
    }
    node->retval()->accept(this, lvl + 2);
  } else {
    if (node->retval()) {
      rettype = cdk::functional_type::cast(symbol->type());
      if (rettype->output() != nullptr && rettype->output(0)->name() == cdk::TYPE_VOID) {
        throw std::string((node)->lineno() + ": error -> void function cannot return a value");
      }
      node->retval()->accept(this, lvl + 2);
    }
  }

  if (node->retval() && rettype != nullptr && rettype->output() != nullptr) {
    const int expectedType = rettype->output(0)->name();
    const int actualType = node->retval()->type()->name();

    if (expectedType == actualType) {
      return;
    }

    if (expectedType == cdk::TYPE_DOUBLE && (actualType == cdk::TYPE_INT || actualType == cdk::TYPE_DOUBLE)) {
      return;
    }

    if (expectedType == cdk::TYPE_POINTER && actualType == cdk::TYPE_POINTER &&
        pointers_compatibility(rettype->output(0), node->retval()->type())) {
      return;
    }

    if (expectedType == cdk::TYPE_FUNCTIONAL && node->retval()->is_typed(cdk::TYPE_FUNCTIONAL) &&
        (function_compatibility(cdk::functional_type::cast(rettype->output(0)),
                                cdk::functional_type::cast(node->retval()->type())) ||
         (node->retval()->is_typed(cdk::TYPE_POINTER) &&
          cdk::reference_type::cast(node->retval()->type())->referenced() == nullptr))) {
      return;
    }

    throw std::string((node)->lineno() + ": error -> return type mismatch");
  }
}

//--------------------------------------------------------------------------//
//                           MEMORY                                         //
//--------------------------------------------------------------------------//

void mml::type_checker::do_address_of_node(mml::address_of_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->lvalue()->accept(this, lvl + 2);
  node->type(cdk::reference_type::create(4, node->lvalue()->type()));
}

void mml::type_checker::do_index_node(mml::index_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->base()->accept(this, lvl + 2);

  if (!node->base()->is_typed(cdk::TYPE_POINTER))
    throw std::string((node)->lineno() + ": error -> pointer expected in base");

  std::shared_ptr<cdk::reference_type> base = cdk::reference_type::cast(node->base()->type());

  node->index()->accept(this, lvl + 2);
  if (!node->index()->is_typed(cdk::TYPE_INT))
    throw std::string((node)->lineno() + ": error -> int expected in index");

  node->type(base->referenced());
}

void mml::type_checker::do_sizeof_node(mml::sizeof_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->expression()->accept(this, lvl + 2);
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::do_stack_alloc_node(mml::stack_alloc_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);

  if (!node->argument()->is_typed(cdk::TYPE_INT)) {
    throw std::string((node)->lineno() + ": error -> int expected in stack alloc");
  }

  node->type(cdk::reference_type::create(4, cdk::primitive_type::create(8, cdk::TYPE_DOUBLE)));
}

//--------------------------------------------------------------------------//
//                           NEXT AND STOP INSTRUCTIONS                     //
//--------------------------------------------------------------------------//

void mml::type_checker::do_stop_node(mml::stop_node *const node, int lvl) {}

void mml::type_checker::do_next_node(mml::next_node *const node, int lvl) {}

//--------------------------------------------------------------------------//
//                           BLOCK                                          //
//--------------------------------------------------------------------------//

void mml::type_checker::do_block_node(mml::block_node *const node, int lvl) {}

//--------------------------------------------------------------------------//
//                           VARIABLE                                       //
//--------------------------------------------------------------------------//

void mml::type_checker::do_variable_declaration_node(mml::variable_declaration_node *const node, int lvl) {
  if (node->initializer() != nullptr) {
    node->initializer()->accept(this, lvl + 2);
    if (node->type()) {
      const int variableType = node->type()->name();
      const int initializerType = node->initializer()->type()->name();

      if (variableType != initializerType &&
          !(variableType == cdk::TYPE_DOUBLE &&
            (initializerType == cdk::TYPE_INT || initializerType == cdk::TYPE_DOUBLE)) &&
          !(variableType == cdk::TYPE_POINTER && initializerType == cdk::TYPE_POINTER &&
            pointers_compatibility(node->type(), node->initializer()->type())) &&
          !(variableType == cdk::TYPE_FUNCTIONAL &&
            ((initializerType == cdk::TYPE_FUNCTIONAL &&
              function_compatibility(cdk::functional_type::cast(node->type()),
                                     cdk::functional_type::cast(node->initializer()->type()))) ||
             (initializerType == cdk::TYPE_POINTER &&
              cdk::reference_type::cast(node->initializer()->type())->referenced() == nullptr)))) {
        throw std::string((node)->lineno() + ": error -> initializer is not compatible with variable");
      }
    } else {
      node->type(node->initializer()->type());
    }
  }

  std::string id = node->identifier();
  if (id == "_main") {
    id = "._main";
  }

  auto symbol = mml::make_symbol(node->type(), id, (bool)node->initializer(), node->qualifier());
  std::shared_ptr<mml::symbol> previous = _symtab.find_local(symbol->name());

  if (previous) {
    const int previousType = previous->type()->name();
    const int symbolType = symbol->type()->name();

    if ((previousType == cdk::TYPE_FUNCTIONAL && symbolType == cdk::TYPE_FUNCTIONAL &&
         function_compatibility(cdk::functional_type::cast(previous->type()),
                                cdk::functional_type::cast(symbol->type()))) ||
        (previousType == cdk::TYPE_POINTER && symbolType == cdk::TYPE_POINTER &&
         pointers_compatibility(previous->type(), symbol->type())) ||
        previousType == symbolType) {
      _symtab.replace(symbol->name(), symbol);
    } else {
      throw std::string((node)->lineno() + ": error -> redefinition of '" + id + "'");
    }
  } else {
    _symtab.insert(id, symbol);
  }

  _parent->set_new_symbol(symbol);

  if (node->qualifier() == tFOREIGN) {
    symbol->set_foreign(true);
  }
}

void mml::type_checker::do_variable_node(cdk::variable_node *const node, int lvl) {
  ASSERT_UNSPEC;
  std::shared_ptr<mml::symbol> symbol = _symtab.find(node->name());
  if (symbol)
    node->type(symbol->type());
  else
    throw node->name();
}

void mml::type_checker::do_rvalue_node(cdk::rvalue_node *const node, int lvl) {
  ASSERT_UNSPEC;
  try {
    node->lvalue()->accept(this, lvl);
    node->type(node->lvalue()->type());
  } catch (const std::string &id) {
    throw std::string((node)->lineno() + ": error -> undeclared variable '" + id + "'");
  }
}

void mml::type_checker::do_assignment_node(cdk::assignment_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->lvalue()->accept(this, lvl + 4);
  node->rvalue()->accept(this, lvl + 4);

  int lType = node->lvalue()->type()->name();
  int rType = node->rvalue()->type()->name();

  bool compatible =
      lType == rType || rType == cdk::TYPE_UNSPEC || (lType == cdk::TYPE_DOUBLE && rType == cdk::TYPE_INT);

  if (lType == cdk::TYPE_POINTER) {
    compatible &= pointers_compatibility(node->lvalue()->type(), node->rvalue()->type());
  } else if (lType == cdk::TYPE_FUNCTIONAL) {
    compatible &=
        function_compatibility(cdk::functional_type::cast(node->lvalue()->type()),
                               cdk::functional_type::cast(node->rvalue()->type())) ||
        (rType == cdk::TYPE_POINTER && cdk::reference_type::cast(node->rvalue()->type())->referenced() == nullptr);
  }

  if (!compatible) {
    throw std::string((node)->lineno() + ": error -> wrong assignment");
  }

  node->type(node->lvalue()->type());
  if (rType == cdk::TYPE_UNSPEC)
    node->rvalue()->type(node->lvalue()->type());
}
