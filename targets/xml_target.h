#ifndef __MML_TARGETS_XML_TARGET_H__
#define __MML_TARGETS_XML_TARGET_H__

#include "targets/xml_writer.h"
#include <cdk/ast/basic_node.h>
#include <cdk/targets/basic_target.h>

namespace mml {

  class xml_target : public cdk::basic_target {
    static xml_target _self;

  private:
    xml_target() : cdk::basic_target("xml") {}

  public:
    bool evaluate(std::shared_ptr<cdk::compiler> compiler) {
      // this symbol table will be used to check identifiers
      // an exception will be thrown if identifiers are used before declaration
      cdk::symbol_table<mml::symbol> symtab;

      xml_writer writer(compiler, symtab);
      compiler->ast()->accept(&writer, 0);
      return true;
    }
  };

} // namespace mml

#endif
