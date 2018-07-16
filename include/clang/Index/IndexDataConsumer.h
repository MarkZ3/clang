//===--- IndexDataConsumer.h - Abstract index data consumer ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_INDEX_INDEXDATACONSUMER_H
#define LLVM_CLANG_INDEX_INDEXDATACONSUMER_H

#include "clang/Index/IndexSymbol.h"
#include "clang/Lex/Preprocessor.h"

namespace clang {
  class ASTContext;
  class DeclContext;
  class Expr;
  class FileID;
  class IdentifierInfo;
  class ImportDecl;
  class MacroInfo;

namespace index {

// Hack so that we can use the same Clangd branch with Clang that might or
// might not have the libIndexStore patches
#define HAS_SYS_FILE_PARAM

class IndexDataConsumer {
public:
  struct ASTNodeInfo {
    const Expr *OrigE;
    const Decl *OrigD;
    const Decl *Parent;
    const DeclContext *ContainerDC;
  };

  virtual ~IndexDataConsumer() {}

  virtual void initialize(ASTContext &Ctx) {}

  virtual void setPreprocessor(std::shared_ptr<Preprocessor> PP) {}

  /// \returns true to continue indexing, or false to abort.
  virtual bool handleDeclOccurence(const Decl *D, SymbolRoleSet Roles,
                                   ArrayRef<SymbolRelation> Relations,
                                   SourceLocation Loc, bool IsInSystemFile,
                                   ASTNodeInfo ASTNode);

  /// \returns true to continue indexing, or false to abort.
  virtual bool handleMacroOccurence(const IdentifierInfo *Name,
                                    const MacroInfo *MI, SymbolRoleSet Roles,
                                    SourceLocation Loc, bool IsInSystemFile);

  /// \returns true to continue indexing, or false to abort.
  virtual bool handleModuleOccurence(const ImportDecl *ImportD,
                                     SymbolRoleSet Roles, SourceLocation Loc,
                                     bool IsInSystemFile);

  virtual void finish() {}
};

} // namespace index
} // namespace clang

#endif
