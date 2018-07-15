//===- IndexingContext.h - Indexing context data ----------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_LIB_INDEX_INDEXINGCONTEXT_H
#define LLVM_CLANG_LIB_INDEX_INDEXINGCONTEXT_H

#include "clang/Basic/LLVM.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Index/IndexSymbol.h"
#include "clang/Index/IndexingAction.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"

namespace clang {
  class ASTContext;
  class Decl;
  class DeclGroupRef;
  class ImportDecl;
  class TagDecl;
  class TypeSourceInfo;
  class NamedDecl;
  class ObjCMethodDecl;
  class DeclContext;
  class NestedNameSpecifierLoc;
  class Stmt;
  class Expr;
  class TypeLoc;
  class DirectoryEntry;

namespace index {
  class IndexDataConsumer;

/// Tracks the current system root path and computes and caches whether a
/// file is considered a system file or not
class SystemFileCache {
  std::string SysrootPath;
  // Records whether a directory entry is system or not.
  llvm::DenseMap<const DirectoryEntry *, bool> DirEntries;
  // Keeps track of the last check for whether a FileID is system or
  // not. This is used to speed up isSystemFile() call.
  std::pair<FileID, bool> LastFileCheck;

public:
  SystemFileCache() = default;
  SystemFileCache(std::string SysrootPath);

  void setSysrootPath(StringRef path);
  StringRef getSysrootPath() const { return SysrootPath; }
  bool isSystem(FileID FID, SourceManager &SM);
};

/// Generates and reports indexing data to the provided \c IndexDataConsumer
/// for any AST nodes passed to its various \c index* methods.
class IndexingContext {
  IndexingOptions IndexOpts;
  SystemFileCache SystemCache;
  IndexDataConsumer &DataConsumer;
  ASTContext *Ctx = nullptr;

public:
  IndexingContext(IndexingOptions IndexOpts, IndexDataConsumer &DataConsumer)
      : IndexOpts(IndexOpts), DataConsumer(DataConsumer) {}

  const IndexingOptions &getIndexOpts() const { return IndexOpts; }
  SystemFileCache &getSystemCache() { return SystemCache; }
  IndexDataConsumer &getDataConsumer() { return DataConsumer; }

  void setASTContext(ASTContext &ctx) { Ctx = &ctx; }

  void setSysrootPath(StringRef path) { SystemCache.setSysrootPath(path); }
  StringRef getSysrootPath() const { return SystemCache.getSysrootPath(); }

  bool shouldIndex(const Decl *D);

  const LangOptions &getLangOpts() const;

  bool shouldSuppressRefs() const {
    return false;
  }

  bool shouldIndexFunctionLocalSymbols() const;

  bool shouldIndexImplicitTemplateInsts() const {
    return false;
  }

  static bool isTemplateImplicitInstantiation(const Decl *D);

  bool handleDecl(const Decl *D, SymbolRoleSet Roles = SymbolRoleSet(),
                  ArrayRef<SymbolRelation> Relations = None);

  bool handleDecl(const Decl *D, SourceLocation Loc,
                  SymbolRoleSet Roles = SymbolRoleSet(),
                  ArrayRef<SymbolRelation> Relations = None,
                  const DeclContext *DC = nullptr);

  bool handleReference(const NamedDecl *D, SourceLocation Loc,
                       const NamedDecl *Parent,
                       const DeclContext *DC,
                       SymbolRoleSet Roles = SymbolRoleSet(),
                       ArrayRef<SymbolRelation> Relations = None,
                       const Expr *RefE = nullptr,
                       const Decl *RefD = nullptr);

  bool importedModule(const ImportDecl *ImportD);

  bool indexDecl(const Decl *D);

  void indexTagDecl(const TagDecl *D,
                    ArrayRef<SymbolRelation> Relations = None);

  void indexTypeSourceInfo(TypeSourceInfo *TInfo, const NamedDecl *Parent,
                           const DeclContext *DC = nullptr,
                           bool isBase = false,
                           bool isIBType = false);

  void indexTypeLoc(TypeLoc TL, const NamedDecl *Parent,
                    const DeclContext *DC = nullptr,
                    bool isBase = false,
                    bool isIBType = false);

  void indexNestedNameSpecifierLoc(NestedNameSpecifierLoc NNS,
                                   const NamedDecl *Parent,
                                   const DeclContext *DC = nullptr);

  bool indexDeclContext(const DeclContext *DC);

  void indexBody(const Stmt *S, const NamedDecl *Parent,
                 const DeclContext *DC = nullptr);

  bool indexTopLevelDecl(const Decl *D);
  bool indexDeclGroupRef(DeclGroupRef DG);

private:
  bool shouldIgnoreIfImplicit(const Decl *D);

  bool handleDeclOccurrence(const Decl *D, SourceLocation Loc,
                            bool IsRef, const Decl *Parent,
                            SymbolRoleSet Roles,
                            ArrayRef<SymbolRelation> Relations,
                            const Expr *RefE,
                            const Decl *RefD,
                            const DeclContext *ContainerDC);
};

} // end namespace index
} // end namespace clang

#endif
