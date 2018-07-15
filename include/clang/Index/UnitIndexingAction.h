//===--- UnitIndexingAction.h - Frontend unit indexing action -------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_INDEX_UNITINDEXINGACTION_H
#define LLVM_CLANG_INDEX_UNITINDEXINGACTION_H

#include "clang/Basic/LLVM.h"
#include "clang/Index/IndexingAction.h"
#include "llvm/ADT/ArrayRef.h"

namespace clang {
class CompilerInstance;
class FileEntry;
class FrontendAction;
class Module;

namespace serialization {
class ModuleFile;
}

namespace index {
class UnitIndexDataConsumer;

struct UnitIndexingOptions : IndexingOptions {
  enum class FileIncludeFilterKind {
    None,
    UserOnly, // only record includes inside non-system files.
    All,
  };

  bool IncludeSystemDependencies = true;
  FileIncludeFilterKind FileIncludeFilter = FileIncludeFilterKind::UserOnly;
};

/// \brief Information about a translation unit useful for indexing.
///
struct UnitDetails {
  const CompilerInstance &CI; ///< The owning compiler instance.

  Module *UnitModule;     ///< The corresponding \c Module (module units only).
  std::string ModuleName; ///< The \c Module name (module units only).
  const FileEntry *RootFile; ///< The root \c FileEntry (non-module units only).

  std::string OutputFile; ///< The output file path.
  StringRef SysrootPath;  ///< The "virtual system root" path.
  bool IsSystemUnit;
  bool IsModuleUnit;
  bool IsDebugCompilation;
};

/// Factory function type for producing UnitIndexDataConsumers for a given
/// translation unit
typedef std::function<std::unique_ptr<UnitIndexDataConsumer>(
    UnitDetails UnitInfo)>
    IndexUnitDataConsumerFactory;

/// \brief Creates a frontend action that provides dependency, file inclusion
/// and decl ocurrence information for the translation unit, and optionally its
/// module dependencies.
///
/// Decl occurrence information is provided per-file, sorted by offset.
///
/// \param ConsumerFactory provides an \c IndexUnitDataConsumer to use for a
/// translation unit.
/// \param WrappedAction another frontend action to wrap over or null.
std::unique_ptr<FrontendAction>
createUnitIndexingAction(IndexUnitDataConsumerFactory ConsumerFactory,
                         UnitIndexingOptions UnitIndexOpts,
                         std::unique_ptr<FrontendAction> WrappedAction);

/// Collects and provides dependency, file inclusion and decl occurrence
/// information for a \c ModuleFile to an \c IndexUnitDataConsumer constructed
/// from the provided \c IndexUnitDataConsumerFactory.
void indexModuleFile(serialization::ModuleFile &Mod, const CompilerInstance &CI,
                     IndexUnitDataConsumerFactory UnitConsumerFactory,
                     UnitIndexingOptions Opts);

} // namespace index
} // namespace clang

#endif
