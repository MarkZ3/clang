//===--- RecordingAction.h - Frontend index recording action --------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_INDEX_INDEXRECORDINGACTION_H
#define LLVM_CLANG_INDEX_INDEXRECORDINGACTION_H

#include "clang/Basic/LLVM.h"
#include "clang/Index/UnitIndexingAction.h"
#include "llvm/ADT/ArrayRef.h"

namespace clang {
class CompilerInstance;
class FrontendAction;
class FrontendOptions;

namespace serialization {
class ModuleFile;
}

namespace index {

struct RecordingOptions : UnitIndexingOptions {
  std::string DataDirPath;
  bool RecordSymbolCodeGenName = false;
};

RecordingOptions
getRecordingOptionsFromFrontendOptions(const FrontendOptions &FEOpts);

/// \brief Creates a frontend action that collects dependency, file inclusion
/// and decl ocurrence information for the translation unit and persists it to
/// an index store.
///
/// FIXME: Not implemented yet.
///
/// \param WrappedAction another frontend action to wrap over or null.
std::unique_ptr<FrontendAction>
createIndexDataRecordingAction(RecordingOptions RecordOpts,
                               std::unique_ptr<FrontendAction> WrappedAction);

/// Collects dependency, file inclusion and decl occurrence information for a
/// \c ModuleFile and persists it to an index store. Does \b not check if
/// the store already has up-to-date information for the provided module file.
///
/// FIXME: Not implemented yet.
void recordIndexDataForModuleFile(serialization::ModuleFile *ModFile,
                                  RecordingOptions RecordOpts,
                                  const CompilerInstance &CI);

} // namespace index
} // namespace clang

#endif
