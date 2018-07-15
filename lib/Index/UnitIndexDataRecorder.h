//===--- UnitIndexDataRecorder.h - Persist index data to the file system --===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_LIB_INDEX_UNITINDEXDATARECORDER_H
#define LLVM_CLANG_LIB_INDEX_UNITINDEXDATARECORDER_H

#include "clang/Index/RecordingAction.h"
#include "clang/Index/UnitIndexDataConsumer.h"

namespace clang {
class DiagnosticsEngine;
class FrontendOptions;

namespace index {

/// Persists the provided index data for a single translation unit out to the
/// file system.
class UnitIndexDataRecorder : public UnitIndexDataConsumer {
protected:
  UnitDetails UnitInfo;

public:
  UnitIndexDataRecorder(UnitDetails UnitInfo, RecordingOptions RecordOpts);

  void handleFileDependency(const FileEntry *FE, bool IsSystem) override;

  void handleInclude(const FileEntry *Source, unsigned Line,
                     const FileEntry *Target) override;

  void handleModuleImport(const serialization::ModuleFile &Mod,
                          bool IsSystem) override;

  bool
  shouldIndexModuleDependency(const serialization::ModuleFile &Mod) override;

  bool handleFileOccurrences(FileID FID,
                             ArrayRef<DeclOccurrence> OccurrencesSortedByOffset,
                             bool IsSystem) override;

  void finish() override;
};

} // end namespace index
} // end namespace clang

#endif
