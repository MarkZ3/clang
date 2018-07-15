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

#include "ClangIndexRecordWriter.h"

#include "clang/Index/IndexUnitWriter.h"
#include "clang/Index/RecordingAction.h"
#include "clang/Index/UnitIndexDataConsumer.h"
#include "clang/Lex/HeaderSearch.h"

namespace clang {
class DiagnosticsEngine;

namespace index {

/// Persists the provided index data for a single translation unit out to the
/// file system.
class UnitIndexDataRecorder : public UnitIndexDataConsumer {
protected:
  UnitDetails UnitInfo;
  const CompilerInstance &CI;
  HeaderSearch &HS;
  DiagnosticsEngine &Diag;
  IndexUnitWriter UnitWriter;
  ClangIndexRecordWriter RecordWriter;

public:
  /// Intializes the index store at the provided path. Reports a diagnostic on
  /// failure.
  ///
  /// \returns true if store initialization failed.
  static bool initStore(StringRef StorePath, DiagnosticsEngine &Diag);

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

protected:
  Module *findModuleForHeader(const FileEntry *FE);

  // Initialization helper
  static IndexUnitWriter getUnitWriter(StringRef StorePath,
                                       const UnitDetails &UnitInfo);
};

} // end namespace index
} // end namespace clang

#endif
