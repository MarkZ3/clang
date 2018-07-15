//===--- UnitIndexDataConsumer.h - Abstract unit index data consumer ------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_INDEX_UNITINDEXDATACONSUMER_H
#define LLVM_CLANG_INDEX_UNITINDEXDATACONSUMER_H

#include "clang/Basic/SourceLocation.h"
#include "clang/Index/DeclOccurrence.h"
#include "clang/Index/IndexSymbol.h"
#include "llvm/ADT/ArrayRef.h"

namespace clang {
namespace serialization {
class ModuleFile;
}

namespace index {

/// Consumer for the index data associated with a translation unit.
class UnitIndexDataConsumer {
public:
  virtual ~UnitIndexDataConsumer() = default;

  /// Called for each file dependency of the translation unit.
  virtual void handleFileDependency(const FileEntry *FE, bool IsSystem) {}

  /// Called for each file include in the translation unit.
  virtual void handleInclude(const FileEntry *Source, unsigned Line,
                             const FileEntry *Target) {}

  /// Called for each each module imported by the translation unit.
  virtual void handleModuleImport(const serialization::ModuleFile &Mod,
                                  bool IsSystem) {}

  /// Determines whether to collect the index data associated with the given
  /// dependency of this translation unit or not.
  ///
  /// \param OutFilePath the output file path of the dependency.
  /// \returns true to collect index data for \c Mod.
  virtual bool
  shouldIndexModuleDependency(const serialization::ModuleFile &Mod) {
    return false;
  }

  /// Called with the decl occurrences in each file and AST file dependency,
  /// sorted by offset.
  ///
  /// \returns true to cancel consuming data for this translation unit. Finish
  /// will not be called.
  virtual bool
  handleFileOccurrences(FileID FID,
                        ArrayRef<DeclOccurrence> OccurrencesSortedByOffset,
                        bool IsSystem) {
    return false;
  }

  /// Called when there is no more data to handle.
  virtual void finish() {}

private:
  // avoid duplicate vtables
  virtual void _anchor();
};

} // namespace index
} // namespace clang

#endif
