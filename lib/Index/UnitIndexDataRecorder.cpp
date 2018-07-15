//===--- UnitIndexDataRecorder.cpp - Persist index data to the file system ===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "UnitIndexDataRecorder.h"
#include "clang/Frontend/FrontendOptions.h"

using namespace clang;
using namespace clang::index;

UnitIndexDataRecorder::UnitIndexDataRecorder(UnitDetails UnitDetails,
                                             RecordingOptions RecordOpts)
    : UnitInfo(UnitDetails) {
  // TODO
}

void UnitIndexDataRecorder::handleFileDependency(const FileEntry *FE,
                                                 bool IsSystem) {
  // TODO
}

void UnitIndexDataRecorder::handleInclude(const FileEntry *Source,
                                          unsigned int Line,
                                          const FileEntry *Target) {
  // TODO
}

void UnitIndexDataRecorder::handleModuleImport(
    const serialization::ModuleFile &Mod, bool IsSystem) {
  // TODO
}

bool UnitIndexDataRecorder::shouldIndexModuleDependency(
    const serialization::ModuleFile &Mod) {
  // TODO
  return true;
};

bool UnitIndexDataRecorder::handleFileOccurrences(
    FileID FID, ArrayRef<DeclOccurrence> Occurs, bool IsSystem) {
  // TODO
  return false;
};

void UnitIndexDataRecorder::finish(){
    // TODO
};
