//===--- UnitIndexDataRecorder.cpp - Persist index data to the file system ===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "UnitIndexDataRecorder.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/TargetOptions.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Index/IndexDiagnostic.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Serialization/ASTReader.h"
#include "llvm/Support/Path.h"

using namespace clang;
using namespace clang::index;

bool UnitIndexDataRecorder::initStore(StringRef StorePath,
                                      DiagnosticsEngine &Diag) {
  std::string Error;
  if (IndexUnitWriter::initIndexDirectory(StorePath, Error)) {
    Diag.Report(diag::err_index_store_dir_create_failed) << StorePath << Error;
    return true;
  }
  return false;
}

UnitIndexDataRecorder::UnitIndexDataRecorder(UnitDetails UnitInfo,
                                             RecordingOptions RecordOpts)
    : UnitInfo(UnitInfo), CI(UnitInfo.CI),
      HS(CI.getPreprocessor().getHeaderSearchInfo()), Diag(CI.getDiagnostics()),
      UnitWriter(getUnitWriter(RecordOpts.DataDirPath, UnitInfo)),
      RecordWriter(CI.getASTContext(), std::move(RecordOpts)) {}

void UnitIndexDataRecorder::handleFileDependency(const FileEntry *FE,
                                                 bool IsSystem) {
  UnitWriter.addFileDependency(FE, IsSystem, findModuleForHeader(FE));
}

void UnitIndexDataRecorder::handleInclude(const FileEntry *Source,
                                          unsigned int Line,
                                          const FileEntry *Target) {
  UnitWriter.addInclude(Source, Line, Target);
}

void UnitIndexDataRecorder::handleModuleImport(
    const serialization::ModuleFile &Mod, bool IsSystem) {
  Module *UnitMod = HS.lookupModule(Mod.ModuleName, /*AllowSearch=*/false);
  UnitWriter.addASTFileDependency(Mod.File, IsSystem, UnitMod);
}

bool UnitIndexDataRecorder::shouldIndexModuleDependency(
    const serialization::ModuleFile &Mod) {
  std::string Error;
  // We don't timestamp check with the PCM file on purpose. The PCM may get
  // touched for various reasons which would cause unnecessary work to emit
  // index data. User modules will normally get rebuilt and have their index
  // data re-emitted, and system modules are generally stable (and can also get
  // rebuilt along with their index data).
  auto IsUpToDateOpt =
      UnitWriter.isUnitUpToDateForOutputFile(Mod.FileName, None, Error);
  if (!IsUpToDateOpt.hasValue()) {
    Diag.Report(diag::err_index_store_file_status_failed) << Error;
    return false;
  }
  return !*IsUpToDateOpt;
};

bool UnitIndexDataRecorder::handleFileOccurrences(
    FileID FID, ArrayRef<DeclOccurrence> Occurs, bool IsSystem) {
  auto *FE = CI.getSourceManager().getFileEntryForID(FID);
  std::string RecordFile;
  std::string Error;

  if (RecordWriter.writeRecord(FE->getName(), FID, Occurs, Error,
                               &RecordFile)) {
    Diag.Report(diag::err_index_store_record_write_failed)
        << RecordFile << Error;
    return false;
  }
  UnitWriter.addRecordFile(RecordFile, FE, IsSystem, findModuleForHeader(FE));
  return false;
};

void UnitIndexDataRecorder::finish() {
  std::string Error;
  if (UnitWriter.write(Error)) {
    Diag.Report(diag::err_index_store_unit_write_failed) << Error;
    return;
  }
};

/// A callback for \c UnitWriter to get module information for an \c
/// OpaqueModule.
static writer::ModuleInfo getModuleInfo(writer::OpaqueModule Mod,
                                        SmallVectorImpl<char> &Scratch) {
  assert(Mod);
  writer::ModuleInfo Info;
  std::string FullName = static_cast<const Module *>(Mod)->getFullModuleName();
  unsigned offset = Scratch.size();
  Scratch.append(FullName.begin(), FullName.end());
  Info.Name = StringRef(Scratch.data() + offset, FullName.size());
  return Info;
}

static std::string getClangVersion() {
  // Try picking the version from an Apple Clang tag.
  std::string RepositoryPath = getClangRepositoryPath();
  StringRef BuildNumber = RepositoryPath;
  size_t DashOffset = BuildNumber.find('-');
  if (BuildNumber.startswith("clang") && DashOffset != StringRef::npos) {
    BuildNumber = BuildNumber.substr(DashOffset + 1);
    return BuildNumber;
  }
  // Fallback to the generic version.
  return CLANG_VERSION_STRING;
}

Module *UnitIndexDataRecorder::findModuleForHeader(const FileEntry *FE) {
  if (!UnitInfo.UnitModule)
    return nullptr;

  HeaderSearch &HS = CI.getPreprocessor().getHeaderSearchInfo();
  if (auto Mod = HS.findModuleForHeader(FE).getModule())
    if (Mod->isSubModuleOf(UnitInfo.UnitModule))
      return Mod;
  return nullptr;
}

IndexUnitWriter
UnitIndexDataRecorder::getUnitWriter(StringRef DataPath,
                                     const UnitDetails &UnitInfo) {
  return IndexUnitWriter(
      UnitInfo.CI.getFileManager(), DataPath, "clang", getClangVersion(),
      UnitInfo.OutputFile, UnitInfo.ModuleName, UnitInfo.RootFile,
      UnitInfo.IsSystemUnit, UnitInfo.IsModuleUnit, UnitInfo.IsDebugCompilation,
      UnitInfo.CI.getTargetOpts().Triple, UnitInfo.SysrootPath, getModuleInfo);
}
