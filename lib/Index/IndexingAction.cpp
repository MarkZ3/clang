//===- IndexingAction.cpp - Frontend index action -------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "clang/Index/IndexingAction.h"
#include "clang/Index/RecordingAction.h"
#include "clang/Index/UnitIndexingAction.h"

#include "FileIndexData.h"
#include "IndexingContext.h"
#include "UnitIndexDataRecorder.h"
#include "clang/Basic/FileManager.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Frontend/FrontendDiagnostic.h"
#include "clang/Frontend/MultiplexConsumer.h"
#include "clang/Frontend/Utils.h"
#include "clang/Index/IndexDataConsumer.h"
#include "clang/Index/IndexDiagnostic.h"
#include "clang/Index/UnitIndexDataConsumer.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Serialization/ASTReader.h"
#include "llvm/Support/Path.h"

using namespace clang;
using namespace clang::index;

void UnitIndexDataConsumer::_anchor() {}

bool IndexDataConsumer::handleDeclOccurence(const Decl *D, SymbolRoleSet Roles,
                                            ArrayRef<SymbolRelation> Relations,
                                            SourceLocation Loc,
                                            bool IsInSystemFile,
                                            ASTNodeInfo ASTNode) {
  return true;
}

bool IndexDataConsumer::handleMacroOccurence(const IdentifierInfo *Name,
                                             const MacroInfo *MI,
                                             SymbolRoleSet Roles,
                                             SourceLocation Loc,
                                             bool IsInSystemFile) {
  return true;
}

bool IndexDataConsumer::handleModuleOccurence(const ImportDecl *ImportD,
                                              SymbolRoleSet Roles,
                                              SourceLocation Loc,
                                              bool IsInSystemFile) {
  return true;
}

namespace {

class IndexASTConsumer : public ASTConsumer {
  std::shared_ptr<Preprocessor> PP;
  IndexingContext &IndexCtx;

public:
  IndexASTConsumer(std::shared_ptr<Preprocessor> PP, IndexingContext &IndexCtx)
      : PP(std::move(PP)), IndexCtx(IndexCtx) {}

protected:
  void Initialize(ASTContext &Context) override {
    IndexCtx.setASTContext(Context);
    IndexCtx.getDataConsumer().initialize(Context);
    IndexCtx.getDataConsumer().setPreprocessor(PP);
  }

  bool HandleTopLevelDecl(DeclGroupRef DG) override {
    return IndexCtx.indexDeclGroupRef(DG);
  }

  void HandleInterestingDecl(DeclGroupRef DG) override {
    // Ignore deserialized decls.
  }

  void HandleTopLevelDeclInObjCContainer(DeclGroupRef DG) override {
    IndexCtx.indexDeclGroupRef(DG);
  }

  void HandleTranslationUnit(ASTContext &Ctx) override {
  }
};

/// Abstracts the core logic shared between \c IndexAction and
/// \c WrappingIndexAction frontend actions.
class IndexActionImpl {
public:
  virtual ~IndexActionImpl() = default;

  /// Called at the beginning of processing a single input, this creates the
  /// IndexASTConsumer object to use.
  ///
  /// \param CI The compiler instance used to process the input
  /// \returns the created IndexASTConsumer.
  virtual std::unique_ptr<IndexASTConsumer>
  createIndexASTConsumer(CompilerInstance &CI) = 0;

  /// Callback at the end of processing a single input.
  ///
  /// \param CI The compiler instance used to process the input. It will be the
  /// same instance as provided in \c createIndexASTConsumer.
  virtual void finish(CompilerInstance &CI) = 0;
};

class IndexAction : public ASTFrontendAction {
  std::unique_ptr<IndexActionImpl> Impl;

public:
  IndexAction(std::unique_ptr<IndexActionImpl> Impl) : Impl(std::move(Impl)) {}

protected:
  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                 StringRef InFile) override {
    return Impl->createIndexASTConsumer(CI);
  }

  void EndSourceFileAction() override {
    FrontendAction::EndSourceFileAction();
    Impl->finish(getCompilerInstance());
  }
};

class WrappingIndexAction : public WrapperFrontendAction {
  std::unique_ptr<IndexActionImpl> Impl;
  bool CreatedASTConsumer = false;

public:
  WrappingIndexAction(std::unique_ptr<FrontendAction> WrappedAction,
                      std::unique_ptr<IndexActionImpl> Impl)
      : WrapperFrontendAction(std::move(WrappedAction)), Impl(std::move(Impl)) {
  }

protected:
  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                 StringRef InFile) override {
    auto OtherConsumer = WrapperFrontendAction::CreateASTConsumer(CI, InFile);
    if (!OtherConsumer)
      return nullptr;

    std::vector<std::unique_ptr<ASTConsumer>> Consumers;
    Consumers.push_back(std::move(OtherConsumer));
    Consumers.push_back(Impl->createIndexASTConsumer(CI));
    CreatedASTConsumer = true;

    return llvm::make_unique<MultiplexConsumer>(std::move(Consumers));
  };

  void EndSourceFileAction() override {
    // Invoke wrapped action's method.
    WrapperFrontendAction::EndSourceFileAction();
    if (CreatedASTConsumer) {
      CreatedASTConsumer = false;
      Impl->finish(getCompilerInstance());
    }
  };
};

/// An implementation for \c IndexAction or \c WrappingIndexAction that provides
/// decl ocurrences information from the AST.
class DataConsumerActionImpl : public IndexActionImpl {
protected:
  std::shared_ptr<IndexDataConsumer> DataConsumer;
  IndexingContext IndexCtx;

public:
  DataConsumerActionImpl(std::shared_ptr<IndexDataConsumer> Consumer,
                         IndexingOptions Opts)
      : DataConsumer(std::move(Consumer)), IndexCtx(Opts, *DataConsumer) {}

  std::unique_ptr<IndexASTConsumer>
  createIndexASTConsumer(CompilerInstance &CI) override {
    IndexCtx.setSysrootPath(CI.getHeaderSearchOpts().Sysroot);
    return llvm::make_unique<IndexASTConsumer>(CI.getPreprocessorPtr(),
                                               IndexCtx);
  }

  void finish(CompilerInstance &CI) override { DataConsumer->finish(); }
};

} // anonymous namespace

std::unique_ptr<FrontendAction>
index::createIndexingAction(std::shared_ptr<IndexDataConsumer> DataConsumer,
                            IndexingOptions Opts,
                            std::unique_ptr<FrontendAction> WrappedAction) {
  auto ActionImpl =
      llvm::make_unique<DataConsumerActionImpl>(std::move(DataConsumer), Opts);
  if (WrappedAction)
    return llvm::make_unique<WrappingIndexAction>(std::move(WrappedAction),
                                                  std::move(ActionImpl));
  return llvm::make_unique<IndexAction>(std::move(ActionImpl));
}

static bool topLevelDeclVisitor(void *context, const Decl *D) {
  IndexingContext &IndexCtx = *static_cast<IndexingContext*>(context);
  return IndexCtx.indexTopLevelDecl(D);
}

static void indexTranslationUnit(ASTUnit &Unit, IndexingContext &IndexCtx) {
  Unit.visitLocalTopLevelDecls(&IndexCtx, topLevelDeclVisitor);
}

void index::indexASTUnit(ASTUnit &Unit, IndexDataConsumer &DataConsumer,
                         IndexingOptions Opts) {
  IndexingContext IndexCtx(Opts, DataConsumer);
  IndexCtx.setASTContext(Unit.getASTContext());
  DataConsumer.initialize(Unit.getASTContext());
  DataConsumer.setPreprocessor(Unit.getPreprocessorPtr());
  indexTranslationUnit(Unit, IndexCtx);
  DataConsumer.finish();
}

void index::indexTopLevelDecls(ASTContext &Ctx, ArrayRef<const Decl *> Decls,
                               IndexDataConsumer &DataConsumer,
                               IndexingOptions Opts) {
  IndexingContext IndexCtx(Opts, DataConsumer);
  IndexCtx.setASTContext(Ctx);

  DataConsumer.initialize(Ctx);
  for (const Decl *D : Decls)
    IndexCtx.indexTopLevelDecl(D);
  DataConsumer.finish();
}

void index::indexModuleFile(serialization::ModuleFile &Mod, ASTReader &Reader,
                            IndexDataConsumer &DataConsumer,
                            IndexingOptions Opts) {
  ASTContext &Ctx = Reader.getContext();
  IndexingContext IndexCtx(Opts, DataConsumer);
  IndexCtx.setASTContext(Ctx);
  DataConsumer.initialize(Ctx);

  for (const Decl *D : Reader.getModuleFileLevelDecls(Mod)) {
    IndexCtx.indexTopLevelDecl(D);
  }
  DataConsumer.finish();
}

//===----------------------------------------------------------------------===//
// Index Data Recording
//===----------------------------------------------------------------------===//

/// Construct a \c UnitDetails for a translation unit with the provided root
/// \c FileEntry or \c Module and with the provided sysroot path.
static index::UnitDetails getUnitDetails(const CompilerInstance &CI,
                                         std::string OutputFile,
                                         const FileEntry *RootFile,
                                         Module *UnitMod,
                                         StringRef SysrootPath) {
  std::string ModuleName =
      UnitMod ? UnitMod->getFullModuleName() : std::string();
  bool IsSystemUnit = UnitMod ? UnitMod->IsSystem : false;
  bool IsModuleUnit = UnitMod != nullptr;
  bool IsDebugCompilation = CI.getCodeGenOpts().OptimizationLevel == 0;

  // Ignore sysroot path if it points to root, otherwise every header will be
  // treated as system one.
  if (llvm::sys::path::root_path(SysrootPath) == SysrootPath)
    SysrootPath = "";

  return {CI,           UnitMod,      ModuleName,
          RootFile,     OutputFile,   SysrootPath,
          IsSystemUnit, IsModuleUnit, IsDebugCompilation};
}

/// Construct a \c UnitDetails from the invocation associated with the provided
/// \c CompilerInstance and the provided sysroot path.
static index::UnitDetails getUnitDetails(const CompilerInstance &CI,
                                         StringRef SysrootPath) {
  SourceManager &SM = CI.getASTContext().getSourceManager();

  std::string OutputFile = CI.getFrontendOpts().OutputFile;
  if (OutputFile.empty()) {
    OutputFile = CI.getFrontendOpts().Inputs[0].getFile();
    OutputFile += ".o";
  }

  const FileEntry *RootFile = nullptr;
  Module *UnitMod = nullptr;
  bool IsModuleGeneration = CI.getLangOpts().isCompilingModule();
  if (!IsModuleGeneration &&
      CI.getFrontendOpts().ProgramAction != frontend::GeneratePCH)
    RootFile = SM.getFileEntryForID(SM.getMainFileID());

  if (IsModuleGeneration) {
    HeaderSearch &HS = CI.getPreprocessor().getHeaderSearchInfo();
    UnitMod = HS.lookupModule(CI.getLangOpts().CurrentModule,
                              /*AllowSearch=*/false);
    assert(UnitMod && "only loaded modules should be indexed");
  }
  return getUnitDetails(CI, std::move(OutputFile), RootFile, UnitMod,
                        SysrootPath);
}

/// Construct a \c UnitDetails for the given module file.
static index::UnitDetails getUnitDetails(serialization::ModuleFile &Mod,
                                         const CompilerInstance &CI,
                                         StringRef SysrootPath) {
  HeaderSearch &HS = CI.getPreprocessor().getHeaderSearchInfo();
  Module *UnitMod = HS.lookupModule(Mod.ModuleName, /*AllowSearch=*/false);
  assert(UnitMod && "only loaded modules should be indexed");

  return getUnitDetails(CI, /*OutputFile=*/Mod.FileName, /*RootFile=*/nullptr,
                        UnitMod, SysrootPath);
}

namespace {

/// Collects and groups consumed index data by \c FileID.
class FileIndexDataCollector : public IndexDataConsumer {
  std::shared_ptr<Preprocessor> PP;
  typedef llvm::DenseMap<FileID, std::unique_ptr<FileIndexData>>
      IndexDataByFileTy;
  IndexDataByFileTy IndexDataByFile;

public:
  void setPreprocessor(std::shared_ptr<Preprocessor> PreProc) override {
    PP = PreProc;
  }

  IndexDataByFileTy::const_iterator begin() const {
    return IndexDataByFile.begin();
  }

  IndexDataByFileTy::const_iterator end() const {
    return IndexDataByFile.end();
  }

  bool empty() const { return IndexDataByFile.empty(); }

  bool handleDeclOccurence(const Decl *D, SymbolRoleSet Roles,
                           ArrayRef<SymbolRelation> Relations,
                           SourceLocation Loc, bool IsInSystemFile,
                           ASTNodeInfo ASTNode) override {
    ASTContext &Ctx = D->getASTContext();
    SourceManager &SM = Ctx.getSourceManager();
    Loc = SM.getFileLoc(Loc);
    FileID FID = SM.getFileID(Loc);
    unsigned Offset = SM.getFileOffset(Loc);

    // Ignore occurrences in the predefines buffer
    if (FID == PP->getPredefinesFileID())
      return true;

    FileIndexData &FileData = getFileIndexData(FID, IsInSystemFile);
    FileData.addDeclOccurence(Roles, Offset, D, Relations);
    return true;
  }

private:
  FileIndexData &getFileIndexData(FileID FID, bool IsInSystemFile) {
    auto &Entry = IndexDataByFile[FID];
    if (!Entry) {
      Entry.reset(new FileIndexData(FID, IsInSystemFile));
    }
    return *Entry;
  }
};

struct IncludeLocation {
  const FileEntry *Source;
  const FileEntry *Target;
  unsigned Line;
};

/// Preprocessor callbacks to collect file to file inclusion information
class IncludePPCallbacks : public PPCallbacks {
  SystemFileCache &SystemCache;
  UnitIndexingOptions::FileIncludeFilterKind FileIncludeFilter;
  std::vector<IncludeLocation> &Includes;
  SourceManager &SourceMgr;

public:
  IncludePPCallbacks(SystemFileCache &SystemCache,
                     UnitIndexingOptions::FileIncludeFilterKind IncludeFilter,
                     std::vector<IncludeLocation> &IncludesForFile,
                     SourceManager &SourceMgr)
      : SystemCache(SystemCache), FileIncludeFilter(IncludeFilter),
        Includes(IncludesForFile), SourceMgr(SourceMgr) {}

  virtual void InclusionDirective(SourceLocation HashLoc,
                                  const Token &IncludeTok, StringRef FileName,
                                  bool IsAngled, CharSourceRange FilenameRange,
                                  const FileEntry *File, StringRef SearchPath,
                                  StringRef RelativePath,
                                  const Module *Imported,
                                  SrcMgr::CharacteristicKind FileTy) override {
    if (HashLoc.isFileID() && File && File->isValid())
      addInclude(HashLoc, File);
  }

private:
  void addInclude(SourceLocation From, const FileEntry *To) {
    assert(To);
    if (FileIncludeFilter == UnitIndexingOptions::FileIncludeFilterKind::None)
      return;

    std::pair<FileID, unsigned> LocInfo =
        SourceMgr.getDecomposedExpansionLoc(From);

    if (LocInfo.first.isInvalid())
      return; // Ignore invalid locations.

    if (FileIncludeFilter ==
        UnitIndexingOptions::FileIncludeFilterKind::UserOnly)
      if (SystemCache.isSystem(LocInfo.first, SourceMgr))
        return; // Ignore includes of system headers.

    if (auto *FE = SourceMgr.getFileEntryForID(LocInfo.first)) {
      auto lineNo = SourceMgr.getLineNumber(LocInfo.first, LocInfo.second);
      Includes.push_back({FE, To, lineNo});
    }
  }
};

/// Abstract interface for providing the file and module dependencies of a
/// translation unit, as well as the set of file to file inclusions
class IndexDependencyProvider {
public:
  virtual ~IndexDependencyProvider() {}

  virtual void forEachFileDependency(
      const CompilerInstance &CI,
      llvm::function_ref<void(const FileEntry *FE, bool IsSystem)> Callback)
      const = 0;

  virtual void
  forEachInclude(llvm::function_ref<void(const FileEntry *Source, unsigned Line,
                                         const FileEntry *Target)>
                     Callback) const = 0;
  virtual void forEachModuleImport(
      const CompilerInstance &CI,
      llvm::function_ref<void(serialization::ModuleFile &Mod, bool IsSystem)>
          Callback) const = 0;
};

/// An IndexDependencyProvider for the index data collected by
/// \c FileIndexDependencyCollector.
class FileIndexDependencyProvider : public IndexDependencyProvider {
  llvm::SetVector<const FileEntry *> Files;
  llvm::BitVector IsSystemByUID;
  std::vector<IncludeLocation> Includes;
  bool IncludeSysModules;

public:
  FileIndexDependencyProvider(llvm::SetVector<const FileEntry *> Entries,
                              llvm::BitVector IsSystemByUID,
                              std::vector<IncludeLocation> Includes,
                              bool IncludeSysDeps)
      : Files(std::move(Entries)), IsSystemByUID(std::move(IsSystemByUID)),
        Includes(std::move(Includes)), IncludeSysModules(IncludeSysDeps) {}

  void forEachFileDependency(
      const CompilerInstance &CI,
      llvm::function_ref<void(const FileEntry *FE, bool IsSystem)> Callback)
      const override {
    for (auto *FE : Files)
      Callback(FE, isSystemFile(FE));
  }

  void
  forEachInclude(llvm::function_ref<void(const FileEntry *Source, unsigned Line,
                                         const FileEntry *Target)>
                     Callback) const override {
    for (auto &Include : Includes)
      Callback(Include.Source, Include.Line, Include.Target);
  }

  void forEachModuleImport(
      const CompilerInstance &CI,
      llvm::function_ref<void(serialization::ModuleFile &Mod, bool IsSystem)>
          Callback) const override {
    HeaderSearch &HS = CI.getPreprocessor().getHeaderSearchInfo();

    if (auto Reader = CI.getModuleManager()) {
      Reader->getModuleManager().visit(
          [&](serialization::ModuleFile &Mod) -> bool {
            bool IsSystemMod = false;
            if (Mod.isModule()) {
              if (auto *M =
                      HS.lookupModule(Mod.ModuleName, /*AllowSearch=*/false))
                IsSystemMod = M->IsSystem;
            }
            if (!IsSystemMod || IncludeSysModules)
              Callback(Mod, IsSystemMod);
            return true; // skip module dependencies.
          });
    }
  }

private:
  bool isSystemFile(const FileEntry *FE) const {
    auto UID = FE->getUID();
    return IsSystemByUID.size() > UID && IsSystemByUID[UID];
  }
};

/// Collects file and module dependency information for a translation unit,
/// including file to file inclusions.
class FileIndexDependencyCollector : public DependencyCollector {
  SystemFileCache &SystemCache;
  UnitIndexingOptions IndexOpts;
  llvm::SetVector<const FileEntry *> SeenFiles;
  llvm::BitVector IsSystemByUID;
  std::vector<IncludeLocation> Includes;
  SourceManager *SourceMgr = nullptr;

public:
  FileIndexDependencyCollector(SystemFileCache &SystemCache,
                               UnitIndexingOptions IndexOpts)
      : SystemCache(SystemCache), IndexOpts(IndexOpts) {}

  void attachToPreprocessor(Preprocessor &PP) override {
    DependencyCollector::attachToPreprocessor(PP);
    PP.addPPCallbacks(llvm::make_unique<IncludePPCallbacks>(
        SystemCache, IndexOpts.FileIncludeFilter, Includes,
        PP.getSourceManager()));
  }

  void setSourceManager(SourceManager *SourceMgr) {
    this->SourceMgr = SourceMgr;
  }

  FileIndexDependencyProvider consume() {
    return FileIndexDependencyProvider(
        std::move(SeenFiles), std::move(IsSystemByUID), std::move(Includes),
        IndexOpts.IncludeSystemDependencies);
  }

private:
  bool needSystemDependencies() override {
    return IndexOpts.IncludeSystemDependencies;
  }

  bool sawDependency(StringRef Filename, bool FromModule, bool IsSystem,
                     bool IsModuleFile, bool IsMissing) override {
    bool SawIt = DependencyCollector::sawDependency(
        Filename, FromModule, IsSystem, IsModuleFile, IsMissing);
    if (auto *FE = SourceMgr->getFileManager().getFile(Filename)) {
      if (SawIt)
        SeenFiles.insert(FE);

      // Record system-ness for all files that we pass through.
      if (IsSystemByUID.size() < FE->getUID() + 1)
        IsSystemByUID.resize(FE->getUID() + 1);
      IsSystemByUID[FE->getUID()] = IsSystem || isInSysroot(Filename);
    }
    return SawIt;
  }

  bool isInSysroot(StringRef Filename) {
    StringRef SysrootPath = SystemCache.getSysrootPath();
    return !SysrootPath.empty() && Filename.startswith(SysrootPath);
  }
};
} // anonymous namespace

static void reportData(const CompilerInstance &CI,
                       const FileIndexDataCollector &Collector,
                       const IndexDependencyProvider &DepProvider,
                       UnitDetails UnitInfo,
                       const IndexUnitDataConsumerFactory &UnitConsumerFactory,
                       const UnitIndexingOptions &IndexOpts) {

  std::unique_ptr<UnitIndexDataConsumer> Consumer =
      UnitConsumerFactory(UnitInfo);
  if (!Consumer)
    return;

  DepProvider.forEachFileDependency(
      CI, [&](const FileEntry *FE, bool IsSystemFile) {
        Consumer->handleFileDependency(FE, IsSystemFile);
      });
  DepProvider.forEachInclude(
      [&](const FileEntry *Source, unsigned Line, const FileEntry *Target) {
        Consumer->handleInclude(Source, Line, Target);
      });
  DepProvider.forEachModuleImport(
      CI, [&](serialization::ModuleFile &Mod, bool IsSystemMod) {
        Consumer->handleModuleImport(Mod, IsSystemMod);
        if (Mod.isModule() && Consumer->shouldIndexModuleDependency(Mod))
          indexModuleFile(Mod, CI, UnitConsumerFactory, IndexOpts);
      });

  for (auto I = Collector.begin(), E = Collector.end(); I != E; ++I) {
    FileID FID = I->first;
    const FileIndexData &FileData = *I->second;
    if (Consumer->handleFileOccurrences(
            FID, FileData.getDeclOccurrencesSortedByOffset(),
            FileData.isSystem()))
      return;
  }

  Consumer->finish();
}

namespace {

/// An implementation for IndexAction or WrappingIndexAction that gathers decl
/// occurrence, file inclusion and dependency information for the translation
/// unit and, optionally, its module dependencies.
class UnitDataConsumerActionImpl : public IndexActionImpl {
  UnitIndexingOptions IndexOpts;
  FileIndexDataCollector Collector;
  IndexingContext IndexCtx;
  FileIndexDependencyCollector DepCollector;
  IndexUnitDataConsumerFactory UnitConsumerFactory;

public:
  UnitDataConsumerActionImpl(UnitIndexingOptions UnitIndexOpts,
                             IndexUnitDataConsumerFactory UnitConsumerFactory)
      : IndexOpts(UnitIndexOpts), IndexCtx(UnitIndexOpts, Collector),
        DepCollector(IndexCtx.getSystemCache(), IndexOpts),
        UnitConsumerFactory(std::move(UnitConsumerFactory)) {}

  std::unique_ptr<IndexASTConsumer>
  createIndexASTConsumer(CompilerInstance &CI) override {
    IndexCtx.setSysrootPath(CI.getHeaderSearchOpts().Sysroot);

    std::shared_ptr<Preprocessor> PP = CI.getPreprocessorPtr();
    Collector.setPreprocessor(PP);
    DepCollector.setSourceManager(&CI.getSourceManager());
    DepCollector.attachToPreprocessor(CI.getPreprocessor());

    return llvm::make_unique<IndexASTConsumer>(PP, IndexCtx);
  }

  /// Provides the collected indexing info to the \c IndexUnitDataConsumer
  void finish(CompilerInstance &CI) override {
    // The consumer may emit more diagnostics so do the begin/end source file
    // invocations on the diagnostic client.
    // FIXME: FrontendAction::EndSourceFile() should probably not call
    // CI.getDiagnosticClient().EndSourceFile()' until after it has called
    // 'EndSourceFileAction()', so that code executing during
    // EndSourceFileAction() can emit diagnostics. If this is fixed,
    // DiagClientBeginEndRAII can go away.
    struct DiagClientBeginEndRAII {
      CompilerInstance &CI;
      DiagClientBeginEndRAII(CompilerInstance &CI) : CI(CI) {
        CI.getDiagnosticClient().BeginSourceFile(CI.getLangOpts());
      }
      ~DiagClientBeginEndRAII() { CI.getDiagnosticClient().EndSourceFile(); }
    } diagClientBeginEndRAII(CI);

    Collector.finish();
    reportData(CI, Collector, DepCollector.consume(),
               getUnitDetails(CI, IndexCtx.getSysrootPath()),
               UnitConsumerFactory, IndexOpts);
  }
};

/// Provides the file and module dependency information for a \c ModuleFile
class ModuleFileIndexDependencyCollector : public IndexDependencyProvider {
  serialization::ModuleFile &ModFile;
  bool CollectSystemDependencies;

public:
  ModuleFileIndexDependencyCollector(serialization::ModuleFile &Mod,
                                     bool CollectSystemDependencies)
      : ModFile(Mod), CollectSystemDependencies(CollectSystemDependencies) {}

  void forEachFileDependency(
      const CompilerInstance &CI,
      llvm::function_ref<void(const FileEntry *FE, bool IsSystem)> Callback)
      const override {
    auto Reader = CI.getModuleManager();
    Reader->visitInputFiles(
        ModFile, CollectSystemDependencies, /*Complain=*/false,
        [&](const serialization::InputFile &IF, bool IsSystem) {
          auto *FE = IF.getFile();
          if (!FE)
            return;
          // Ignore module map files, they are not as important to track as
          // source files and they may be auto-generated which would create an
          // undesirable dependency on an intermediate build byproduct.
          if (FE->getName().endswith("module.modulemap"))
            return;

          Callback(FE, IsSystem);
        });
  }

  void
  forEachInclude(llvm::function_ref<void(const FileEntry *Source, unsigned Line,
                                         const FileEntry *Target)>
                     Callback) const override {
    // FIXME: Module files without a preprocessing record do not have info about
    // include locations. Serialize enough data to be able to retrieve such
    // info.
  }

  void forEachModuleImport(
      const CompilerInstance &CI,
      llvm::function_ref<void(serialization::ModuleFile &Mod, bool IsSystem)>
          Callback) const override {
    HeaderSearch &HS = CI.getPreprocessor().getHeaderSearchInfo();
    for (auto *Mod : ModFile.Imports) {
      bool IsSystemMod = false;
      if (auto *M = HS.lookupModule(Mod->ModuleName, /*AllowSearch=*/false))
        IsSystemMod = M->IsSystem;
      if (!IsSystemMod || CollectSystemDependencies)
        Callback(*Mod, IsSystemMod);
    }
  }
};
} // anonymous namespace.

void index::indexModuleFile(serialization::ModuleFile &Mod,
                            const CompilerInstance &CI,
                            IndexUnitDataConsumerFactory UnitConsumerFactory,
                            UnitIndexingOptions IndexOpts) {

  DiagnosticsEngine &Diag = CI.getDiagnostics();
  Diag.Report(Mod.ImportLoc, diag::remark_index_producing_module_file_data)
      << Mod.FileName;

  FileIndexDataCollector Collector;
  IndexingContext ModIndexCtx(IndexOpts, Collector);

  auto &ASTCtx = CI.getASTContext();
  Collector.initialize(ASTCtx);
  Collector.setPreprocessor(CI.getPreprocessorPtr());
  ModIndexCtx.setASTContext(ASTCtx);
  ModIndexCtx.setSysrootPath(CI.getHeaderSearchOpts().Sysroot);

  for (const Decl *D : CI.getModuleManager()->getModuleFileLevelDecls(Mod))
    ModIndexCtx.indexTopLevelDecl(D);

  Collector.finish();

  ModuleFileIndexDependencyCollector DepCollector(
      Mod, IndexOpts.IncludeSystemDependencies);

  reportData(CI, Collector, DepCollector,
             getUnitDetails(Mod, CI, ModIndexCtx.getSysrootPath()),
             UnitConsumerFactory, IndexOpts);
}

std::unique_ptr<FrontendAction>
index::createUnitIndexingAction(IndexUnitDataConsumerFactory ConsumerFactory,
                                UnitIndexingOptions IndexOpts,
                                std::unique_ptr<FrontendAction> WrappedAction) {
  auto ActionImpl = llvm::make_unique<UnitDataConsumerActionImpl>(
      std::move(IndexOpts), ConsumerFactory);
  if (WrappedAction)
    return llvm::make_unique<WrappingIndexAction>(std::move(WrappedAction),
                                                  std::move(ActionImpl));
  return llvm::make_unique<IndexAction>(std::move(ActionImpl));
};

std::unique_ptr<FrontendAction> index::createIndexDataRecordingAction(
    RecordingOptions RecordOpts,
    std::unique_ptr<FrontendAction> WrappedAction) {

  auto ConsumerFactory =
      [RecordOpts](
          UnitDetails UnitInfo) -> std::unique_ptr<UnitIndexDataConsumer> {
    return llvm::make_unique<UnitIndexDataRecorder>(std::move(UnitInfo),
                                                    RecordOpts);
  };
  return createUnitIndexingAction(ConsumerFactory, std::move(RecordOpts),
                                  std::move(WrappedAction));
};

RecordingOptions
index::getRecordingOptionsFromFrontendOptions(const FrontendOptions &FEOpts) {
  RecordingOptions RecordOpts;
  RecordOpts.DataDirPath = FEOpts.IndexStorePath;
  if (FEOpts.IndexIgnoreSystemSymbols) {
    RecordOpts.SystemSymbolFilter =
        index::IndexingOptions::SystemSymbolFilterKind::None;
  }
  RecordOpts.RecordSymbolCodeGenName = FEOpts.IndexRecordCodegenName;
  return RecordOpts;
}

void index::recordIndexDataForModuleFile(serialization::ModuleFile *ModFile,
                                         RecordingOptions RecordOpts,
                                         const CompilerInstance &CI) {
  auto UnitConsumerFactory = [RecordOpts](UnitDetails UnitInfo) {
    return llvm::make_unique<UnitIndexDataRecorder>(std::move(UnitInfo),
                                                    RecordOpts);
  };
  return indexModuleFile(*ModFile, CI, UnitConsumerFactory,
                         std::move(RecordOpts));
}
