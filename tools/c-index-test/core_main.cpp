//===-- core_main.cpp - Core Index Tool testbed ---------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "clang/CodeGen/ObjectFilePCHContainerOperations.h"
#include "clang/Frontend/ASTUnit.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/CompilerInvocation.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Index/CodegenNameGenerator.h"
#include "clang/Index/IndexDataConsumer.h"
#include "clang/Index/USRGeneration.h"
#include "clang/Index/UnitIndexDataConsumer.h"
#include "clang/Index/UnitIndexingAction.h"
#include "clang/Serialization/ASTReader.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/raw_ostream.h"

using namespace clang;
using namespace clang::index;
using namespace llvm;

extern "C" int indextest_core_main(int argc, const char **argv);

namespace {

enum class ActionType {
  None,
  PrintSourceSymbols,
  PrintSourceUnit,
};

namespace options {

static cl::OptionCategory IndexTestCoreCategory("index-test-core options");

static cl::opt<ActionType> Action(
    cl::desc("Action:"), cl::init(ActionType::None),
    cl::values(clEnumValN(ActionType::PrintSourceSymbols,
                          "print-source-symbols", "Print symbols from source"),
               clEnumValN(ActionType::PrintSourceUnit, "print-source-unit",
                          "Print unit info from source")),
    cl::cat(IndexTestCoreCategory));

static cl::extrahelp MoreHelp(
  "\nAdd \"-- <compiler arguments>\" at the end to setup the compiler "
  "invocation\n"
);

static cl::opt<bool>
DumpModuleImports("dump-imported-module-files",
               cl::desc("Print symbols and input files from imported modules"));

static cl::opt<bool>
IncludeLocals("include-locals", cl::desc("Print local symbols"));

static cl::opt<std::string>
ModuleFilePath("module-file",
               cl::desc("Path to module file to print symbols from"));
static cl::opt<std::string>
  ModuleFormat("fmodule-format", cl::init("raw"),
        cl::desc("Container format for clang modules and PCH, 'raw' or 'obj'"));

}
} // anonymous namespace

static void printSymbolInfo(SymbolInfo SymInfo, raw_ostream &OS);
static void printSymbolNameAndUSR(const Decl *D, ASTContext &Ctx,
                                  raw_ostream &OS);

static void printDeclOccurrence(const Decl *D, SymbolRoleSet Roles,
                                ArrayRef<SymbolRelation> Relations, FileID FID,
                                unsigned Offset, bool IsInSystemFile,
                                CodegenNameGenerator &CGNameGen,
                                raw_ostream &OS) {
  ASTContext &Ctx = D->getASTContext();
  SourceManager &SM = Ctx.getSourceManager();

  unsigned Line = SM.getLineNumber(FID, Offset);
  unsigned Col = SM.getColumnNumber(FID, Offset);
  OS << Line << ':' << Col << " | ";

  printSymbolInfo(getSymbolInfo(D), OS);
  OS << " | ";

  printSymbolNameAndUSR(D, Ctx, OS);
  OS << " | ";

  if (CGNameGen.writeName(D, OS))
    OS << "<no-cgname>";
  OS << " | ";

  printSymbolRoles(Roles, OS);
  OS << " | ";

  OS << "rel: " << Relations.size() << '\n';

  for (auto &SymRel : Relations) {
    OS << '\t';
    printSymbolRoles(SymRel.Roles, OS);
    OS << " | ";
    printSymbolNameAndUSR(SymRel.RelatedSymbol, Ctx, OS);
    OS << '\n';
  }
}

namespace {

class PrintIndexDataConsumer : public IndexDataConsumer {
  raw_ostream &OS;
  std::unique_ptr<CodegenNameGenerator> CGNameGen;

public:
  PrintIndexDataConsumer(raw_ostream &OS) : OS(OS) {
  }

  void initialize(ASTContext &Ctx) override {
    CGNameGen.reset(new CodegenNameGenerator(Ctx));
  }

  bool handleDeclOccurence(const Decl *D, SymbolRoleSet Roles,
                           ArrayRef<SymbolRelation> Relations,
                           SourceLocation Loc, bool IsInSystemFile,
                           ASTNodeInfo ASTNode) override {
    ASTContext &Ctx = D->getASTContext();
    SourceManager &SM = Ctx.getSourceManager();

    Loc = SM.getFileLoc(Loc);
    FileID FID = SM.getFileID(Loc);
    unsigned Offset = SM.getFileOffset(Loc);
    printDeclOccurrence(D, Roles, Relations, FID, Offset, IsInSystemFile,
                        *CGNameGen, OS);
    return true;
  }

  bool handleModuleOccurence(const ImportDecl *ImportD, SymbolRoleSet Roles,
                             SourceLocation Loc, bool IsInSystemFile) override {
    ASTContext &Ctx = ImportD->getASTContext();
    SourceManager &SM = Ctx.getSourceManager();

    Loc = SM.getFileLoc(Loc);
    FileID FID = SM.getFileID(Loc);
    unsigned Line = SM.getLineNumber(FID, SM.getFileOffset(Loc));
    unsigned Col = SM.getColumnNumber(FID, SM.getFileOffset(Loc));
    OS << Line << ':' << Col << " | ";

    printSymbolInfo(getSymbolInfo(ImportD), OS);
    OS << " | ";

    OS << ImportD->getImportedModule()->getFullModuleName() << " | ";

    printSymbolRoles(Roles, OS);
    OS << " |\n";

    return true;
  }
};

} // anonymous namespace

//===----------------------------------------------------------------------===//
// Print Source Symbols
//===----------------------------------------------------------------------===//

static void dumpModuleFileInputs(serialization::ModuleFile &Mod,
                                 ASTReader &Reader,
                                 raw_ostream &OS) {
  OS << "---- Module Inputs ----\n";
  Reader.visitInputFiles(Mod, /*IncludeSystem=*/true, /*Complain=*/false,
                        [&](const serialization::InputFile &IF, bool isSystem) {
    OS << (isSystem ? "system" : "user") << " | ";
    OS << IF.getFile()->getName() << '\n';
  });
}

static bool printSourceSymbols(ArrayRef<const char *> Args,
                               bool dumpModuleImports,
                               bool indexLocals) {
  SmallVector<const char *, 4> ArgsWithProgName;
  ArgsWithProgName.push_back("clang");
  ArgsWithProgName.append(Args.begin(), Args.end());
  IntrusiveRefCntPtr<DiagnosticsEngine>
    Diags(CompilerInstance::createDiagnostics(new DiagnosticOptions));
  auto CInvok = createInvocationFromCommandLine(ArgsWithProgName, Diags);
  if (!CInvok)
    return true;

  raw_ostream &OS = outs();
  auto DataConsumer = std::make_shared<PrintIndexDataConsumer>(OS);
  IndexingOptions IndexOpts;
  IndexOpts.IndexFunctionLocals = indexLocals;
  std::unique_ptr<FrontendAction> IndexAction;
  IndexAction = createIndexingAction(DataConsumer, IndexOpts,
                                     /*WrappedAction=*/nullptr);

  auto PCHContainerOps = std::make_shared<PCHContainerOperations>();
  std::unique_ptr<ASTUnit> Unit(ASTUnit::LoadFromCompilerInvocationAction(
      std::move(CInvok), PCHContainerOps, Diags, IndexAction.get()));

  if (!Unit)
    return true;

  if (dumpModuleImports) {
    if (auto Reader = Unit->getASTReader()) {
      Reader->getModuleManager().visit([&](serialization::ModuleFile &Mod) -> bool {
        OS << "==== Module " << Mod.ModuleName << " ====\n";
        indexModuleFile(Mod, *Reader, *DataConsumer, IndexOpts);
        dumpModuleFileInputs(Mod, *Reader, OS);
        return true; // skip module dependencies.
      });
    }
  }

  return false;
}

static bool printSourceSymbolsFromModule(StringRef modulePath,
                                         StringRef format) {
  FileSystemOptions FileSystemOpts;
  auto pchContOps = std::make_shared<PCHContainerOperations>();
  // Register the support for object-file-wrapped Clang modules.
  pchContOps->registerReader(llvm::make_unique<ObjectFilePCHContainerReader>());
  auto pchRdr = pchContOps->getReaderOrNull(format);
  if (!pchRdr) {
    errs() << "unknown module format: " << format << '\n';
    return true;
  }

  IntrusiveRefCntPtr<DiagnosticsEngine> Diags =
      CompilerInstance::createDiagnostics(new DiagnosticOptions());
  std::unique_ptr<ASTUnit> AU = ASTUnit::LoadFromASTFile(
      modulePath, *pchRdr, ASTUnit::LoadASTOnly, Diags,
      FileSystemOpts, /*UseDebugInfo=*/false,
      /*OnlyLocalDecls=*/true, None,
      /*CaptureDiagnostics=*/false,
      /*AllowPCHWithCompilerErrors=*/true,
      /*UserFilesAreVolatile=*/false);
  if (!AU) {
    errs() << "failed to create TU for: " << modulePath << '\n';
    return true;
  }

  PrintIndexDataConsumer DataConsumer(outs());
  IndexingOptions IndexOpts;
  indexASTUnit(*AU, DataConsumer, IndexOpts);

  return false;
}

class PrintUnitDataConsumer : public UnitIndexDataConsumer {
  struct Dependency {
    enum Kind { File, Module };

    Kind Kind;
    std::string Name;
    bool IsSystem;

    void print(raw_ostream &OS) const {
      switch (Kind) {
      case File:
        OS << "File";
        break;
      case Module:
        OS << "Module";
        break;
      }
      OS << " | " << (IsSystem ? "system" : "user") << " | " << Name << '\n';
    }
  };

  struct Include {
    std::string Source;
    std::string Target;
    unsigned Line;

    void print(raw_ostream &OS) const {
      OS << Source << ':' << Line << " -> " << Target << '\n';
    }
  };

  struct FileOccurrences {
    SourceManager &SM;
    FileID FID;
    bool IsSystem;
    std::vector<DeclOccurrence> Occurrences;

    void print(raw_ostream &OS, CodegenNameGenerator &CGNameGen) const {
      const FileEntry *FE = SM.getFileEntryForID(FID);
      OS << '\n' << FE->getName() << "\n----------\n";
      for (const DeclOccurrence &Occur : Occurrences) {
        printDeclOccurrence(Occur.Dcl, Occur.Roles, Occur.Relations, FID,
                            Occur.Offset, IsSystem, CGNameGen, OS);
      }
    }
  };

  raw_ostream &OS;
  UnitDetails UnitInfo;
  const CompilerInstance &CI;
  CodegenNameGenerator CGNameGen;
  bool IndexModDependencies = true;
  std::vector<Dependency> Dependencies;
  std::vector<Include> Includes;
  std::vector<FileOccurrences> FileOccurInfos;

public:
  PrintUnitDataConsumer(raw_ostream &OS, UnitDetails UnitInfo,
                        bool IndexModDeps)
      : OS(OS), UnitInfo(UnitInfo), CI(UnitInfo.CI),
        CGNameGen(CI.getASTContext()), IndexModDependencies(IndexModDeps) {}

  void handleFileDependency(const FileEntry *FE, bool IsSystem) override {
    Dependencies.push_back({Dependency::File, FE->getName(), IsSystem});
  }

  void handleModuleImport(const serialization::ModuleFile &Mod,
                          bool IsSystem) override {
    Dependencies.push_back({Dependency::Module, Mod.FileName, IsSystem});
  }

  void handleInclude(const FileEntry *Source, unsigned Line,
                     const FileEntry *Target) override {
    Includes.push_back({Source->getName(), Target->getName(), Line});
  }

  bool
  shouldIndexModuleDependency(const serialization::ModuleFile &Mod) override {
    return IndexModDependencies;
  }

  bool handleFileOccurrences(FileID FID,
                             ArrayRef<DeclOccurrence> OccurrencesSortedByOffset,
                             bool IsSystem) override {
    SourceManager &SM = CI.getASTContext().getSourceManager();
    FileOccurInfos.push_back({SM, FID, IsSystem, OccurrencesSortedByOffset});
    return false;
  }

  void finish() override {
    OS << sys::path::filename(UnitInfo.OutputFile) << '\n'
       << "----------\n"
       << "is-system: " << UnitInfo.IsSystemUnit << '\n'
       << "is-module: " << UnitInfo.IsModuleUnit << '\n'
       << "module-name: " << UnitInfo.ModuleName << '\n'
       << "has-main: " << !!UnitInfo.RootFile << '\n'
       << "main-path: "
       << (UnitInfo.RootFile ? UnitInfo.RootFile->getName() : "") << '\n'
       << "out-file: " << UnitInfo.OutputFile << '\n'
       << "target: " << UnitInfo.CI.getTargetOpts().Triple << '\n'
       << "is-debug: " << UnitInfo.IsDebugCompilation << '\n';

    OS << "\nDEPEND START\n";
    for (const Dependency &Dep : Dependencies) {
      Dep.print(OS);
    }
    OS << "DEPEND END (" << Dependencies.size() << ")\n";
    OS << "\nINCLUDE START\n";
    for (const Include &Inc : Includes) {
      Inc.print(OS);
    }
    OS << "INCLUDE END (" << Includes.size() << ")\n";
    for (const FileOccurrences &FileInfo : FileOccurInfos) {
      FileInfo.print(OS, CGNameGen);
    }
    OS << '\n';
  }
};

static bool printSourceUnit(ArrayRef<const char *> Args, bool IndexLocals,
                            bool IndexModDeps) {
  SmallVector<const char *, 4> ArgsWithProgName;
  ArgsWithProgName.push_back("clang");
  ArgsWithProgName.append(Args.begin(), Args.end());
  IntrusiveRefCntPtr<DiagnosticsEngine> Diags(
      CompilerInstance::createDiagnostics(new DiagnosticOptions));
  auto CInvok = createInvocationFromCommandLine(ArgsWithProgName, Diags);
  if (!CInvok)
    return true;

  raw_ostream &OS = outs();
  UnitIndexingOptions IndexOpts;
  IndexOpts.IndexFunctionLocals = IndexLocals;

  auto ConsumerFactory = [&OS, IndexModDeps](UnitDetails UnitInfo) {
    return llvm::make_unique<PrintUnitDataConsumer>(OS, std::move(UnitInfo),
                                                    IndexModDeps);
  };

  std::unique_ptr<FrontendAction> IndexAction;
  IndexAction = createUnitIndexingAction(ConsumerFactory, IndexOpts,
                                         /*WrappedAction=*/nullptr);

  auto PCHContainerOps = std::make_shared<PCHContainerOperations>();
  std::unique_ptr<ASTUnit> Unit(ASTUnit::LoadFromCompilerInvocationAction(
      std::move(CInvok), PCHContainerOps, Diags, IndexAction.get()));

  return !Unit;
}

//===----------------------------------------------------------------------===//
// Helper Utils
//===----------------------------------------------------------------------===//

static void printSymbolInfo(SymbolInfo SymInfo, raw_ostream &OS) {
  OS << getSymbolKindString(SymInfo.Kind);
  if (SymInfo.SubKind != SymbolSubKind::None)
    OS << '/' << getSymbolSubKindString(SymInfo.SubKind);
  if (SymInfo.Properties) {
    OS << '(';
    printSymbolProperties(SymInfo.Properties, OS);
    OS << ')';
  }
  OS << '/' << getSymbolLanguageString(SymInfo.Lang);
}

static void printSymbolNameAndUSR(const Decl *D, ASTContext &Ctx,
                                  raw_ostream &OS) {
  if (printSymbolName(D, Ctx.getLangOpts(), OS)) {
    OS << "<no-name>";
  }
  OS << " | ";

  SmallString<256> USRBuf;
  if (generateUSRForDecl(D, USRBuf)) {
    OS << "<no-usr>";
  } else {
    OS << USRBuf;
  }
}

//===----------------------------------------------------------------------===//
// Command line processing.
//===----------------------------------------------------------------------===//

int indextest_core_main(int argc, const char **argv) {
  sys::PrintStackTraceOnErrorSignal(argv[0]);
  PrettyStackTraceProgram X(argc, argv);

  assert(argv[1] == StringRef("core"));
  ++argv;
  --argc;

  std::vector<const char *> CompArgs;
  const char **DoubleDash = std::find(argv, argv + argc, StringRef("--"));
  if (DoubleDash != argv + argc) {
    CompArgs = std::vector<const char *>(DoubleDash + 1, argv + argc);
    argc = DoubleDash - argv;
  }

  cl::HideUnrelatedOptions(options::IndexTestCoreCategory);
  cl::ParseCommandLineOptions(argc, argv, "index-test-core");

  if (options::Action == ActionType::None) {
    errs() << "error: action required; pass '-help' for options\n";
    return 1;
  }

  if (options::Action == ActionType::PrintSourceSymbols) {
    if (!options::ModuleFilePath.empty()) {
      return printSourceSymbolsFromModule(options::ModuleFilePath,
                                          options::ModuleFormat);
    }
    if (CompArgs.empty()) {
      errs() << "error: missing compiler args; pass '-- <compiler arguments>'\n";
      return 1;
    }
    return printSourceSymbols(CompArgs, options::DumpModuleImports, options::IncludeLocals);
  }

  if (options::Action == ActionType::PrintSourceUnit) {
    if (CompArgs.empty()) {
      errs()
          << "error: missing compiler args; pass '-- <compiler arguments>'\n";
      return 1;
    }
    return printSourceUnit(CompArgs, options::IncludeLocals,
                           /*IndexModDepedencies=*/true);
  }

  return 0;
}
