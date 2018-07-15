//===--- IndexDataStore.cpp - Index data store info -----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "IndexDataStoreUtils.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Mutex.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"

using namespace clang;
using namespace clang::index;
using namespace clang::index::store;
using namespace llvm;

static void appendSubDir(StringRef subdir,
                         SmallVectorImpl<char> &StorePathBuf) {
  SmallString<10> VersionPath;
  raw_svector_ostream(VersionPath) << 'v' << STORE_FORMAT_VERSION;

  sys::path::append(StorePathBuf, VersionPath);
  sys::path::append(StorePathBuf, subdir);
}

void store::appendInteriorUnitPath(StringRef UnitName,
                                   SmallVectorImpl<char> &PathBuf) {
  sys::path::append(PathBuf, UnitName);
}

void store::appendUnitSubDir(SmallVectorImpl<char> &StorePathBuf) {
  return appendSubDir("units", StorePathBuf);
}

void store::appendRecordSubDir(SmallVectorImpl<char> &StorePathBuf) {
  return appendSubDir("records", StorePathBuf);
}

void store::appendInteriorRecordPath(StringRef RecordName,
                                     SmallVectorImpl<char> &PathBuf) {
  // To avoid putting a huge number of files into the records directory, create
  // subdirectories based on the last 2 characters from the hash.
  StringRef hash2chars = RecordName.substr(RecordName.size() - 2);
  sys::path::append(PathBuf, hash2chars);
  sys::path::append(PathBuf, RecordName);
}
