//===--- IndexDataStoreSymbolUtils.h - Utilities for indexstore symbols ---===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_INDEX_INDEXDATASTORESYMBOLUTILS_H
#define LLVM_CLANG_INDEX_INDEXDATASTORESYMBOLUTILS_H

#include "indexstore/indexstore.h"
#include "clang/Index/IndexSymbol.h"

namespace clang {
namespace index {

/// Map a SymbolKind to an indexstore_symbol_kind_t.
indexstore_symbol_kind_t getIndexStoreKind(SymbolKind K);

/// Map a SymbolSubKind to an indexstore_symbol_subkind_t.
indexstore_symbol_subkind_t getIndexStoreSubKind(SymbolSubKind K);

/// Map a SymbolLanguage to an indexstore_symbol_language_t.
indexstore_symbol_language_t getIndexStoreLang(SymbolLanguage L);

/// Map a SymbolPropertySet to its indexstore representation.
uint64_t getIndexStoreProperties(SymbolPropertySet Props);

/// Map a SymbolRoleSet to its indexstore representation.
uint64_t getIndexStoreRoles(SymbolRoleSet Roles);

} // end namespace index
} // end namespace clang

#endif // LLVM_CLANG_INDEX_INDEXDATASTORESYMBOLUTILS_H
