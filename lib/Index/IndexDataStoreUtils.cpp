//===--- IndexDataStoreUtils.cpp - Functions/constants for the data store -===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "IndexDataStoreUtils.h"
#include "clang/Index/IndexDataStoreSymbolUtils.h"
#include "llvm/Bitcode/BitstreamWriter.h"

using namespace clang;
using namespace clang::index;
using namespace clang::index::store;
using namespace llvm;

void store::emitBlockID(unsigned ID, const char *Name, BitstreamWriter &Stream,
                        RecordDataImpl &Record) {
  Record.clear();
  Record.push_back(ID);
  Stream.EmitRecord(bitc::BLOCKINFO_CODE_SETBID, Record);

  // Emit the block name if present.
  if (!Name || Name[0] == 0)
    return;
  Record.clear();
  while (*Name)
    Record.push_back(*Name++);
  Stream.EmitRecord(bitc::BLOCKINFO_CODE_BLOCKNAME, Record);
}

void store::emitRecordID(unsigned ID, const char *Name, BitstreamWriter &Stream,
                         RecordDataImpl &Record) {
  Record.clear();
  Record.push_back(ID);
  while (*Name)
    Record.push_back(*Name++);
  Stream.EmitRecord(bitc::BLOCKINFO_CODE_SETRECORDNAME, Record);
}

indexstore_symbol_kind_t index::getIndexStoreKind(SymbolKind K) {
  switch (K) {
  case SymbolKind::Unknown:
    return INDEXSTORE_SYMBOL_KIND_UNKNOWN;
  case SymbolKind::Module:
    return INDEXSTORE_SYMBOL_KIND_MODULE;
  case SymbolKind::Namespace:
    return INDEXSTORE_SYMBOL_KIND_NAMESPACE;
  case SymbolKind::NamespaceAlias:
    return INDEXSTORE_SYMBOL_KIND_NAMESPACEALIAS;
  case SymbolKind::Macro:
    return INDEXSTORE_SYMBOL_KIND_MACRO;
  case SymbolKind::Enum:
    return INDEXSTORE_SYMBOL_KIND_ENUM;
  case SymbolKind::Struct:
    return INDEXSTORE_SYMBOL_KIND_STRUCT;
  case SymbolKind::Class:
    return INDEXSTORE_SYMBOL_KIND_CLASS;
  case SymbolKind::Protocol:
    return INDEXSTORE_SYMBOL_KIND_PROTOCOL;
  case SymbolKind::Extension:
    return INDEXSTORE_SYMBOL_KIND_EXTENSION;
  case SymbolKind::Union:
    return INDEXSTORE_SYMBOL_KIND_UNION;
  case SymbolKind::TypeAlias:
    return INDEXSTORE_SYMBOL_KIND_TYPEALIAS;
  case SymbolKind::Function:
    return INDEXSTORE_SYMBOL_KIND_FUNCTION;
  case SymbolKind::Variable:
    return INDEXSTORE_SYMBOL_KIND_VARIABLE;
  case SymbolKind::Field:
    return INDEXSTORE_SYMBOL_KIND_FIELD;
  case SymbolKind::EnumConstant:
    return INDEXSTORE_SYMBOL_KIND_ENUMCONSTANT;
  case SymbolKind::InstanceMethod:
    return INDEXSTORE_SYMBOL_KIND_INSTANCEMETHOD;
  case SymbolKind::ClassMethod:
    return INDEXSTORE_SYMBOL_KIND_CLASSMETHOD;
  case SymbolKind::StaticMethod:
    return INDEXSTORE_SYMBOL_KIND_STATICMETHOD;
  case SymbolKind::InstanceProperty:
    return INDEXSTORE_SYMBOL_KIND_INSTANCEPROPERTY;
  case SymbolKind::ClassProperty:
    return INDEXSTORE_SYMBOL_KIND_CLASSPROPERTY;
  case SymbolKind::StaticProperty:
    return INDEXSTORE_SYMBOL_KIND_STATICPROPERTY;
  case SymbolKind::Constructor:
    return INDEXSTORE_SYMBOL_KIND_CONSTRUCTOR;
  case SymbolKind::Destructor:
    return INDEXSTORE_SYMBOL_KIND_DESTRUCTOR;
  case SymbolKind::ConversionFunction:
    return INDEXSTORE_SYMBOL_KIND_CONVERSIONFUNCTION;
  case SymbolKind::Parameter:
    return INDEXSTORE_SYMBOL_KIND_PARAMETER;
  case SymbolKind::Using:
    return INDEXSTORE_SYMBOL_KIND_USING;
  }
  llvm_unreachable("unexpected symbol kind");
}

indexstore_symbol_subkind_t index::getIndexStoreSubKind(SymbolSubKind K) {
  switch (K) {
  case SymbolSubKind::None:
    return INDEXSTORE_SYMBOL_SUBKIND_NONE;
  case SymbolSubKind::CXXCopyConstructor:
    return INDEXSTORE_SYMBOL_SUBKIND_CXXCOPYCONSTRUCTOR;
  case SymbolSubKind::CXXMoveConstructor:
    return INDEXSTORE_SYMBOL_SUBKIND_CXXMOVECONSTRUCTOR;
  case SymbolSubKind::AccessorGetter:
    return INDEXSTORE_SYMBOL_SUBKIND_ACCESSORGETTER;
  case SymbolSubKind::AccessorSetter:
    return INDEXSTORE_SYMBOL_SUBKIND_ACCESSORSETTER;
  case SymbolSubKind::UsingTypename:
    return INDEXSTORE_SYMBOL_SUBKIND_USINGTYPENAME;
  case SymbolSubKind::UsingValue:
    return INDEXSTORE_SYMBOL_SUBKIND_USINGVALUE;
  }
  llvm_unreachable("unexpected symbol subkind");
}

indexstore_symbol_language_t index::getIndexStoreLang(SymbolLanguage L) {
  switch (L) {
  case SymbolLanguage::C:
    return INDEXSTORE_SYMBOL_LANG_C;
  case SymbolLanguage::ObjC:
    return INDEXSTORE_SYMBOL_LANG_OBJC;
  case SymbolLanguage::CXX:
    return INDEXSTORE_SYMBOL_LANG_CXX;
  case SymbolLanguage::Swift:
    return INDEXSTORE_SYMBOL_LANG_SWIFT;
  }
  llvm_unreachable("unexpected symbol language");
}

uint64_t index::getIndexStoreProperties(SymbolPropertySet Props) {
  uint64_t storeProp = 0;
  applyForEachSymbolProperty(Props, [&](SymbolProperty prop) {
    switch (prop) {
    case SymbolProperty::Generic:
      storeProp |= INDEXSTORE_SYMBOL_PROPERTY_GENERIC;
      break;
    case SymbolProperty::TemplatePartialSpecialization:
      storeProp |= INDEXSTORE_SYMBOL_PROPERTY_TEMPLATE_PARTIAL_SPECIALIZATION;
      break;
    case SymbolProperty::TemplateSpecialization:
      storeProp |= INDEXSTORE_SYMBOL_PROPERTY_TEMPLATE_SPECIALIZATION;
      break;
    case SymbolProperty::UnitTest:
      storeProp |= INDEXSTORE_SYMBOL_PROPERTY_UNITTEST;
      break;
    case SymbolProperty::IBAnnotated:
      storeProp |= INDEXSTORE_SYMBOL_PROPERTY_IBANNOTATED;
      break;
    case SymbolProperty::IBOutletCollection:
      storeProp |= INDEXSTORE_SYMBOL_PROPERTY_IBOUTLETCOLLECTION;
      break;
    case SymbolProperty::GKInspectable:
      storeProp |= INDEXSTORE_SYMBOL_PROPERTY_GKINSPECTABLE;
      break;
    case SymbolProperty::Local:
      storeProp |= INDEXSTORE_SYMBOL_PROPERTY_LOCAL;
      break;
    }
  });
  return storeProp;
}

uint64_t index::getIndexStoreRoles(SymbolRoleSet Roles) {
  uint64_t storeRoles = 0;
  applyForEachSymbolRole(Roles, [&](SymbolRole role) {
    switch (role) {
    case SymbolRole::Declaration:
      storeRoles |= INDEXSTORE_SYMBOL_ROLE_DECLARATION;
      break;
    case SymbolRole::Definition:
      storeRoles |= INDEXSTORE_SYMBOL_ROLE_DEFINITION;
      break;
    case SymbolRole::Reference:
      storeRoles |= INDEXSTORE_SYMBOL_ROLE_REFERENCE;
      break;
    case SymbolRole::Read:
      storeRoles |= INDEXSTORE_SYMBOL_ROLE_READ;
      break;
    case SymbolRole::Write:
      storeRoles |= INDEXSTORE_SYMBOL_ROLE_WRITE;
      break;
    case SymbolRole::Call:
      storeRoles |= INDEXSTORE_SYMBOL_ROLE_CALL;
      break;
    case SymbolRole::Dynamic:
      storeRoles |= INDEXSTORE_SYMBOL_ROLE_DYNAMIC;
      break;
    case SymbolRole::AddressOf:
      storeRoles |= INDEXSTORE_SYMBOL_ROLE_ADDRESSOF;
      break;
    case SymbolRole::Implicit:
      storeRoles |= INDEXSTORE_SYMBOL_ROLE_IMPLICIT;
      break;
    case SymbolRole::RelationChildOf:
      storeRoles |= INDEXSTORE_SYMBOL_ROLE_REL_CHILDOF;
      break;
    case SymbolRole::RelationBaseOf:
      storeRoles |= INDEXSTORE_SYMBOL_ROLE_REL_BASEOF;
      break;
    case SymbolRole::RelationOverrideOf:
      storeRoles |= INDEXSTORE_SYMBOL_ROLE_REL_OVERRIDEOF;
      break;
    case SymbolRole::RelationReceivedBy:
      storeRoles |= INDEXSTORE_SYMBOL_ROLE_REL_RECEIVEDBY;
      break;
    case SymbolRole::RelationCalledBy:
      storeRoles |= INDEXSTORE_SYMBOL_ROLE_REL_CALLEDBY;
      break;
    case SymbolRole::RelationExtendedBy:
      storeRoles |= INDEXSTORE_SYMBOL_ROLE_REL_EXTENDEDBY;
      break;
    case SymbolRole::RelationAccessorOf:
      storeRoles |= INDEXSTORE_SYMBOL_ROLE_REL_ACCESSOROF;
      break;
    case SymbolRole::RelationContainedBy:
      storeRoles |= INDEXSTORE_SYMBOL_ROLE_REL_CONTAINEDBY;
      break;
    case SymbolRole::RelationIBTypeOf:
      storeRoles |= INDEXSTORE_SYMBOL_ROLE_REL_IBTYPEOF;
      break;
    case SymbolRole::RelationSpecializationOf:
      storeRoles |= INDEXSTORE_SYMBOL_ROLE_REL_SPECIALIZATIONOF;
      break;
    }
  });
  return storeRoles;
}
