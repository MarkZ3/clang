// RUN: c-index-test core -print-source-symbols -- %s -isystem %S/Inputs/sys | FileCheck %S/Inputs/sys/system-head.h
// RUN: c-index-test core -print-source-unit -- %s -isystem %S/Inputs/sys | FileCheck -check-prefixes=UNIT,CHECK %S/Inputs/sys/system-head.h

#include "system-head.h"
