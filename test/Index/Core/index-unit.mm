// RUN: rm -rf %t.mcp
// RUN: c-index-test core -print-source-unit -- -arch x86_64 -mmacosx-version-min=10.7 -c %s -o %t.o -isystem %S/Inputs/sys -fmodules -fmodules-cache-path=%t.mcp -Xclang -fdisable-module-hash -I %S/Inputs/module -I %S/Inputs | FileCheck %s

@import ModDep;
@import ModSystem;



// CHECK: ModTop.pcm

// CHECK: is-system: 0
// CHECK: is-module: 1
// CHECK: module-name: ModTop
// CHECK: has-main: 0
// CHECK: main-path: {{$}}
// CHECK: out-file: {{.*}}/ModTop.pcm

// CHECK: DEPEND START
// CHECK: File | user | {{.*}}/Inputs/module/ModTopSub2.h
// CHECK: File | user | {{.*}}/Inputs/module/ModTopSub1.h
// CHECK: File | user | {{.*}}/Inputs/module/ModTop.h
// CHECK: DEPEND END (3)

// CHECK: INCLUDE START
// CHECK: INCLUDE END (0)

// CHECK: {{.*}}/Inputs/module/ModTop.h
// CHECK: 2:9 | struct/C | <no-name> | c:{{.*}} | <no-cgname> | Def | rel: 0
// CHECK: 2:19 | type-alias/C | ModTopStruct | [[ModTopStruct_USR:.*]] | <no-cgname> | Def | rel: 0
// CHECK: 4:6 | function/C | ModTop_func | {{.*}} | __Z11ModTop_funcv | Decl | rel: 0

// CHECK: {{.*}}/Inputs/module/ModTopSub1.h
// CHECK: 1:6 | function/C | ModTopSub1_func | {{.*}} | __Z15ModTopSub1_funcv | Decl | rel: 0



// CHECK: ModDep.pcm

// CHECK: is-system: 0
// CHECK: is-module: 1
// CHECK: module-name: ModDep
// CHECK: has-main: 0
// CHECK: main-path: {{$}}
// CHECK: out-file: {{.*}}/ModDep.pcm

// CHECK: DEPEND START
// CHECK: File | user | {{.*}}/Inputs/module/ModDep.h
// CHECK: Module | user | {{.*}}/ModTop.pcm
// CHECK: DEPEND END (2)

// CHECK: INCLUDE START
// CHECK: INCLUDE END (0)

// CHECK: {{.*}}/Inputs/module/ModDep.h
// CHECK: 3:6 | function/C | ModDep_func | [[ModDep_func_USR:.*]] | __Z11ModDep_func12ModTopStruct | Decl | rel: 0
// CHECK: 3:18 | type-alias/C | ModTopStruct | [[ModTopStruct_USR]] | <no-cgname> | Ref,RelCont | rel: 1
// CHECK-NEXT: RelCont | ModDep_func | [[ModDep_func_USR]]



// CHECK: ModSystem.pcm

// CHECK: is-system: 1
// CHECK: is-module: 1
// CHECK: module-name: ModSystem
// CHECK: has-main: 0
// CHECK: main-path: {{$}}
// CHECK: out-file: {{.*}}/ModSystem.pcm

// CHECK: DEPEND START
// CHECK: File | system | {{.*}}/Inputs/module/ModSystem.h
// CHECK: DEPEND END (1)

// CHECK: INCLUDE START
// CHECK: INCLUDE END (0)

// CHECK: {{.*}}/Inputs/module/ModSystem.h
// CHECK: 2:9 | struct/C | <no-name> | {{.*}} | <no-cgname> | Def | rel: 0
// CHECK: 2:19 | type-alias/C | ModSystemStruct | {{.*}} | <no-cgname> | Def | rel: 0
// CHECK: 4:6 | function/C | ModSystem_func | {{.*}} | __Z14ModSystem_funcv | Decl | rel: 0



// CHECK: index-unit.mm.o

// CHECK: is-system: 0
// CHECK: is-module: 0
// CHECK: module-name: {{$}}
// CHECK: has-main: 1
// CHECK: main-path: {{.*}}/index-unit.mm
// CHECK: out-file: {{.*}}/index-unit.mm.o

// CHECK: DEPEND START
// CHECK: File | user | {{.*}}/index-unit.mm
// CHECK: File | user | {{.*}}/Inputs/module/module.modulemap
// CHECK: File | user | {{.*}}/Inputs/transitive-include.h
// CHECK: File | system | {{.*}}/Inputs/sys/system-head.h
// CHECK: Module | user | {{.*}}/ModDep.pcm
// CHECK: Module | system | {{.*}}/ModSystem.pcm
// CHECK: DEPEND END (6)

// CHECK: INCLUDE START
// CHECK: {{.*}}index-unit.mm:[[@LINE+1]] -> {{.*}}/Inputs/transitive-include.h
#include "transitive-include.h"
// CHECK: {{.*}}/Inputs/transitive-include.h:1 -> {{.*}}/Inputs/sys/system-head.h
// CHECK: INCLUDE END (2)

// CHECK: {{.*}}/Inputs/transitive-include.h
// CHECK: 3:8 | struct/C | Point | [[Point_USR:.*]] | <no-cgname> | Def | rel: 0
// CHECK: 4:7 | field/C | x | {{.*}} | <no-cgname> | Def,RelChild | rel: 1
// CHECK-NEXT: RelChild | Point | [[Point_USR]]
// CHECK: 5:7 | field/C | y | {{.*}} | <no-cgname> | Def,RelChild | rel: 1
// CHECK-NEXT: RelChild | Point | [[Point_USR]]

// CHECK: {{.*}}/Inputs/sys/system-head.h
// CHECK: 19:12 | class/ObjC | Base | [[Base_USR:.*]] | _OBJC_CLASS_$_Base | Decl | rel: 0
// CHECK: 23:11 | protocol/ObjC | Prot1 | [[Prot1_USR:.*]] | <no-cgname> | Decl | rel: 0
// CHECK: 29:11 | protocol/ObjC | Prot2 | [[Prot2_USR:.*]] | <no-cgname> | Decl | rel: 0
// CHECK: 29:17 | protocol/ObjC | Prot1 | [[Prot1_USR]] | <no-cgname> | Ref,RelBase,RelCont | rel: 1
// CHECK-NEXT: RelBase,RelCont | Prot2 | [[Prot2_USR]]
// CHECK: 39:12 | class/ObjC | Sub | [[Sub_USR:.*]] | _OBJC_CLASS_$_Sub | Decl | rel: 0
// CHECK: 39:18 | class/ObjC | Base | [[Base_USR]] | _OBJC_CLASS_$_Base | Ref,RelBase,RelCont | rel: 1
// CHECK-NEXT: RelBase,RelCont | Sub | [[Sub_USR]]
// CHECK: 39:23 | protocol/ObjC | Prot2 | [[Prot2_USR]] | <no-cgname> | Ref,RelBase,RelCont | rel: 1
// CHECK-NEXT: RelBase,RelCont | Sub | [[Sub_USR]]
// CHECK: 39:30 | protocol/ObjC | Prot1 | [[Prot1_USR]] | <no-cgname> | Ref,RelBase,RelCont | rel: 1
// CHECK-NEXT: RelBase,RelCont | Sub | [[Sub_USR]]
// CHECK: 41:8 | instance-method/ObjC | getit | {{.*}} | -[Sub getit] | Decl,Dyn,RelChild | rel: 1
// CHECK-NEXT: RelChild | Sub | [[Sub_USR]]
// CHECK: 45:7 | class/C++ | Cls | [[Cls_USR:.*]] | <no-cgname> | Def | rel: 0
// CHECK: 50:7 | class/C++ | SubCls1 | [[SubCls1_USR:.*]] | <no-cgname> | Def | rel: 0
// CHECK: 50:24 | class/C++ | Cls | [[Cls_USR]] | <no-cgname> | Ref,RelBase,RelCont | rel: 1
// CHECK-NEXT: RelBase,RelCont | SubCls1 | [[SubCls1_USR]]
// CHECK: 52:12 | field/C++ | f | {{.*}} | <no-cgname> | Def,RelChild | rel: 1
// CHECK-NEXT: RelChild | SubCls1 | [[SubCls1_USR]]
