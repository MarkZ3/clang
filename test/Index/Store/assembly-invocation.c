// Make sure it doesn't crash.
// RUN: %clang -target x86_64-apple-macosx10.7 -S %s -o %t.s
// RUN: %clang -target x86_64-apple-macosx10.7 -c %t.s -o %t.o -index-store-path %t.idx
