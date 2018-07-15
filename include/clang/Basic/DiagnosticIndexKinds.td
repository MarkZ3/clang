//==--- DiagnosticIndexKinds.td - indexing diagnostics --------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Indexing Diagnostics
//===----------------------------------------------------------------------===//

let Component = "Index" in {

let CategoryName = "Index Store Issue" in {

def err_index_store_dir_create_failed : Error<"failed creating the index store "
  "directory: %0">;
def err_index_store_file_status_failed : Error<"failed file status check: %0">;
def err_index_store_record_write_failed : Error<"failed writing record '%0': "
  "%1">;
def err_index_store_unit_write_failed : Error<"failed writing unit data: %0">;

def remark_index_producing_module_file_data : Remark<"producing index data for "
  "module file '%0'">,
  InGroup<IndexStore>;

}

} // end of Indexing diagnostics
