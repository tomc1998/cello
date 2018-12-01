#pragma once

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

namespace cello {
  void write_to_obj(const char* filename, llvm::Module* module) {
    auto target_triple = llvm::sys::getDefaultTargetTriple();
    module->setTargetTriple(target_triple);

    std::string error;
    auto target = llvm::TargetRegistry::lookupTarget(target_triple, error);

    // Print an error and exit if we couldn't find the requested target.
    // This generally occurs if we've forgotten to initialise the
    // TargetRegistry or we have a bogus target triple.
    if (!target) {
      llvm::errs() << error;
      return;
    }

    auto cpu = "generic";
    auto features = "";

    llvm::TargetOptions opt;

    auto rm = llvm::Optional<llvm::Reloc::Model>();
    auto target_machine =
      target->createTargetMachine(target_triple, cpu, features, opt, rm);

    module->setDataLayout(target_machine->createDataLayout());

    std::error_code ec;
    llvm::raw_fd_ostream dest(filename, ec, llvm::sys::fs::F_None);

    if (ec) {
      llvm::errs() << "Could not open file: " << ec.message();
      return;
    }

    llvm::legacy::PassManager pass;
    auto FileType = llvm::TargetMachine::CGFT_ObjectFile;

    if (target_machine->addPassesToEmitFile(pass, dest, nullptr, FileType)) {
      llvm::errs() << "target_machine can't emit a file of this type";
      return;
    }

    pass.run(*module);
    dest.flush();

    llvm::outs() << "Wrote " << filename << "\n";
  }
}
