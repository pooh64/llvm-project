//===----------------------------------------------------------------------===//
//
// Implements the info about USim target spec.
//
//===----------------------------------------------------------------------===//

#include "USimTargetMachine.h"
#include "USim.h"
//#include "USimTargetTransformInfo.h"
#include "TargetInfo/USimTargetInfo.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/CodeGen/TargetPassConfig.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/CodeGen.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Target/TargetOptions.h"

#define DEBUG_TYPE "USim"

using namespace llvm;

static Reloc::Model getRelocModel(Optional<Reloc::Model> RM) {
  return RM.getValueOr(Reloc::Static);
}

/// USimTargetMachine ctor - Create an ILP32 Architecture model
USimTargetMachine::USimTargetMachine(const Target &T, const Triple &TT,
                                     StringRef CPU, StringRef FS,
                                     const TargetOptions &Options,
                                     Optional<Reloc::Model> RM,
                                     Optional<CodeModel::Model> CM,
                                     CodeGenOpt::Level OL, bool JIT)
    : LLVMTargetMachine(T,
                        "e-m:e-p:32:32-i1:8:32-i8:8:32-i16:16:32-i32:32:32-"
                        "f32:32:32-i64:32-f64:32-a:0:32-n32",
                        TT, CPU, FS, Options, getRelocModel(RM),
                        getEffectiveCodeModel(CM, CodeModel::Small), OL),
      TLOF(std::make_unique<TargetLoweringObjectFileELF>()),
      Subtarget(TT, std::string(CPU), std::string(FS), *this) {
  initAsmInfo();
}

USimTargetMachine::~USimTargetMachine() = default;

namespace {

/// USim Code Generator Pass Configuration Options.
class USimPassConfig : public TargetPassConfig {
public:
  USimPassConfig(USimTargetMachine &TM, PassManagerBase &PM)
      : TargetPassConfig(TM, PM) {}

  USimTargetMachine &getUSimTargetMachine() const {
    llvm_unreachable("");
    return getTM<USimTargetMachine>();
  }

  bool addInstSelector() override;
  void addPreEmitPass() override;
  void addPreRegAlloc() override;
};

} // end anonymous namespace

TargetPassConfig *USimTargetMachine::createPassConfig(PassManagerBase &PM) {
  return new USimPassConfig(*this, PM);
}

bool USimPassConfig::addInstSelector() { llvm_unreachable(""); }

void USimPassConfig::addPreEmitPass() { llvm_unreachable(""); }

void USimPassConfig::addPreRegAlloc() { llvm_unreachable(""); }

// Force static initialization.
extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeUSimTarget() {
  RegisterTargetMachine<USimTargetMachine> X(getTheUSimTarget());
}

#if 0
TargetTransformInfo
USimTargetMachine::getTargetTransformInfo(const Function &F) {
  return TargetTransformInfo(USimTTIImpl(this, F));
}
#endif