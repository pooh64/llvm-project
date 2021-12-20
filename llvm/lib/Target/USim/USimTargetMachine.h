#ifndef LLVM_LIB_TARGET_USIM_USIMTARGETMACHINE_H
#define LLVM_LIB_TARGET_USIM_USIMTARGETMACHINE_H

#include "USimInstrInfo.h"
#include "USimSubtarget.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm {

class USimTargetMachine : public LLVMTargetMachine {
  std::unique_ptr<TargetLoweringObjectFile> TLOF;
  USimSubtarget Subtarget;
  // mutable StringMap<std::unique_ptr<USimSubtarget>> SubtargetMap;

public:
  USimTargetMachine(const Target &T, const Triple &TT, StringRef CPU,
                    StringRef FS, const TargetOptions &Options,
                    Optional<Reloc::Model> RM, Optional<CodeModel::Model> CM,
                    CodeGenOpt::Level OL, bool JIT);
  ~USimTargetMachine() override;

  const USimSubtarget *getSubtargetImpl() const { return &Subtarget; }
  const USimSubtarget *getSubtargetImpl(const Function &) const override {
    return &Subtarget;
  }

  // Pass Pipeline Configuration
  TargetPassConfig *createPassConfig(PassManagerBase &PM) override;
  TargetLoweringObjectFile *getObjFileLowering() const override {
    return TLOF.get();
  }

  // TargetTransformInfo getTargetTransformInfo(const Function &F) override;
};
} // end namespace llvm

#endif
