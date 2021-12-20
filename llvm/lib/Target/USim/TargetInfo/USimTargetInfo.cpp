#include "USim.h"
#include "llvm/IR/Module.h"
#include "TargetInfo/USimTargetInfo.h"
#include "llvm/MC/TargetRegistry.h"

using namespace llvm;

Target &llvm::getTheUSimTarget() {
  static Target TheUSimTarget;
  return TheUSimTarget;
}

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeUSimTargetInfo() {
  RegisterTarget<Triple::usim, false> X(getTheUSimTarget(), "USim", "USim (32-bit simulator arch)", "USim");
}
