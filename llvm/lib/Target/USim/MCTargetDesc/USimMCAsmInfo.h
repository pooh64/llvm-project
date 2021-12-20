#ifndef LLVM_LIB_TARGET_USIM_MCTARGETDESC_USIMMCASMINFO_H
#define LLVM_LIB_TARGET_USIM_MCTARGETDESC_USIMMCASMINFO_H

#include "llvm/MC/MCAsmInfoELF.h"

namespace llvm {

class Triple;

class USimMCAsmInfo : public MCAsmInfoELF {
  void anchor() override;

public:
  explicit USimMCAsmInfo(const Triple &TT);
};

} // end namespace llvm

#endif // LLVM_LIB_TARGET_USIM_MCTARGETDESC_USIMMCASMINFO_H
