#ifndef LLVM_LIB_TARGET_USim_USim_H
#define LLVM_LIB_TARGET_USim_USim_H

#include "MCTargetDesc/USimMCTargetDesc.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm {
class USimTargetMachine;
class FunctionPass;

FunctionPass *createUSimISelDag(USimTargetMachine &TM,
                                CodeGenOpt::Level OptLevel);

namespace USim {
enum {
  GP = USim::R0,
  RA = USim::R1,
  SP = USim::R2,
  FP = USim::R3,
  BP = USim::R4,
};
} // namespace USim

} // namespace llvm

#endif
