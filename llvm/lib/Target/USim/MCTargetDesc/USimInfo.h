#ifndef LLVM_LIB_TARGET_USIM_MCTARGETDESC_USIMINFO_H
#define LLVM_LIB_TARGET_USIM_MCTARGETDESC_USIMINFO_H

#include "llvm/MC/MCInstrDesc.h"

namespace llvm {

namespace USimCC {
enum CondCode {
  EQ,
  NE,
  LE,
  GT,
  LEU,
  GTU,
  INVALID,
};

CondCode getOppositeBranchCondition(CondCode);

enum BRCondCode {
  BREQ = 0x0,
};
} // end namespace USimCC

namespace USimOp {
enum OperandType : unsigned {
  OPERAND_SIMM16 = MCOI::OPERAND_FIRST_TARGET,
  OPERAND_UIMM16,
};
} // namespace USimOp

} // end namespace llvm

#endif
