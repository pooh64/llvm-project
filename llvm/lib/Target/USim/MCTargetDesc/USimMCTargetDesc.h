#ifndef LLVM_LIB_TARGET_USim_MCTARGETDESC_USimMCTARGETDESC_H
#define LLVM_LIB_TARGET_USim_MCTARGETDESC_USimMCTARGETDESC_H

#include "llvm/Support/DataTypes.h"

#include <memory>

namespace llvm {
class Target;
class Triple;

extern Target TheUSimTarget;

} // End llvm namespace

// Defines symbolic names for USim registers.  This defines a mapping from
// register name to register number.
#define GET_REGINFO_ENUM
#include "USimGenRegisterInfo.inc"

// Defines symbolic names for the USim instructions.
#define GET_INSTRINFO_ENUM
#include "USimGenInstrInfo.inc"

#endif
