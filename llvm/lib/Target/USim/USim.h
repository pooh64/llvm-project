#ifndef LLVM_LIB_TARGET_USim_USim_H
#define LLVM_LIB_TARGET_USim_USim_H

#include "MCTargetDesc/USimMCTargetDesc.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm {
class USimTargetMachine;
class FunctionPass;
class USimSubtarget;
class AsmPrinter;
class InstructionSelector;
class MCInst;
class MCOperand;
class MachineInstr;
class MachineOperand;
class PassRegistry;

bool lowerUSimMachineInstrToMCInst(const MachineInstr *MI, MCInst &OutMI,
                                    AsmPrinter &AP);
bool LowerUSimMachineOperandToMCOperand(const MachineOperand &MO,
                                         MCOperand &MCOp, const AsmPrinter &AP);

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
