#include "USimRegisterInfo.h"
#include "USim.h"
#include "USimInstrInfo.h"
//#include "USimMachineFunctionInfo.h"
#include "USimSubtarget.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/RegisterScavenging.h"
#include "llvm/CodeGen/TargetFrameLowering.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

using namespace llvm;

#define DEBUG_TYPE "USim-reg-info"

#define GET_REGINFO_TARGET_DESC
#include "USimGenRegisterInfo.inc"

USimRegisterInfo::USimRegisterInfo() : USimGenRegisterInfo(USim::R1) {}

#if 0
bool USimRegisterInfo::needsFrameMoves(const MachineFunction &MF) {
  return MF.needsFrameMoves();
}
#endif

const MCPhysReg *
USimRegisterInfo::getCalleeSavedRegs(const MachineFunction *MF) const {
  llvm_unreachable("");
  // const USimSubtarget &Subtarget = MF.getSubtarget<USimSubtarget>();
}

BitVector USimRegisterInfo::getReservedRegs(const MachineFunction &MF) const {
  llvm_unreachable("");
}

bool USimRegisterInfo::requiresRegisterScavenging(
    const MachineFunction &MF) const {
  llvm_unreachable("");
}

#if 0
bool USimRegisterInfo::useFPForScavengingIndex(
    const MachineFunction &MF) const {
  llvm_unreachable("");
}
#endif

void USimRegisterInfo::eliminateFrameIndex(MachineBasicBlock::iterator II,
                                           int SPAdj, unsigned FIOperandNum,
                                           RegScavenger *RS) const {
  llvm_unreachable("");
}

Register USimRegisterInfo::getFrameRegister(const MachineFunction &MF) const {
  llvm_unreachable("");
}

const uint32_t *
USimRegisterInfo::getCallPreservedMask(const MachineFunction &MF,
                                       CallingConv::ID CC) const {
  llvm_unreachable("");
}
