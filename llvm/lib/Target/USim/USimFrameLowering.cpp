#include "USimFrameLowering.h"
//#include "USimMachineFunctionInfo.h"
#include "USimSubtarget.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/RegisterScavenging.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"

#define DEBUG_TYPE "USim-frame-lowering"

using namespace llvm;

void USimFrameLowering::determineCalleeSaves(MachineFunction &MF,
                                             BitVector &SavedRegs,
                                             RegScavenger *RS) const {
  llvm_unreachable("");
}

void USimFrameLowering::adjustStackToMatchRecords(
    MachineBasicBlock &MBB, MachineBasicBlock::iterator MBBI,
    bool Allocate) const {
  llvm_unreachable("");
}

void USimFrameLowering::emitPrologue(MachineFunction &MF,
                                     MachineBasicBlock &MBB) const {
  llvm_unreachable("");
}

void USimFrameLowering::emitEpilogue(MachineFunction &MF,
                                     MachineBasicBlock &MBB) const {
  llvm_unreachable("");
}

bool USimFrameLowering::assignCalleeSavedSpillSlots(
    MachineFunction &MF, const TargetRegisterInfo *TRI,
    std::vector<CalleeSavedInfo> &CSI) const {
  llvm_unreachable("");
}

bool USimFrameLowering::spillCalleeSavedRegisters(
    MachineBasicBlock &MBB, MachineBasicBlock::iterator MI,
    ArrayRef<CalleeSavedInfo> CSI, const TargetRegisterInfo *TRI) const {
  llvm_unreachable("");
}

bool USimFrameLowering::restoreCalleeSavedRegisters(
    MachineBasicBlock &MBB, MachineBasicBlock::iterator MI,
    MutableArrayRef<CalleeSavedInfo> CSI, const TargetRegisterInfo *TRI) const {
  llvm_unreachable("");
}

void USimFrameLowering::processFunctionBeforeFrameFinalized(
    MachineFunction &MF, RegScavenger *RS) const {
  llvm_unreachable("");
}

MachineBasicBlock::iterator USimFrameLowering::eliminateCallFramePseudoInstr(
    MachineFunction &MF, MachineBasicBlock &MBB,
    MachineBasicBlock::iterator I) const {
  llvm_unreachable("");
}

bool USimFrameLowering::hasFP(const MachineFunction &MF) const {
  llvm_unreachable("");
}
