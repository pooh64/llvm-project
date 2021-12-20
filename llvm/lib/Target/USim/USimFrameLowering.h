#ifndef LLVM_LIB_TARGET_USIM_USIMFRAMELOWERING_H
#define LLVM_LIB_TARGET_USIM_USIMFRAMELOWERING_H

#include "USim.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/TargetFrameLowering.h"

namespace llvm {

class MachineFunction;
class USimSubtarget;
class USimInstrInfo;

class USimFrameLowering : public TargetFrameLowering {
public:
  USimFrameLowering(const USimSubtarget &STI)
      : TargetFrameLowering(TargetFrameLowering::StackGrowsDown, Align(4), 0),
        STI(STI) {}

  void emitPrologue(MachineFunction &MF, MachineBasicBlock &MBB) const override;
  void emitEpilogue(MachineFunction &MF, MachineBasicBlock &MBB) const override;

  void determineCalleeSaves(MachineFunction &MF, BitVector &SavedRegs,
                            RegScavenger *RS) const override;

  bool spillCalleeSavedRegisters(MachineBasicBlock &MBB,
                                 MachineBasicBlock::iterator MI,
                                 ArrayRef<CalleeSavedInfo> CSI,
                                 const TargetRegisterInfo *TRI) const override;

  bool
  restoreCalleeSavedRegisters(MachineBasicBlock &MBB,
                              MachineBasicBlock::iterator MI,
                              MutableArrayRef<CalleeSavedInfo> CSI,
                              const TargetRegisterInfo *TRI) const override;

  void processFunctionBeforeFrameFinalized(MachineFunction &MF,
                                           RegScavenger *RS) const override;

  bool hasFP(const MachineFunction &MF) const override;

  MachineBasicBlock::iterator
  eliminateCallFramePseudoInstr(MachineFunction &MF, MachineBasicBlock &MBB,
                                MachineBasicBlock::iterator I) const override;

  bool assignCalleeSavedSpillSlots(
      llvm::MachineFunction &, const llvm::TargetRegisterInfo *,
      std::vector<llvm::CalleeSavedInfo> &) const override;

private:
  void adjustStackToMatchRecords(MachineBasicBlock &MBB,
                                 MachineBasicBlock::iterator MI,
                                 bool Allocate) const;

  const USimSubtarget &STI;
};

} // end namespace llvm

#endif // LLVM_LIB_TARGET_USIM_USIMFRAMELOWERING_H
