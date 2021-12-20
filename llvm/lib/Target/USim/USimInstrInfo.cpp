#include "USimInstrInfo.h"
#include "USim.h"
//#include "USimMachineFunctionInfo.h"
#include "MCTargetDesc/USimInfo.h"
#include "USimSubtarget.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineMemOperand.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

#define GET_INSTRINFO_CTOR_DTOR
#include "USimGenInstrInfo.inc"

#define DEBUG_TYPE "USim-inst-info"

void USimInstrInfo::anchor() {}

USimInstrInfo::USimInstrInfo(const USimSubtarget &STI)
    : USimGenInstrInfo(USim::ADJCALLSTACKDOWN, USim::ADJCALLSTACKUP), STI(STI) {
}

unsigned USimInstrInfo::isLoadFromStackSlot(const MachineInstr &MI,
                                            int &FrameIndex) const {
  llvm_unreachable("");
  return 0;
}

unsigned USimInstrInfo::isStoreToStackSlot(const MachineInstr &MI,
                                           int &FrameIndex) const {
  llvm_unreachable("");
  return 0;
}

bool USimInstrInfo::analyzeBranch(MachineBasicBlock &MBB,
                                  MachineBasicBlock *&TBB,
                                  MachineBasicBlock *&FBB,
                                  SmallVectorImpl<MachineOperand> &Cond,
                                  bool AllowModify) const {
  llvm_unreachable("");
}

unsigned USimInstrInfo::removeBranch(MachineBasicBlock &MBB,
                                     int *BytesRemoved) const {
  llvm_unreachable("");
}

void USimInstrInfo::copyPhysReg(MachineBasicBlock &MBB,
                                MachineBasicBlock::iterator I,
                                const DebugLoc &DL, MCRegister DestReg,
                                MCRegister SrcReg, bool KillSrc) const {
  llvm_unreachable("");
}

void USimInstrInfo::storeRegToStackSlot(MachineBasicBlock &MBB,
                                        MachineBasicBlock::iterator I,
                                        Register SrcReg, bool IsKill,
                                        int FrameIndex,
                                        const TargetRegisterClass *RC,
                                        const TargetRegisterInfo *TRI) const {
  llvm_unreachable("");
}

void USimInstrInfo::loadRegFromStackSlot(MachineBasicBlock &MBB,
                                         MachineBasicBlock::iterator I,
                                         Register DestReg, int FrameIndex,
                                         const TargetRegisterClass *RC,
                                         const TargetRegisterInfo *TRI) const {
  llvm_unreachable("");
}

bool USimInstrInfo::reverseBranchCondition(
    SmallVectorImpl<MachineOperand> &Cond) const {
  llvm_unreachable("");
}

unsigned USimInstrInfo::insertBranch(
    MachineBasicBlock &MBB, MachineBasicBlock *TBB, MachineBasicBlock *FBB,
    ArrayRef<MachineOperand> Cond, const DebugLoc &DL, int *BytesAdded) const {
  llvm_unreachable("");
}

unsigned USimInstrInfo::getInstSizeInBytes(const MachineInstr &MI) const {
  llvm_unreachable("");
}

bool USimInstrInfo::getBaseAndOffsetPosition(const MachineInstr &MI,
                                             unsigned &BasePos,
                                             unsigned &OffsetPos) const {
  llvm_unreachable("");
}
