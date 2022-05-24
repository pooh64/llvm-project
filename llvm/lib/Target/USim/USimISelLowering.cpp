#include "USimISelLowering.h"
#include "MCTargetDesc/USimInfo.h"
#include "USim.h"
#include "USimMachineFunctionInfo.h"
#include "USimRegisterInfo.h"
#include "USimSubtarget.h"
#include "USimTargetMachine.h"
#include "llvm/CodeGen/CallingConvLower.h"
#include "llvm/CodeGen/ISDOpcodes.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineJumpTableInfo.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/Register.h"
#include "llvm/CodeGen/SelectionDAGNodes.h"
#include "llvm/CodeGen/ValueTypes.h"
#include "llvm/IR/CallingConv.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MachineValueType.h"
#include <algorithm>

#define DEBUG_TYPE "USim-lower"

using namespace llvm;

static const MCPhysReg ArgGPRs[] = {USim::R9, USim::R10, USim::R11, USim::R12};

void USimTargetLowering::ReplaceNodeResults(SDNode *N,
                                            SmallVectorImpl<SDValue> &Results,
                                            SelectionDAG &DAG) const {
  llvm_unreachable("");
}

USimTargetLowering::USimTargetLowering(const TargetMachine &TM,
                                       const USimSubtarget &STI)
    : TargetLowering(TM), STI(STI) {
  addRegisterClass(MVT::i32, &USim::GPRRegClass);

  computeRegisterProperties(STI.getRegisterInfo());

  setStackPointerRegisterToSaveRestore(USim::R2);

  // setSchedulingPreference(Sched::Source);

  for (unsigned Opc = 0; Opc < ISD::BUILTIN_OP_END; ++Opc)
    setOperationAction(Opc, MVT::i32, Expand);

  setOperationAction(ISD::ADD, MVT::i32, Legal);
  setOperationAction(ISD::SUB, MVT::i32, Legal);
  setOperationAction(ISD::MUL, MVT::i32, Legal);
  setOperationAction(ISD::SDIV, MVT::i32, Legal);
  setOperationAction(ISD::UDIV, MVT::i32, Legal);
  setOperationAction(ISD::SREM, MVT::i32, Legal);
  setOperationAction(ISD::UREM, MVT::i32, Legal);
  setOperationAction(ISD::OR, MVT::i32, Legal);
  setOperationAction(ISD::AND, MVT::i32, Legal);
  setOperationAction(ISD::XOR, MVT::i32, Legal);
  setOperationAction(ISD::SHL, MVT::i32, Legal);
  setOperationAction(ISD::SRL, MVT::i32, Legal);
  setOperationAction(ISD::SRA, MVT::i32, Legal);

  setOperationAction(ISD::LOAD, MVT::i32, Legal);
  setOperationAction(ISD::STORE, MVT::i32, Legal);

  setOperationAction(ISD::Constant, MVT::i32, Legal);
  setOperationAction(ISD::UNDEF, MVT::i32, Legal);

  setOperationAction(ISD::BR_CC, MVT::i32, Custom);
  setOperationAction(ISD::SELECT_CC, MVT::i32, Custom);

  setOperationAction(ISD::FRAMEADDR, MVT::i32, Legal);
  // setOperationAction(ISD::FrameIndex, MVT::i32, Custom);
  // setOperationAction(ISD::GlobalAddress, MVT::i32, Custom);
}

const char *USimTargetLowering::getTargetNodeName(unsigned Opcode) const {
  switch (Opcode) {
  case USimISD::CALL:
    return "USimISD::CALL";
  case USimISD::RET:
    return "USimISD::RET";
  }
  return nullptr;
}

//===----------------------------------------------------------------------===//
//  Misc Lower Operation implementation
//===----------------------------------------------------------------------===//

#include "USimGenCallingConv.inc"

//===----------------------------------------------------------------------===//
//                  Call Calling Convention Implementation
//===----------------------------------------------------------------------===//

static Align getPrefTypeAlign(EVT VT, SelectionDAG &DAG) {
  return DAG.getDataLayout().getPrefTypeAlign(
      VT.getTypeForEVT(*DAG.getContext()));
}

// TODO: rewrite
SDValue USimTargetLowering::LowerCall(TargetLowering::CallLoweringInfo &CLI,
                                      SmallVectorImpl<SDValue> &InVals) const {
  SelectionDAG &DAG = CLI.DAG;
  SDLoc &DL = CLI.DL;
  SmallVectorImpl<ISD::OutputArg> &Outs = CLI.Outs;
  SmallVectorImpl<SDValue> &OutVals = CLI.OutVals;
  SmallVectorImpl<ISD::InputArg> &Ins = CLI.Ins;
  SDValue Chain = CLI.Chain;
  SDValue Callee = CLI.Callee;
  CLI.IsTailCall = false; // assert(!CLI.IsTailCall); // tolerate
  CallingConv::ID CallConv = CLI.CallConv;
  bool IsVarArg = CLI.IsVarArg;
  EVT PtrVT = getPointerTy(DAG.getDataLayout());

  MachineFunction &MF = DAG.getMachineFunction();

  // Analyze the operands of the call, assigning locations to each operand.
  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(CallConv, IsVarArg, MF, ArgLocs, *DAG.getContext());
  CCInfo.AnalyzeCallOperands(Outs, CC_USim);

  // Get a count of how many bytes are to be pushed on the stack.
  unsigned NumBytes = CCInfo.getNextStackOffset();

  // Create local copies for byval args
  SmallVector<SDValue, 8> ByValArgs;
  for (unsigned i = 0, e = Outs.size(); i != e; ++i) {
    ISD::ArgFlagsTy Flags = Outs[i].Flags;
    if (!Flags.isByVal())
      continue;

    SDValue Arg = OutVals[i];
    unsigned Size = Flags.getByValSize();
    Align Alignment = Flags.getNonZeroByValAlign();

    int FI =
        MF.getFrameInfo().CreateStackObject(Size, Alignment, /*isSS=*/false);
    SDValue FIPtr = DAG.getFrameIndex(FI, getPointerTy(DAG.getDataLayout()));
    SDValue SizeNode = DAG.getConstant(Size, DL, MVT::i32);

    Chain = DAG.getMemcpy(Chain, DL, FIPtr, Arg, SizeNode, Alignment,
                          /*IsVolatile=*/false,
                          /*AlwaysInline=*/false, false, MachinePointerInfo(),
                          MachinePointerInfo());
    ByValArgs.push_back(FIPtr);
  }

  Chain = DAG.getCALLSEQ_START(Chain, NumBytes, 0, CLI.DL);

  // Copy argument values to their designated locations.
  SmallVector<std::pair<Register, SDValue>, 8> RegsToPass;
  SmallVector<SDValue, 8> MemOpChains;
  SDValue StackPtr;
  for (unsigned i = 0, j = 0, e = ArgLocs.size(); i != e; ++i) {
    CCValAssign &VA = ArgLocs[i];
    SDValue ArgValue = OutVals[i];
    ISD::ArgFlagsTy Flags = Outs[i].Flags;
    // Promote the value if needed.
    // For now, only handle fully promoted and indirect arguments.
    if (VA.getLocInfo() == CCValAssign::Indirect) {
      // Store the argument in a stack slot and pass its address.
      Align StackAlign =
          std::max(getPrefTypeAlign(Outs[i].ArgVT, DAG),
                   getPrefTypeAlign(ArgValue.getValueType(), DAG));
      TypeSize StoredSize = ArgValue.getValueType().getStoreSize();
      unsigned ArgIndex = Outs[i].OrigArgIndex;
      unsigned ArgPartOffset = Outs[i].PartOffset;
      assert(ArgPartOffset == 0);
      // Calculate the total size to store. We don't have access to what we're
      // actually storing other than performing the loop and collecting the
      // info.
      SmallVector<std::pair<SDValue, SDValue>> Parts;
      while (i + 1 != e && Outs[i + 1].OrigArgIndex == ArgIndex) {
        SDValue PartValue = OutVals[i + 1];
        unsigned PartOffset = Outs[i + 1].PartOffset - ArgPartOffset;
        SDValue Offset = DAG.getIntPtrConstant(PartOffset, DL);
        EVT PartVT = PartValue.getValueType();
        StoredSize += PartVT.getStoreSize();
        StackAlign = std::max(StackAlign, getPrefTypeAlign(PartVT, DAG));
        Parts.push_back(std::make_pair(PartValue, Offset));
        ++i;
      }
      SDValue SpillSlot = DAG.CreateStackTemporary(StoredSize, StackAlign);
      int FI = cast<FrameIndexSDNode>(SpillSlot)->getIndex();
      MemOpChains.push_back(
          DAG.getStore(Chain, DL, ArgValue, SpillSlot,
                       MachinePointerInfo::getFixedStack(MF, FI)));
      for (const auto &Part : Parts) {
        SDValue PartValue = Part.first;
        SDValue PartOffset = Part.second;
        SDValue Address =
            DAG.getNode(ISD::ADD, DL, PtrVT, SpillSlot, PartOffset);
        MemOpChains.push_back(
            DAG.getStore(Chain, DL, PartValue, Address,
                         MachinePointerInfo::getFixedStack(MF, FI)));
      }
      ArgValue = SpillSlot;
    } else {
      assert(VA.getLocInfo() == CCValAssign::Full);
    }

    // Use local copy if it is a byval arg.
    if (Flags.isByVal())
      ArgValue = ByValArgs[j++];

    if (VA.isRegLoc()) {
      // Queue up the argument copies and emit them at the end.
      RegsToPass.push_back(std::make_pair(VA.getLocReg(), ArgValue));
    } else {
      assert(VA.isMemLoc() && "Argument not register or memory");

      // Work out the address of the stack slot.
      if (!StackPtr.getNode())
        StackPtr = DAG.getCopyFromReg(Chain, DL, USim::SP, PtrVT);
      SDValue Address =
          DAG.getNode(ISD::ADD, DL, PtrVT, StackPtr,
                      DAG.getIntPtrConstant(VA.getLocMemOffset(), DL));

      // Emit the store.
      MemOpChains.push_back(
          DAG.getStore(Chain, DL, ArgValue, Address, MachinePointerInfo()));
    }
  }

  // Join the stores, which are independent of one another.
  if (!MemOpChains.empty())
    Chain = DAG.getNode(ISD::TokenFactor, DL, MVT::Other, MemOpChains);

  SDValue Glue;

  // Build a sequence of copy-to-reg nodes, chained and glued together.
  for (auto &Reg : RegsToPass) {
    Chain = DAG.getCopyToReg(Chain, DL, Reg.first, Reg.second, Glue);
    Glue = Chain.getValue(1);
  }

  // No external symbols support
  if (GlobalAddressSDNode *S = dyn_cast<GlobalAddressSDNode>(Callee)) {
    // llvm_unreachable("How do i suppose to lower this?");
    const GlobalValue *GV = S->getGlobal();
    assert(getTargetMachine().shouldAssumeDSOLocal(*GV->getParent(), GV));
    Callee = DAG.getTargetGlobalAddress(GV, DL, PtrVT, 0, 0);
  }

  // The first call operand is the chain and the second is the target address.
  SmallVector<SDValue, 8> Ops;
  Ops.push_back(Chain);
  Ops.push_back(Callee);

  // Add argument registers to the end of the list so that they are
  // known live into the call.
  for (auto &Reg : RegsToPass)
    Ops.push_back(DAG.getRegister(Reg.first, Reg.second.getValueType()));

  // Add a register mask operand representing the call-preserved registers.
  const TargetRegisterInfo *TRI = STI.getRegisterInfo();
  const uint32_t *Mask = TRI->getCallPreservedMask(MF, CallConv);
  Ops.push_back(DAG.getRegisterMask(Mask));

  // Glue the call to the argument copies, if any.
  if (Glue.getNode())
    Ops.push_back(Glue);

  // Emit the call.
  SDVTList NodeTys = DAG.getVTList(MVT::Other, MVT::Glue);

  Chain = DAG.getNode(USimISD::CALL, DL, NodeTys, Ops);
  DAG.addNoMergeSiteInfo(Chain.getNode(), CLI.NoMerge);
  Glue = Chain.getValue(1);

  // Mark the end of the call, which is glued to the call itself.
  Chain = DAG.getCALLSEQ_END(Chain, DAG.getConstant(NumBytes, DL, PtrVT, true),
                             DAG.getConstant(0, DL, PtrVT, true), Glue, DL);
  Glue = Chain.getValue(1);

  // Assign locations to each value returned by this call.
  SmallVector<CCValAssign, 16> RVLocs;
  CCState RetCCInfo(CallConv, IsVarArg, MF, RVLocs, *DAG.getContext());
  RetCCInfo.AnalyzeCallResult(Ins, RetCC_USim);

  // Copy all of the result registers out of their specified physreg.
  for (auto &VA : RVLocs) {
    // Copy the value out
    SDValue RetValue =
        DAG.getCopyFromReg(Chain, DL, VA.getLocReg(), VA.getLocVT(), Glue);
    // Glue the RetValue to the end of the call sequence
    Chain = RetValue.getValue(1);
    Glue = RetValue.getValue(2);

    assert(VA.getLocInfo() == CCValAssign::Full);
    InVals.push_back(RetValue);
  }

  return Chain;
}

//===----------------------------------------------------------------------===//
//             Formal Arguments Calling Convention Implementation
//===----------------------------------------------------------------------===//

namespace {

struct ArgDataPair {
  SDValue SDV;
  ISD::ArgFlagsTy Flags;
};

} // end anonymous namespace

// TODO: rewrite
static SDValue convertValVTToLocVT(SelectionDAG &DAG, SDValue Val,
                                   const CCValAssign &VA, const SDLoc &DL,
                                   const USimSubtarget &Subtarget) {
  EVT LocVT = VA.getLocVT();

  if (VA.getValVT() == MVT::f32)
    llvm_unreachable("");

  switch (VA.getLocInfo()) {
  default:
    llvm_unreachable("Unexpected CCValAssign::LocInfo");
  case CCValAssign::Full:
    break;
  case CCValAssign::BCvt:
    llvm_unreachable("");
    Val = DAG.getNode(ISD::BITCAST, DL, LocVT, Val);
    break;
  }
  return Val;
}

// Convert Val to a ValVT. Should not be called for CCValAssign::Indirect
// values.
// TODO: rewrite
static SDValue convertLocVTToValVT(SelectionDAG &DAG, SDValue Val,
                                   const CCValAssign &VA, const SDLoc &DL,
                                   const USimSubtarget &Subtarget) {
  if (VA.getValVT() == MVT::f32)
    llvm_unreachable("");

  switch (VA.getLocInfo()) {
  default:
    llvm_unreachable("Unexpected CCValAssign::LocInfo");
  case CCValAssign::Full:
    break;
  case CCValAssign::BCvt:
    llvm_unreachable("");
    Val = DAG.getNode(ISD::BITCAST, DL, VA.getValVT(), Val);
  }
  return Val;
}

// TODO: rewrite
static SDValue unpackFromRegLoc(SelectionDAG &DAG, SDValue Chain,
                                const CCValAssign &VA, const SDLoc &DL,
                                const USimTargetLowering &TLI) {
  MachineFunction &MF = DAG.getMachineFunction();
  MachineRegisterInfo &RegInfo = MF.getRegInfo();
  EVT LocVT = VA.getLocVT();
  SDValue Val;
  const TargetRegisterClass *RC = TLI.getRegClassFor(LocVT.getSimpleVT());
  Register VReg = RegInfo.createVirtualRegister(RC);
  RegInfo.addLiveIn(VA.getLocReg(), VReg);
  Val = DAG.getCopyFromReg(Chain, DL, VReg, LocVT);

  if (VA.getLocInfo() == CCValAssign::Indirect)
    return Val;

  return convertLocVTToValVT(DAG, Val, VA, DL, TLI.getSubtarget());
}

// The caller is responsible for loading the full value if the argument is
// passed with CCValAssign::Indirect.
// TODO: rewrite
static SDValue unpackFromMemLoc(SelectionDAG &DAG, SDValue Chain,
                                const CCValAssign &VA, const SDLoc &DL) {
  MachineFunction &MF = DAG.getMachineFunction();
  MachineFrameInfo &MFI = MF.getFrameInfo();
  EVT LocVT = VA.getLocVT();
  EVT ValVT = VA.getValVT();
  EVT PtrVT = MVT::getIntegerVT(DAG.getDataLayout().getPointerSizeInBits(0));
  int FI = MFI.CreateFixedObject(ValVT.getStoreSize(), VA.getLocMemOffset(),
                                 /*IsImmutable=*/true);
  SDValue FIN = DAG.getFrameIndex(FI, PtrVT);
  SDValue Val;

  ISD::LoadExtType ExtType;
  switch (VA.getLocInfo()) {
  default:
    llvm_unreachable("Unexpected CCValAssign::LocInfo");
  case CCValAssign::Full:
  case CCValAssign::Indirect:
  case CCValAssign::BCvt:
    ExtType = ISD::NON_EXTLOAD;
    break;
  }
  Val = DAG.getExtLoad(
      ExtType, DL, LocVT, Chain, FIN,
      MachinePointerInfo::getFixedStack(DAG.getMachineFunction(), FI), ValVT);
  return Val;
}

/// USim formal arguments implementation
// TODO: rewrite
SDValue USimTargetLowering::LowerFormalArguments(
    SDValue Chain, CallingConv::ID CallConv, bool IsVarArg,
    const SmallVectorImpl<ISD::InputArg> &Ins, const SDLoc &DL,
    SelectionDAG &DAG, SmallVectorImpl<SDValue> &InVals) const {
  switch (CallConv) {
  default:
    report_fatal_error("Unsupported calling convention");
  case CallingConv::C:
  case CallingConv::Fast:
    break;
  }

  MachineFunction &MF = DAG.getMachineFunction();
  EVT PtrVT = getPointerTy(DAG.getDataLayout());
  unsigned StackSlotSize = MVT(MVT::i32).getSizeInBits() / 8;
  // Used with vargs to acumulate store chains.
  std::vector<SDValue> OutChains;

  // Assign locations to all of the incoming arguments.
  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(CallConv, IsVarArg, MF, ArgLocs, *DAG.getContext());
  CCInfo.AnalyzeFormalArguments(Ins, CC_USim);

  for (unsigned i = 0, e = ArgLocs.size(); i != e; ++i) {
    CCValAssign &VA = ArgLocs[i];
    SDValue ArgValue;
    if (VA.isRegLoc())
      ArgValue = unpackFromRegLoc(DAG, Chain, VA, DL, *this);
    else
      ArgValue = unpackFromMemLoc(DAG, Chain, VA, DL);

    if (VA.getLocInfo() == CCValAssign::Indirect) {
      InVals.push_back(DAG.getLoad(VA.getValVT(), DL, Chain, ArgValue,
                                   MachinePointerInfo()));
      unsigned ArgIndex = Ins[i].OrigArgIndex;
      unsigned ArgPartOffset = Ins[i].PartOffset;
      assert(ArgPartOffset == 0);
      while (i + 1 != e && Ins[i + 1].OrigArgIndex == ArgIndex) {
        CCValAssign &PartVA = ArgLocs[i + 1];
        unsigned PartOffset = Ins[i + 1].PartOffset - ArgPartOffset;
        SDValue Offset = DAG.getIntPtrConstant(PartOffset, DL);
        SDValue Address = DAG.getNode(ISD::ADD, DL, PtrVT, ArgValue, Offset);
        InVals.push_back(DAG.getLoad(PartVA.getValVT(), DL, Chain, Address,
                                     MachinePointerInfo()));
        ++i;
      }
      continue;
    }
    InVals.push_back(ArgValue);
  }

  if (IsVarArg) {
    ArrayRef<MCPhysReg> ArgRegs = makeArrayRef(ArgGPRs);
    unsigned Idx = CCInfo.getFirstUnallocated(ArgRegs);
    const TargetRegisterClass *RC = &USim::GPRRegClass;
    MachineFrameInfo &MFI = MF.getFrameInfo();
    MachineRegisterInfo &RegInfo = MF.getRegInfo();
    USimFunctionInfo *UFI = MF.getInfo<USimFunctionInfo>();

    int VaArgOffset, VarArgsSaveSize;

    // If all registers are allocated, then all varargs must be passed on the
    // stack and we don't need to save any argregs.
    if (ArgRegs.size() == Idx) {
      VaArgOffset = CCInfo.getNextStackOffset();
      VarArgsSaveSize = 0;
    } else {
      VarArgsSaveSize = StackSlotSize * (ArgRegs.size() - Idx);
      VaArgOffset = -VarArgsSaveSize;
    }

    // Record the frame index of the first variable argument
    // which is a value necessary to VASTART.
    int FI = MFI.CreateFixedObject(StackSlotSize, VaArgOffset, true);
    UFI->setVarArgsFrameIndex(FI);

    // If saving an odd number of registers then create an extra stack slot to
    // ensure that the frame pointer is 2*XLEN-aligned, which in turn ensures
    // offsets to even-numbered registered remain 2*XLEN-aligned.
    if (Idx % 2) {
      MFI.CreateFixedObject(StackSlotSize, VaArgOffset - (int)StackSlotSize,
                            true);
      VarArgsSaveSize += StackSlotSize;
    }

    // Copy the integer registers that may have been used for passing varargs
    // to the vararg save area.
    for (unsigned I = Idx; I < ArgRegs.size();
         ++I, VaArgOffset += StackSlotSize) {
      const Register Reg = RegInfo.createVirtualRegister(RC);
      RegInfo.addLiveIn(ArgRegs[I], Reg);
      SDValue ArgValue = DAG.getCopyFromReg(Chain, DL, Reg, MVT::i32);
      FI = MFI.CreateFixedObject(StackSlotSize, VaArgOffset, true);
      SDValue PtrOff = DAG.getFrameIndex(FI, getPointerTy(DAG.getDataLayout()));
      SDValue Store = DAG.getStore(Chain, DL, ArgValue, PtrOff,
                                   MachinePointerInfo::getFixedStack(MF, FI));
      cast<StoreSDNode>(Store.getNode())
          ->getMemOperand()
          ->setValue((Value *)nullptr);
      OutChains.push_back(Store);
    }
    UFI->setVarArgsSaveSize(VarArgsSaveSize);
  }

  // All stores are grouped in one node to allow the matching between
  // the size of Ins and InVals. This only happens for vararg functions.
  if (!OutChains.empty()) {
    OutChains.push_back(Chain);
    Chain = DAG.getNode(ISD::TokenFactor, DL, MVT::Other, OutChains);
  }

  return Chain;
}

//===----------------------------------------------------------------------===//
//               Return Value Calling Convention Implementation
//===----------------------------------------------------------------------===//

bool USimTargetLowering::CanLowerReturn(
    CallingConv::ID CallConv, MachineFunction &MF, bool IsVarArg,
    const SmallVectorImpl<ISD::OutputArg> &Outs, LLVMContext &Context) const {
  SmallVector<CCValAssign, 16> RVLocs;
  CCState CCInfo(CallConv, IsVarArg, MF, RVLocs, Context);
  if (!CCInfo.CheckReturn(Outs, RetCC_USim))
    return false;
  if (CCInfo.getNextStackOffset() != 0 && IsVarArg)
    llvm_unreachable(""); // TODO: what for
  return true;
}

// TODO: rewrite
SDValue
USimTargetLowering::LowerReturn(SDValue Chain, CallingConv::ID CallConv,
                                bool IsVarArg,
                                const SmallVectorImpl<ISD::OutputArg> &Outs,
                                const SmallVectorImpl<SDValue> &OutVals,
                                const SDLoc &DL, SelectionDAG &DAG) const {
  const MachineFunction &MF = DAG.getMachineFunction();
  const USimSubtarget &STI = MF.getSubtarget<USimSubtarget>();

  // Stores the assignment of the return value to a location.
  SmallVector<CCValAssign, 16> RVLocs;

  // Info about the registers and stack slot.
  CCState CCInfo(CallConv, IsVarArg, DAG.getMachineFunction(), RVLocs,
                 *DAG.getContext());

  CCInfo.AnalyzeReturn(Outs, RetCC_USim);

  SDValue Glue;
  SmallVector<SDValue, 4> RetOps(1, Chain);

  // Copy the result values into the output registers.
  for (unsigned i = 0, e = RVLocs.size(); i < e; ++i) {
    SDValue Val = OutVals[i];
    CCValAssign &VA = RVLocs[i];
    assert(VA.isRegLoc() && "Can only return in registers!");

    Val = convertValVTToLocVT(DAG, Val, VA, DL, STI);
    Chain = DAG.getCopyToReg(Chain, DL, VA.getLocReg(), Val, Glue);

    // Guarantee that all emitted copies are stuck together.
    Glue = Chain.getValue(1);
    RetOps.push_back(DAG.getRegister(VA.getLocReg(), VA.getLocVT()));
  }

  RetOps[0] = Chain; // Update chain.
  // Add the glue node if we have it.
  if (Glue.getNode()) {
    RetOps.push_back(Glue);
  }
  return DAG.getNode(USimISD::RET, DL, MVT::Other, RetOps);
}

//===----------------------------------------------------------------------===//
// Target Optimization Hooks
//===----------------------------------------------------------------------===//

SDValue USimTargetLowering::PerformDAGCombine(SDNode *N,
                                              DAGCombinerInfo &DCI) const {
  // TODO: advanced opts
  return {};
}

//===----------------------------------------------------------------------===//
//  Addressing mode description hooks
//===----------------------------------------------------------------------===//

/// Return true if the addressing mode represented by AM is legal for this
/// target, for a load/store of the specified type.
// TODO: verify
bool USimTargetLowering::isLegalAddressingMode(const DataLayout &DL,
                                               const AddrMode &AM, Type *Ty,
                                               unsigned AS,
                                               Instruction *I) const {
  // No global is ever allowed as a base.
  if (AM.BaseGV)
    return false;

  if (!isInt<16>(AM.BaseOffs))
    return false;

  switch (AM.Scale) {
  case 0: // "r+i" or just "i", depending on HasBaseReg.
    break;
  case 1:
    if (!AM.HasBaseReg) // allow "r+i".
      break;
    return false; // disallow "r+r" or "r+r+i".
  default:
    return false;
  }

  return true;
}

// Don't emit tail calls for the time being.
bool USimTargetLowering::mayBeEmittedAsTailCall(const CallInst *CI) const {
  return false;
}

template <typename Opnd>
static void translateSetCCForBranch(Opnd &LHS, Opnd &RHS, ISD::CondCode &CC) {
  switch (CC) {
  default:
    break;
  case ISD::SETLT:
  case ISD::SETGE:
  case ISD::SETULT:
  case ISD::SETUGE:
    CC = ISD::getSetCCSwappedOperands(CC);
    std::swap(LHS, RHS);
    break;
  }
}

// TODO: style
SDValue USimTargetLowering::lowerBR_CC(SDValue Op, SelectionDAG &DAG) const {
  ISD::CondCode CCVal = cast<CondCodeSDNode>(Op.getOperand(1))->get();
  SDValue LHS = Op.getOperand(2);
  SDValue RHS = Op.getOperand(3);
  SDValue Block = Op->getOperand(4);
  SDLoc DL(Op);

  assert(LHS.getValueType() == MVT::i32);

  translateSetCCForBranch(LHS, RHS, CCVal);
  SDValue TargetCC = DAG.getCondCode(CCVal);

  return DAG.getNode(USimISD::BR_CC, DL, Op.getValueType(), Op.getOperand(0),
                     LHS, RHS, TargetCC, Block);
}

SDValue USimTargetLowering::lowerSELECT_CC(SDValue Op,
                                           SelectionDAG &DAG) const {
  SDValue LHS = Op.getOperand(0);
  SDValue RHS = Op.getOperand(1);
  SDValue TrueV = Op.getOperand(2);
  SDValue FalseV = Op.getOperand(3);
  ISD::CondCode CCVal = cast<CondCodeSDNode>(Op->getOperand(4))->get();
  SDLoc DL(Op);

  if (TrueV == FalseV)
    return TrueV;

  translateSetCCForBranch(LHS, RHS, CCVal);
  SDValue TargetCC = DAG.getCondCode(CCVal);
  SDValue Ops[] = {LHS, RHS, TargetCC, TrueV, FalseV};

  return DAG.getNode(USimISD::SELECT_CC, DL, Op.getValueType(), Ops);
}

static MachineBasicBlock *emitSelect(MachineInstr &MI, MachineBasicBlock *BB,
                                     const USimSubtarget &STI) {

  const USimInstrInfo *TII = STI.getInstrInfo();
  DebugLoc DL = MI.getDebugLoc();

  const BasicBlock *LLVM_BB = BB->getBasicBlock();
  MachineFunction::iterator I = ++BB->getIterator();
  MachineBasicBlock *MBBHeader = BB;

  MachineFunction *F = BB->getParent();
  MachineBasicBlock *MBBTail = F->CreateMachineBasicBlock(LLVM_BB);
  MachineBasicBlock *MBBFalse = F->CreateMachineBasicBlock(LLVM_BB);
  F->insert(I, MBBFalse);
  F->insert(I, MBBTail);

  MBBTail->splice(MBBTail->begin(), MBBHeader,
                  std::next(MachineBasicBlock::iterator(MI)), MBBHeader->end());
  MBBTail->transferSuccessorsAndUpdatePHIs(MBBHeader);
  MBBHeader->addSuccessor(MBBFalse);
  MBBHeader->addSuccessor(MBBTail);

  auto CC = static_cast<USimCC::CondCode>(MI.getOperand(3).getImm());
  BuildMI(MBBHeader, DL, STI.getInstrInfo()->getBrCond(CC))
      .addReg(MI.getOperand(1).getReg())
      .addReg(MI.getOperand(2).getReg())
      .addMBB(MBBTail);
  MBBFalse->addSuccessor(MBBTail);

  BuildMI(*MBBTail, MBBTail->begin(), DL, TII->get(USim::PHI),
          MI.getOperand(0).getReg())
      .addReg(MI.getOperand(4).getReg())
      .addMBB(MBBHeader)
      .addReg(MI.getOperand(5).getReg())
      .addMBB(MBBFalse);

  MI.eraseFromParent();
  return MBBTail;
}

MachineBasicBlock *
USimTargetLowering::EmitInstrWithCustomInserter(MachineInstr &MI,
                                                MachineBasicBlock *BB) const {
  switch (MI.getOpcode()) {
  case USim::Select_CC_GPR:
    return emitSelect(MI, BB, STI);
  default:
    llvm_unreachable("");
  }
}

SDValue USimTargetLowering::lowerFRAMEADDR(SDValue Op,
                                           SelectionDAG &DAG) const {
  const USimRegisterInfo &RI = *STI.getRegisterInfo();
  MachineFunction &MF = DAG.getMachineFunction();
  MachineFrameInfo &MFI = MF.getFrameInfo();
  MFI.setFrameAddressIsTaken(true);
  Register FrameReg = RI.getFrameRegister(MF);
  EVT VT = Op.getValueType();
  SDLoc DL(Op);
  SDValue FrameAddr = DAG.getCopyFromReg(DAG.getEntryNode(), DL, FrameReg, VT);
  // Only for current frame
  assert(cast<ConstantSDNode>(Op.getOperand(0))->getZExtValue() == 0);
  return FrameAddr;
}

SDValue USimTargetLowering::LowerOperation(SDValue Op,
                                           SelectionDAG &DAG) const {
  switch (Op->getOpcode()) {
  case ISD::BR_CC:
    return lowerBR_CC(Op, DAG);
  case ISD::SELECT_CC:
    return lowerSELECT_CC(Op, DAG);
  case ISD::FRAMEADDR:
    return lowerFRAMEADDR(Op, DAG);
  default:
    llvm_unreachable("");
  }
}
