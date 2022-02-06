#include "MCTargetDesc/USimInstPrinter.h"
#include "TargetInfo/USimTargetInfo.h"
#include "USim.h"
#include "USimSubtarget.h"
#include "USimTargetMachine.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/CodeGen/MachineConstantPool.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

#define DEBUG_TYPE "asm-printer"

namespace {

class USimAsmPrinter : public AsmPrinter {
  const MCSubtargetInfo *STI;

public:
  explicit USimAsmPrinter(TargetMachine &TM,
                          std::unique_ptr<MCStreamer> Streamer)
      : AsmPrinter(TM, std::move(Streamer)), STI(TM.getMCSubtargetInfo()) {}

  StringRef getPassName() const override { return "USim Assembly Printer"; }

  void emitInstruction(const MachineInstr *MI) override;
  bool runOnMachineFunction(MachineFunction &MF) override;
};

} // end anonymous namespace

void USimAsmPrinter::emitInstruction(const MachineInstr *MI) {
  llvm_unreachable("");
#if 0
  MCInst TmpInst;
  if (!lowerUSimMachineInstrToMCInst(MI, TmpInst, *this))
    AsmPrinter::EmitToStreamer(*OutStreamer, TmpInst);
#endif
}

bool USimAsmPrinter::runOnMachineFunction(MachineFunction &MF) {
  // Set the current MCSubtargetInfo to a copy which has the correct
  // feature bits for the current MachineFunction
  MCSubtargetInfo &NewSTI =
      OutStreamer->getContext().getSubtargetCopy(*TM.getMCSubtargetInfo());
  NewSTI.setFeatureBits(MF.getSubtarget().getFeatureBits());
  STI = &NewSTI;

  SetupMachineFunction(MF);
  emitFunctionBody();
  return false;
}

// Force static initialization.
extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeUSimAsmPrinter() {
  RegisterAsmPrinter<USimAsmPrinter> X(getTheUSimTarget());
}
