#include "USimMCTargetDesc.h"
#include "TargetInfo/USimTargetInfo.h"
#include "USimInfo.h"
#include "USimInstPrinter.h"
#include "USimMCAsmInfo.h"
#include "USimTargetStreamer.h"
#include "llvm/MC/MCDwarf.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FormattedStream.h"

using namespace llvm;

#define GET_INSTRINFO_MC_DESC
#include "USimGenInstrInfo.inc"

#define GET_SUBTARGETINFO_MC_DESC
#include "USimGenSubtargetInfo.inc"

#define GET_REGINFO_MC_DESC
#include "USimGenRegisterInfo.inc"

static MCInstrInfo *createUSimMCInstrInfo() {
  auto *X = new MCInstrInfo();
  InitUSimMCInstrInfo(X);
  return X;
}

static MCRegisterInfo *createUSimMCRegisterInfo(const Triple &TT) {
  auto *X = new MCRegisterInfo();
  InitUSimMCRegisterInfo(X, USim::R1);
  return X;
}

static MCSubtargetInfo *createUSimMCSubtargetInfo(const Triple &TT,
                                                  StringRef CPU, StringRef FS) {
  return createUSimMCSubtargetInfoImpl(TT, CPU, /*TuneCPU=*/CPU, FS);
}

static MCAsmInfo *createUSimMCAsmInfo(const MCRegisterInfo &MRI,
                                      const Triple &TT,
                                      const MCTargetOptions &Options) {
  MCAsmInfo *MAI = new USimMCAsmInfo(TT);
  MCRegister SP = MRI.getDwarfRegNum(USim::R2, true);
  MCCFIInstruction Inst = MCCFIInstruction::cfiDefCfa(nullptr, SP, 0);
  MAI->addInitialFrameState(Inst);
  return MAI;
}

static MCInstPrinter *createUSimMCInstPrinter(const Triple &T,
                                              unsigned SyntaxVariant,
                                              const MCAsmInfo &MAI,
                                              const MCInstrInfo &MII,
                                              const MCRegisterInfo &MRI) {
  return new USimInstPrinter(MAI, MII, MRI);
}

USimTargetStreamer::USimTargetStreamer(MCStreamer &S) : MCTargetStreamer(S) {}
USimTargetStreamer::~USimTargetStreamer() = default;

static MCTargetStreamer *createTargetAsmStreamer(MCStreamer &S,
                                                 formatted_raw_ostream &OS,
                                                 MCInstPrinter *InstPrint,
                                                 bool isVerboseAsm) {
  return new USimTargetStreamer(S);
}

// Force static initialization.
extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeUSimTargetMC() {
  // Register the MC asm info.
  Target &TheUSimTarget = getTheUSimTarget();
  RegisterMCAsmInfoFn X(TheUSimTarget, createUSimMCAsmInfo);

  // Register the MC instruction info.
  TargetRegistry::RegisterMCInstrInfo(TheUSimTarget, createUSimMCInstrInfo);

  // Register the MC register info.
  TargetRegistry::RegisterMCRegInfo(TheUSimTarget, createUSimMCRegisterInfo);

  // Register the MC subtarget info.
  TargetRegistry::RegisterMCSubtargetInfo(TheUSimTarget,
                                          createUSimMCSubtargetInfo);

  // Register the MCInstPrinter
  TargetRegistry::RegisterMCInstPrinter(TheUSimTarget, createUSimMCInstPrinter);

  TargetRegistry::RegisterAsmTargetStreamer(TheUSimTarget,
                                            createTargetAsmStreamer);
}
