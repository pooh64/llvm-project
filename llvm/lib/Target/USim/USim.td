//===----------------------------------------------------------------------===//
// This is the top level entry point for the USim target.
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Target-independent interfaces
//===----------------------------------------------------------------------===//

include "llvm/Target/Target.td"

//===----------------------------------------------------------------------===//
// USim Subtarget features
//===----------------------------------------------------------------------===//

//def FeatureDummy
//    : SubtargetFeature<"dummy", "Dummy", "true",
//                       "Dummy feature">;

//===----------------------------------------------------------------------===//
// Target-dependent interfaces
//===----------------------------------------------------------------------===//

include "USimRegisterInfo.td"

//===----------------------------------------------------------------------===//
// Calling Conv, Instruction Descriptions
//===----------------------------------------------------------------------===//

include "USimCallingConv.td"

include "USimInstrInfo.td"
def USimInstrInfo : InstrInfo;

//===----------------------------------------------------------------------===//
// USim processors supported.
//===----------------------------------------------------------------------===//

class Proc<string Name, list<SubtargetFeature> Features>
 : Processor<Name, NoItineraries, Features>;

def : Proc<"generic", []>;
//def : Proc<"dummy-st", []>;

// Will generate USimGenAsmWrite.inc included by USimInstPrinter.cpp, contents
//  as follows,
// void USimInstPrinter::printInstruction(const MCInst *MI, raw_ostream &O) {...}
// const char *USimInstPrinter::getRegisterName(unsigned RegNo) {...}
def USim : Target {
  let InstructionSet = USimInstrInfo;
}
