//===----------------------------------------------------------------------===//
//
// This file contains the USim implementation of the TargetInstrInfo class.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// USim profiles and nodes
//===----------------------------------------------------------------------===//

def SDT_USimCallSeqStart : SDCallSeqStart<[ SDTCisVT<0, i32>,
                                           SDTCisVT<1, i32> ]>;
def SDT_USimCallSeqEnd   : SDCallSeqEnd<[ SDTCisVT<0, i32>,
                                         SDTCisVT<1, i32> ]>;

// These are target-independent nodes, but have target-specific formats.
def callseq_start : SDNode<"ISD::CALLSEQ_START", SDT_USimCallSeqStart,
                           [SDNPHasChain, SDNPOutGlue]>;
def callseq_end   : SDNode<"ISD::CALLSEQ_END",   SDT_USimCallSeqEnd,
                           [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue]>;

// Return
def USimRet : SDNode<"USimISD::RET", SDTNone,
                     [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;

//===----------------------------------------------------------------------===//
// Instruction format superclass
//===----------------------------------------------------------------------===//

include "USimInstrFormats.td"

//===----------------------------------------------------------------------===//
// Pseudo Instructions
//===----------------------------------------------------------------------===//

let Defs = [R2], Uses = [R2] in {
def ADJCALLSTACKDOWN : USimPseudoInst<(outs), (ins i32imm:$amt, i32imm:$amt2),
                               "# ADJCALLSTACKDOWN $amt, $amt2",
                               [(callseq_start timm:$amt, timm:$amt2)]>;
def ADJCALLSTACKUP : USimPseudoInst<(outs), (ins i32imm:$amt1, i32imm:$amt2),
                            "# ADJCALLSTACKUP $amt1",
                            [(callseq_end timm:$amt1, timm:$amt2)]>;
}

def IsOrAdd: PatFrag<(ops node:$A, node:$B), (or node:$A, node:$B), [{
  return isOrEquivalentToAdd(N);
}]>;

//===----------------------------------------------------------------------===//
// USim Operand, Complex Patterns and Transformations Definitions.
//===----------------------------------------------------------------------===//

class ImmAsmOperand<string prefix, int width, string suffix> : AsmOperandClass {
  let Name = prefix # "Imm" # width # suffix;
  let RenderMethod = "addImmOperands";
  let DiagnosticType = !strconcat("Invalid", Name);
}

class SImmAsmOperand<int width, string suffix = "">
    : ImmAsmOperand<"S", width, suffix> {
}

def simm16 : Operand<i32>, ImmLeaf<i32, [{return isInt<16>(Imm);}]> {
  let ParserMatchClass = SImmAsmOperand<16>;
  let EncoderMethod = "getImmOpValue";
  let DecoderMethod = "decodeSImmOperand<16>";
  let MCOperandPredicate = [{
    int64_t Imm;
    if (MCOp.evaluateAsConstantImm(Imm))
      return isInt<16>(Imm);
    return MCOp.isBareSymbolRef();
  }];
  let OperandType = "OPERAND_SIMM16";
  let OperandNamespace = "USimOp";
}

def LO16 : SDNodeXForm<imm, [{
  return CurDAG->getTargetConstant((uint64_t)N->getZExtValue() & 0xffff,
                                   SDLoc(N), MVT::i32);
}]>;

//def LoImm16AsmOperand : AsmOperandClass { let Name = "LoImm16"; }
def i32lo16u : Operand<i32>, PatLeaf<(i32 imm), [{
    return ((N->getZExtValue() & 0xFFFFUL) == N->getZExtValue());}], LO16> {
  //let ParserMatchClass = LoImm16AsmOperand;
}


// Addressing modes
def AddrFI : ComplexPattern<iPTR, 1, "SelectAddrFI", [frameindex], []>;
def BaseAddr : ComplexPattern<iPTR, 1, "SelectBaseAddr">;

//===----------------------------------------------------------------------===//
// Instructions specific format
//===----------------------------------------------------------------------===//

//@Branches {
let isBranch=1, isTerminator=1, isBarrier=1, hasDelaySlot = 1,
    isIndirectBranch = 1 in
class Branches<bits<8> op, string instr_asm, RegisterClass RC>:
  USimInst<op, (outs), (ins RC:$r1),
     !strconcat(instr_asm, "\t$r1"), [(brind RC:$r1)]> {
}
//@Branches }

//===----------------------------------------------------------------------===//
// Instruction definition
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// USim Instructions
//===----------------------------------------------------------------------===//

// ret TODO: pseudo
let isReturn = 1, isTerminator = 1, isBarrier = 1  in
def RET : USimInst<0x0, (outs), (ins), "jr\t[%r1]", [(USimRet)]> {
  let r1 = 1;
}

// movli
let isAsCheapAsAMove = 1, isCodeGenOnly = 1, isReMaterializable = 1 in
def MOVLI : USimInst<0x0, (outs GPR:$rd), (ins i32lo16u:$r3_imm),
                     "movli\t$r3_imm, $rd",
                     [(set GPR:$rd, i32lo16u:$r3_imm)]>;

// nop
def NOP   : USimInst<0x0, (outs), (ins), "nop", []>;

// sti, but not SubtargetInfo!
let hasSideEffects = 0, mayLoad = 0, mayStore = 1 in
def STI_: USimInst<0x0, (outs), (ins GPR:$rs2, GPR:$rs1, simm16:$r3_imm),
              "sti\t$rs2, ${r3_imm}(${rs1})",
              []>;
def : Pat<(store (i32 GPR:$rs2), BaseAddr:$rs1),
        (STI_ GPR:$rs2, BaseAddr:$rs1, 0)>;
def : Pat<(store (i32 GPR:$rs2), (add BaseAddr:$rs1, simm16:$r3_imm)),
        (STI_ GPR:$rs2, BaseAddr:$rs1, simm16:$r3_imm)>;
def : Pat<(store (i32 GPR:$rs2), (IsOrAdd AddrFI:$rs1, simm16:$r3_imm)),
        (STI_ GPR:$rs2, AddrFI:$rs1, simm16:$r3_imm)>;

// ldi
let hasSideEffects = 0, mayLoad = 1, mayStore = 0 in
def LDI: USimInst<0x0, (outs GPR:$rd), (ins GPR:$rs1, simm16:$r3_imm),
              "ldi\t${r3_imm}(${rs1}), $rd",
              []>;
def : Pat<(i32 (load BaseAddr:$rs1)), (LDI BaseAddr:$rs1, 0)>;
def : Pat<(i32 (load (add BaseAddr:$rs1, simm16:$r3_imm))),
          (LDI BaseAddr:$rs1, simm16:$r3_imm)>;
def : Pat<(i32 (load (IsOrAdd AddrFI:$rs1, simm16:$r3_imm))),
          (LDI AddrFI:$rs1, simm16:$r3_imm)>;

// addi
let isAsCheapAsAMove = 1 in
def ADDI : USimInst<0x0, (outs GPR:$rd), (ins GPR:$rs1, simm16:$r3_imm),
                     "addi\t$rs1, $r3_imm, $rd",
                     [(set GPR:$rd, (add GPR:$rs1, simm16:$r3_imm))]>;

