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

def SDT_USimBrCC : SDTypeProfile<0, 4, [SDTCisSameAs<0, 1>,
                                         SDTCisVT<2, OtherVT>,
                                         SDTCisVT<3, OtherVT>]>;

// These are target-independent nodes, but have target-specific formats.
def callseq_start : SDNode<"ISD::CALLSEQ_START", SDT_USimCallSeqStart,
                           [SDNPHasChain, SDNPOutGlue]>;
def callseq_end   : SDNode<"ISD::CALLSEQ_END",   SDT_USimCallSeqEnd,
                           [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue]>;

def usim_ret      : SDNode<"USimISD::RET", SDTNone,
                            [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;

def usim_brcc     : SDNode<"USimISD::BR_CC", SDT_USimBrCC,
                            [SDNPHasChain]>;

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

class UImmAsmOperand<int width, string suffix = "">
    : ImmAsmOperand<"U", width, suffix> {
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

def uimm16 : Operand<i32>, ImmLeaf<i32, [{return isUInt<16>(Imm);}]> {
  let ParserMatchClass = UImmAsmOperand<16>;
  let EncoderMethod = "getImmOpValue";
  let DecoderMethod = "decodeUImmOperand<16>";
  let MCOperandPredicate = [{
    int64_t Imm;
    if (MCOp.evaluateAsConstantImm(Imm))
      return isUInt<16>(Imm);
    return MCOp.isBareSymbolRef();
  }];
  let OperandType = "OPERAND_UIMM16";
  let OperandNamespace = "USimOp";
}

def btarget16 : Operand<OtherVT> {
  let ParserMatchClass = SImmAsmOperand<16>;
  let PrintMethod = "printBranchOperand";
  let EncoderMethod = "getImmOpValue";
  let DecoderMethod = "decodeSImmOperand<16>";
  let MCOperandPredicate = [{
    int64_t Imm;
    if (MCOp.evaluateAsConstantImm(Imm))
      return isInt<16>(Imm);
    return MCOp.isBareSymbolRef();
  }];
  let OperandType = "OPERAND_PCREL";
}

def LI16 : SDNodeXForm<imm, [{
  return CurDAG->getTargetConstant((uint64_t)N->getZExtValue() & 0xffff,
                                   SDLoc(N), MVT::i32);
}]>;

def HI16 : SDNodeXForm<imm, [{
  return CurDAG->getTargetConstant((uint64_t)N->getZExtValue() >> 16, SDLoc(N),
                                   MVT::i32);
}]>;

// def LiImm16AsmOperand : AsmOperandClass { let Name = "LiImm16"; }
def i32li16u : Operand<i32>, PatLeaf<(i32 imm), [{
    return ((N->getZExtValue() & 0xFFFFUL) == N->getZExtValue());}], LI16> {
  // let ParserMatchClass = LiImm16AsmOperand;
}

// def HiImm16AsmOperand : AsmOperandClass { let Name = "HiImm16"; }
def i32hi16 : Operand<i32>, PatLeaf<(i32 imm), [{
    // i32hi16 predicate - true if the 32-bit immediate has only leftmost 16
    // bits set.
    return ((N->getZExtValue() & 0xFFFF0000UL) == N->getZExtValue());}], HI16> {
  // let ParserMatchClass = HiImm16AsmOperand;
  // let PrintMethod = "printHi16ImmOperand";
}

// Addressing modes
def AddrFI : ComplexPattern<iPTR, 1, "SelectAddrFI", [frameindex], []>;
def BaseAddr : ComplexPattern<iPTR, 1, "SelectBaseAddr">;

class PatGpr<SDPatternOperator OpNode, USimInst Inst>
    : Pat<(OpNode GPR:$rs1), (Inst GPR:$rs1)>;
class PatGprGpr<SDPatternOperator OpNode, USimInst Inst>
    : Pat<(OpNode GPR:$rs1, GPR:$rs2), (Inst GPR:$rs1, GPR:$rs2)>;
class PatGprImm<SDPatternOperator OpNode, USimInst Inst, ImmLeaf ImmType>
    : Pat<(i32 (OpNode (i32 GPR:$rs1), ImmType:$imm)),
          (Inst GPR:$rs1, ImmType:$imm)>;

class PatBcc<CondCode Cond, USimInst Inst>
    : Pat<(usim_brcc GPR:$rs1, GPR:$rs2, Cond, bb:$imm16),
          (Inst GPR:$rs1, GPR:$rs2, btarget16:$imm16)>;

//===----------------------------------------------------------------------===//
// Instructions specific format
//===----------------------------------------------------------------------===//

let hasSideEffects = 0, mayLoad = 0, mayStore = 0, isBranch = 1, isTerminator = 1 in
class BranchCC<bits<8> op, string opcodestr>
    : USimInst<op, (outs), (ins GPR:$rs1, GPR:$rs2, btarget16:$imm16),
              !strconcat(opcodestr, "\t$rs1, $rs2"), []>;

//===----------------------------------------------------------------------===//
// Instruction definition
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// USim Instructions
//===----------------------------------------------------------------------===//

def BEQ  : BranchCC<0x0, "beq">;
def BNE  : BranchCC<0x0, "bne">;
def BLE  : BranchCC<0x0, "ble">;
def BGT  : BranchCC<0x0, "bgt">;

def : PatBcc<SETEQ, BEQ>;
def : PatBcc<SETNE, BNE>;
def : PatBcc<SETLE, BLE>;
def : PatBcc<SETGT, BGT>;
//def : PatBcc<SETULE, BLEU>;
//def : PatBcc<SETUGT, BGTU>;

let hasSideEffects = 0, mayLoad = 0, mayStore = 0, isBarrier = 1, isBranch = 1, isTerminator = 1 in
def B : USimInst<0x0, (outs), (ins btarget16:$imm16),
                   "b\t $imm16",
                   [(br bb:$imm16)]>;

// ret TODO: pseudo
let isReturn = 1, isTerminator = 1, isBarrier = 1  in
def RET : USimInst<0x0, (outs), (ins), "br\t[%r1]", [(usim_ret)]> {
  let r1 = 1;
}

// movli
let isAsCheapAsAMove = 1, isCodeGenOnly = 1, isReMaterializable = 1 in
def MOVLI : USimInst<0x0, (outs GPR:$rd), (ins i32li16u:$imm),
                     "movli\t$rd, $imm",
                     [(set GPR:$rd, i32li16u:$imm)]>;

// movhi
let isAsCheapAsAMove = 1, isCodeGenOnly = 1, isReMaterializable = 1 in
def MOVHI : USimInst<0x0, (outs GPR:$rd), (ins i32hi16:$imm),
                     "movhi\t$rd, $imm",
                     [(set GPR:$rd, i32hi16:$imm)]>;

// TODO: movi pseudo

// mov TODO: pseudo
let isAsCheapAsAMove = 1, isCodeGenOnly = 1, isReMaterializable = 1 in
def MOV : USimInst<0x0, (outs GPR:$rd), (ins GPR:$rs1),
                   "mov\t$rd, $rs1",
                   // [(set GPR:$rd, GPR:$rs1)]>; // kills isel
                   []>;

// nop TODO: pseudo
def NOP   : USimInst<0x0, (outs), (ins), "nop", []>;

// sti, but not SubtargetInfo!
let hasSideEffects = 0, mayLoad = 0, mayStore = 1 in
def STI_: USimInst<0x0, (outs), (ins GPR:$rs2, GPR:$rs1, simm16:$imm),
              "sti\t$rs2, ${imm}(${rs1})",
              []>;
def : Pat<(store (i32 GPR:$rs2), BaseAddr:$rs1),
        (STI_ GPR:$rs2, BaseAddr:$rs1, 0)>;
def : Pat<(store (i32 GPR:$rs2), (add BaseAddr:$rs1, simm16:$imm)),
        (STI_ GPR:$rs2, BaseAddr:$rs1, simm16:$imm)>;
def : Pat<(store (i32 GPR:$rs2), (IsOrAdd AddrFI:$rs1, simm16:$imm)),
        (STI_ GPR:$rs2, AddrFI:$rs1, simm16:$imm)>;

// ldi
let hasSideEffects = 0, mayLoad = 1, mayStore = 0 in
def LDI: USimInst<0x0, (outs GPR:$rd), (ins GPR:$rs1, simm16:$imm),
              "ldi\t$rd, ${imm}(${rs1})",
              []>;
def : Pat<(i32 (load BaseAddr:$rs1)), (LDI BaseAddr:$rs1, 0)>;
def : Pat<(i32 (load (add BaseAddr:$rs1, simm16:$imm))),
          (LDI BaseAddr:$rs1, simm16:$imm)>;
def : Pat<(i32 (load (IsOrAdd AddrFI:$rs1, simm16:$imm))),
          (LDI AddrFI:$rs1, simm16:$imm)>;

// add
let isAsCheapAsAMove = 1 in
def ADD : USimInst<0x0, (outs GPR:$rd), (ins GPR:$rs2, GPR:$rs1),
                   "add\t$rd, $rs1, $rs2",
                   [(set GPR:$rd, (add GPR:$rs1, GPR:$rs2))]>;
def : PatGprGpr<add, ADD>;

// addi
let isAsCheapAsAMove = 1 in
def ADDI : USimInst<0x0, (outs GPR:$rd), (ins GPR:$rs1, simm16:$imm),
                    "addi\t$rd, $rs1, $imm",
                    [(set GPR:$rd, (add GPR:$rs1, simm16:$imm))]>;
def : PatGprImm<add, ADDI, simm16>;

// ori
let isAsCheapAsAMove = 1 in
def ORI : USimInst<0x0, (outs GPR:$rd), (ins GPR:$rs1, uimm16:$imm),
                   "ori\t$rd, $rs1, $imm",
                   [(set GPR:$rd, (or GPR:$rs1, uimm16:$imm))]>;
def : PatGprImm<add, ADDI, uimm16>;

//===----------------------------------------------------------------------===//
// USim Other patterns
//===----------------------------------------------------------------------===//

// u16 imm
def : Pat<(i32 i32li16u:$imm), (MOVLI imm:$imm)>;
// i32 imm
def : Pat<(i32 imm:$imm), (ORI (MOVHI (HI16 imm:$imm)), (LI16 imm:$imm))>;
