//===----------------------------------------------------------------------===//
//
// This file contains the USim implementation of the TargetInstrInfo class.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// USim profiles and nodes
//===----------------------------------------------------------------------===//

def SDT_USimCall         : SDTypeProfile<0, -1, [SDTCisVT<0, i32>]>;
def SDT_USimCallSeqStart : SDCallSeqStart<[ SDTCisVT<0, i32>,
                                            SDTCisVT<1, i32> ]>;
def SDT_USimCallSeqEnd   : SDCallSeqEnd<[ SDTCisVT<0, i32>,
                                          SDTCisVT<1, i32> ]>;

def SDT_USimBrCC : SDTypeProfile<0, 4, [ SDTCisSameAs<0, 1>,
                                         SDTCisVT<2, OtherVT>,
                                         SDTCisVT<3, OtherVT>]>;
def SDT_USimSelectCC     : SDTypeProfile<1, 5, [SDTCisSameAs<1, 2>,
                                             SDTCisVT<3, OtherVT>,
                                             SDTCisSameAs<0, 4>,
                                             SDTCisSameAs<4, 5>]>;


// These are target-independent nodes, but have target-specific formats.
def callseq_start : SDNode<"ISD::CALLSEQ_START", SDT_USimCallSeqStart,
                           [SDNPHasChain, SDNPOutGlue]>;
def callseq_end   : SDNode<"ISD::CALLSEQ_END",   SDT_USimCallSeqEnd,
                           [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue]>;

def usim_ret      : SDNode<"USimISD::RET", SDTNone,
                            [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;

def usim_call     : SDNode<"USimISD::CALL", SDT_USimCall,
                            [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue,
                             SDNPVariadic]>;

def usim_brcc     : SDNode<"USimISD::BR_CC", SDT_USimBrCC,
                            [SDNPHasChain]>;

def usim_selectcc     : SDNode<"USimISD::SELECT_CC", SDT_USimSelectCC,
                               [SDNPInGlue]>;

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

def imm32 : Operand<i32>;

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

// TODO: sym methods
def ctarget16 : Operand<i32> {
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
    return ((uint64_t)(N->getZExtValue() & 0xFFFFUL) == N->getZExtValue());}], LI16> {
  // let ParserMatchClass = LiImm16AsmOperand;
}

def i32li16s : Operand<i32>, PatLeaf<(i32 imm), [{
    return ((int64_t)(N->getSExtValue() & 0xFFFFUL) == N->getSExtValue());}], LI16> {
  // let ParserMatchClass = LiImm16AsmOperand;
}

// def HiImm16AsmOperand : AsmOperandClass { let Name = "HiImm16"; }
def i32hi16 : Operand<i32>, PatLeaf<(i32 imm), [{
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
              !strconcat(opcodestr, "\t$rs1, $rs2, $imm16"), []>;

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in
class ArithmRR<bits<8> op, string opcodestr>
    : USimInst<op, (outs GPR:$rd), (ins GPR:$rs1, GPR:$rs2),
              !strconcat(opcodestr, "\t$rd, $rs1, $rs2"), []>;

let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in
class ArithmRI<bits<8> op, string opcodestr>
    : USimInst<op, (outs GPR:$rd), (ins GPR:$rs1, simm16:$imm),
              !strconcat(opcodestr, "\t$rd, $rs1, $imm"), []>;
/*
let hasSideEffects = 0, mayLoad = 0, mayStore = 0 in
class FPArithmR<bits<8> op, RegisterClass rdt, RegisterClass rs1t, string opcodestr>
    : USimInst<op, (outs rdt:$rd), (ins rs1t:$rs1),
              !strconcat(opcodestr, "\t$rd, $rs1"), []>;
*/
//===----------------------------------------------------------------------===//
// Instruction definition
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// USim Instructions
//===----------------------------------------------------------------------===//

// TODO: check B* flags
let hasSideEffects = 0, mayLoad = 0, mayStore = 0, isBarrier = 1, isBranch = 1, isTerminator = 1 in
def B : USimInst<0x0, (outs), (ins btarget16:$imm16), "b\t$imm16",
                 [(br bb:$imm16)]>;

let hasSideEffects = 0, mayLoad = 0, mayStore = 0, isCall = 1, isBranch = 1, isTerminator = 1 in
def BR : USimInst<0x0, (outs), (ins GPR:$rs1), "br\t$rs1",
                  [(brind GPR:$rs1)]>;

let hasSideEffects = 0, mayLoad = 0, mayStore = 0, isCall = 1, isBranch = 1, isTerminator = 1 in
def BL : USimInst<0x0, (outs GPR:$rd), (ins ctarget16:$imm16), "bl\t$rd, $imm16",
                  []>;

let hasSideEffects = 0, mayLoad = 0, mayStore = 0, isCall = 1, isBranch = 1, isTerminator = 1 in
def BLR : USimInst<0x0, (outs GPR:$rd), (ins GPR:$rs1),
                  "blr\t$rd, $rs1", []>;

let isBarrier = 1, isReturn = 1, isTerminator = 1 in
def PseudoRET : USimPseudoInst<(outs), (ins), "# PseudoRET",
                [(usim_ret)]>,
                PseudoInstExpansion<(BR R1)>;

let isCall = 1, Defs = [R1] in
def PseudoCALL : USimPseudoInst<(outs), (ins ctarget16:$func), "# PseudoCALL",
                 [(usim_call tglobaladdr:$func)]>,
                 PseudoInstExpansion<(BL R1, ctarget16:$func)>;

let isCall = 1, Defs = [R1] in
def PseudoCALLInd : USimPseudoInst<(outs), (ins GPR:$rs1), "# PseudoCALLInd",
                    [(usim_call GPR:$rs1)]>,
                    PseudoInstExpansion<(BLR R1, GPR:$rs1)>;

def BEQ  : BranchCC<0x0, "beq">;
def BNE  : BranchCC<0x0, "bne">;
def BLE  : BranchCC<0x0, "ble">;
def BGT  : BranchCC<0x0, "bgt">;
def BLEU  : BranchCC<0x0, "bleu">;
def BGTU  : BranchCC<0x0, "bgtu">;

def : PatBcc<SETEQ, BEQ>;
def : PatBcc<SETNE, BNE>;
def : PatBcc<SETLE, BLE>;
def : PatBcc<SETGT, BGT>;
def : PatBcc<SETULE, BLEU>;
def : PatBcc<SETUGT, BGTU>;

def IntCCtoUSimCC : SDNodeXForm<usim_selectcc, [{
  ISD::CondCode CC = cast<CondCodeSDNode>(N->getOperand(2))->get();
  USimCC::CondCode BrCC = getUSimCCForIntCC(CC);
  return CurDAG->getTargetConstant(BrCC, SDLoc(N), MVT::i32);
}]>;

def usim_selectcc_frag : PatFrag<(ops node:$lhs, node:$rhs, node:$cc,
                                       node:$truev, node:$falsev),
                                  (usim_selectcc node:$lhs, node:$rhs,
                                                  node:$cc, node:$truev,
                                                  node:$falsev), [{}],
                                  IntCCtoUSimCC>;

let usesCustomInserter = 1 in
def Select_CC_GPR
    : USimPseudoInst<
      (outs GPR:$rd), (ins GPR:$lhs, GPR:$rhs, imm32:$imm, GPR:$st, GPR:$sf),
      "# PseudoSelect",
      [(set i32:$rd, (usim_selectcc_frag:$imm GPR:$lhs, GPR:$rhs, cond, GPR:$st, GPR:$sf))]>;

// movli
let isAsCheapAsAMove = 1, isCodeGenOnly = 1, isReMaterializable = 1 in
def MOVLIu : USimInst<0x0, (outs GPR:$rd), (ins i32li16u:$imm),
                     "movliu\t$rd, $imm",
                     [(set GPR:$rd, i32li16u:$imm)]>;

let isAsCheapAsAMove = 1, isCodeGenOnly = 1, isReMaterializable = 1 in
def MOVLIs : USimInst<0x0, (outs GPR:$rd), (ins i32li16s:$imm),
                     "movlis\t$rd, $imm",
                     [(set GPR:$rd, i32li16s:$imm)]>;

// movhi
let isAsCheapAsAMove = 1, isCodeGenOnly = 1, isReMaterializable = 1 in
def MOVHI : USimInst<0x0, (outs GPR:$rd), (ins i32hi16:$imm),
                     "movhi\t$rd, $imm",
                     [(set GPR:$rd, i32hi16:$imm)]>;

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

def ADD  : ArithmRR<0x0, "add">;  def : PatGprGpr<add,  ADD>;
def SUB  : ArithmRR<0x0, "sub">;  def : PatGprGpr<sub,  SUB>;
def MUL  : ArithmRR<0x0, "mul">;  def : PatGprGpr<mul,  MUL>;
def DIVS : ArithmRR<0x0, "divs">; def : PatGprGpr<sdiv, DIVS>;
def DIVU : ArithmRR<0x0, "divu">; def : PatGprGpr<udiv, DIVU>;
def REM  : ArithmRR<0x0, "rems">; def : PatGprGpr<srem, REM>;
def REMU : ArithmRR<0x0, "remu">; def : PatGprGpr<urem, REMU>;
def OR   : ArithmRR<0x0, "or">;   def : PatGprGpr<or,   OR>;
def AND  : ArithmRR<0x0, "and">;  def : PatGprGpr<and,  AND>;
def XOR  : ArithmRR<0x0, "xor">;  def : PatGprGpr<xor,  XOR>;

def ADDI  : ArithmRI<0x0, "addi">;  def : PatGprImm<add, ADDI, simm16>;
def MULI  : ArithmRI<0x0, "muli">;  def : PatGprImm<mul, MULI, simm16>;
def DIVIS : ArithmRI<0x0, "divis">; def : PatGprImm<sdiv, DIVIS, simm16>;
def DIVIU : ArithmRI<0x0, "diviu">; def : PatGprImm<udiv, DIVIU, simm16>;
def REMIS : ArithmRI<0x0, "remis">; def : PatGprImm<srem, REMIS, simm16>;
def REMIU : ArithmRI<0x0, "remiu">; def : PatGprImm<urem, REMIU, simm16>;
def ORI   : ArithmRI<0x0, "ori">;   def : PatGprImm<or, ORI, simm16>;
def ANDI  : ArithmRI<0x0, "andi">;  def : PatGprImm<and, ANDI, simm16>;
def XORI  : ArithmRI<0x0, "xori">;  def : PatGprImm<xor, XORI, simm16>;

def SHL  : ArithmRR<0x0, "shl">;   def : PatGprGpr<shl, SHL>;
def SHR  : ArithmRR<0x0, "shr">;   def : PatGprGpr<srl, SHR>;
def SHRA : ArithmRR<0x0, "shra">;  def : PatGprGpr<sra, SHRA>;

/*
def FCVTWS : FPArithmR<0x0, GPR, FPR, "fcvtws">;
def : Pat<(any_fp_to_sint (f32 FPR:$rs1)), (FCVTWS $rs1)>;

def FCVTSW : FPArithmR<0x0, FPR, GPR, "fcvtsw">;
def : Pat<(any_sint_to_fp (i32 GPR:$rs1)), (FCVTSW $rs1)>;
*/
//===----------------------------------------------------------------------===//
// USim Other patterns
//===----------------------------------------------------------------------===//

// i16 imm
//def : Pat<(i32 i32li16u:$imm), (MOVLIu imm:$imm)>;
//def : Pat<(i32 i32li16s:$imm), (MOVLIs imm:$imm)>;
// i32 imm
//def : Pat<(i32 imm:$imm), (ORI (MOVHI (HI16 imm:$imm)), (LI16 imm:$imm))>;

def : Pat<(add (i32 AddrFI:$rs), simm16:$imm16),
          (ADDI (i32 AddrFI:$rs), simm16:$imm16)>;
def : Pat<(IsOrAdd (i32 AddrFI:$rs), simm16:$imm16),
          (ADDI (i32 AddrFI:$rs), simm16:$imm16)>;

