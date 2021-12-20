//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
//  Declarations that describe the USim register file
//===----------------------------------------------------------------------===//

// For tablegen(... -gen-emitter)  in CMakeLists.txt
class USimReg<bits<16> Enc, string n> : Register<n> {
  let HWEncoding = Enc;
}

//===----------------------------------------------------------------------===//
//@Registers
//===----------------------------------------------------------------------===//
//@ All registers definition
let Namespace = "USim" in {
  //@ General Purpose Registers
  def R0    : USimReg<0,  "r0">,    DwarfRegNum<[0]>;
  def R1    : USimReg<1,  "r1">,    DwarfRegNum<[1]>; // ra
  def R2    : USimReg<2,  "r2">,    DwarfRegNum<[2]>; // sp
  def R3    : USimReg<3,  "r3">,    DwarfRegNum<[3]>;
  def R4    : USimReg<4,  "r4">,    DwarfRegNum<[4]>;
  def R5    : USimReg<5,  "r5">,    DwarfRegNum<[5]>;
  def R6    : USimReg<6,  "r6">,    DwarfRegNum<[6]>;
  def R7    : USimReg<7,  "r7">,    DwarfRegNum<[7]>;
  def R8    : USimReg<8,  "r8">,    DwarfRegNum<[8]>;
  def R9    : USimReg<9,  "r9">,    DwarfRegNum<[9]>;
  def R10   : USimReg<10, "r10">,   DwarfRegNum<[10]>;
  def R11   : USimReg<11, "r11">,   DwarfRegNum<[11]>;
  def R12   : USimReg<12, "r12">,   DwarfRegNum<[12]>;
  def R13   : USimReg<13, "r13">,   DwarfRegNum<[13]>;
  def R14   : USimReg<14, "r14">,   DwarfRegNum<[14]>;
  def R15   : USimReg<15, "r15">,   DwarfRegNum<[15]>;
}

//===----------------------------------------------------------------------===//
//@Register Classes
//===----------------------------------------------------------------------===//

def GPR : RegisterClass<"USim", [i32], 32, (add
  R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15)>;
