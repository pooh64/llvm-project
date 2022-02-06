//===----------------------------------------------------------------------===//
// This describes the calling conventions for USim architecture.
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// USim Return Value Calling Convention
//===----------------------------------------------------------------------===//
def RetCC_USim : CallingConv<[
  // i32 are returned in registers R7-R10
  CCIfType<[i32, i64], CCAssignToReg<[R7, R8, R9, R10]>>,

  // Integer values get stored in stack slots that are 4 bytes in
  // size and 4-byte aligned.
  CCIfType<[i64], CCAssignToStack<8, 4>>,
  CCIfType<[i32], CCAssignToStack<4, 4>>
]>;

//===----------------------------------------------------------------------===//
// USim Argument Calling Conventions
//===----------------------------------------------------------------------===//
def CC_USim : CallingConv<[
  // Promote i8/i16 arguments to i32.
  CCIfType<[i8, i16], CCPromoteToType<i32>>,

  // The first 5 integer arguments are passed in integer registers.
  CCIfType<[i32, i64], CCAssignToReg<[R7, R8, R9, R10, R11]>>,

  // Integer values get stored in stack slots that are 4 bytes in
  // size and 4-byte aligned.
  CCIfType<[i64], CCAssignToStack<8, 4>>,
  CCIfType<[i32], CCAssignToStack<4, 4>>
]>;

def CSR_USim : CalleeSavedRegs<(add R0, R3, R12, R13, R14, R15)>;
