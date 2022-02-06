add_llvm_component_group(USim)

set(LLVM_TARGET_DEFINITIONS USim.td)

# Generate USimGenRegisterInfo.inc and USimGenInstrInfo.inc which included by
#  your hand code C++ files.
# USimGenRegisterInfo.inc came from USimRegisterInfo.td, USimGenInstrInfo.inc
#  came from USimInstrInfo.td.
tablegen(LLVM USimGenRegisterInfo.inc -gen-register-info)
tablegen(LLVM USimGenInstrInfo.inc -gen-instr-info)
tablegen(LLVM USimGenCallingConv.inc -gen-callingconv)
tablegen(LLVM USimGenMCPseudoLowering.inc -gen-pseudo-lowering)
tablegen(LLVM USimGenSubtargetInfo.inc -gen-subtarget)
tablegen(LLVM USimGenAsmWriter.inc -gen-asm-writer)
tablegen(LLVM USimGenDAGISel.inc -gen-dag-isel)

#tablegen(LLVM USimGenDisassemblerTables.inc -gen-disassembler)

# USimCommonTableGen must be defined
add_public_tablegen_target(USimCommonTableGen)

# USimCodeGen should match with LLVMBuild.txt USimCodeGen
add_llvm_target(USimCodeGen
  USimAsmPrinter.cpp
  USimFrameLowering.cpp
  USimInstrInfo.cpp
  USimISelDagToDag.cpp
  USimISelLowering.cpp
  USimMachineFunctionInfo.cpp
  USimMCInstLower.cpp
  USimRegisterInfo.cpp
  USimSubtarget.cpp
  USimTargetMachine.cpp

  LINK_COMPONENTS
  Analysis
  AsmPrinter
  CodeGen
  Core
  MC
  USimDesc
  USimInfo
  SelectionDAG
  Support
  Target
  GlobalISel

  ADD_TO_COMPONENT
  USim
)

add_subdirectory(TargetInfo)
add_subdirectory(MCTargetDesc)
