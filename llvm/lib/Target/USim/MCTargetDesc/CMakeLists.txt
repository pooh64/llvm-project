add_llvm_component_library(LLVMUSimDesc
  USimInstPrinter.cpp
  USimMCAsmInfo.cpp
  USimMCTargetDesc.cpp

  LINK_COMPONENTS
  MC
  USimInfo
  Support

  ADD_TO_COMPONENT
  USim
  )
