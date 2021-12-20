add_llvm_component_library(LLVMUSimInfo
  USimTargetInfo.cpp

  LINK_COMPONENTS
  Support

  ADD_TO_COMPONENT
  USim
  )
