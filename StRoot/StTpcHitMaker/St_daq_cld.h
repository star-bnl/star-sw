#ifndef _ST_DAQ_CLD_H
#define _ST_DAQ_CLD_H
#if !defined(__CINT__) && !defined(__CLING__)
#include "DAQ_READER/daq_dta_structs.h"
#else
class daq_cld;
#endif
#include "TTable.h"
#include "Ttypes.h"
class St_daq_cld : public TTable {
 public:
  ClassDefTable(St_daq_cld,daq_cld)
  ClassDef(St_daq_cld,1) // 
};
#endif
