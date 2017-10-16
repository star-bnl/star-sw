#ifndef _ST_DAQ_SIM_CLD_H
#define _ST_DAQ_SIM_CLD_H
#if !defined(__CINT__) && !defined(__CLING__)
#include "DAQ_READER/daq_dta_structs.h"
#else
class daq_sim_cld;
#endif
#include "TTable.h"
#include "Ttypes.h"
class St_daq_sim_cld : public TTable {
 public:
  ClassDefTable(St_daq_sim_cld,daq_sim_cld)
  ClassDef(St_daq_sim_cld,1) // 
};
#endif
