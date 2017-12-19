#ifndef _ST_TPC_CL_H
#define _ST_TPC_CL_H
#if !defined(__CINT__)
#include "DAQ_TPC/daq_tpc.h"
#else
class tpc_cl;
#endif
#include "TTable.h"
#include "Ttypes.h"
class St_tpc_cl : public TTable {
 public:
  ClassDefTable(St_tpc_cl,tpc_cl)
  ClassDef(St_tpc_cl,1) // legacy TPC clusters
};
#endif

