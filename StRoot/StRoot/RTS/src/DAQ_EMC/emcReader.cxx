#include <sys/types.h>
#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>
#include <DAQ_READER/daq_det.h>
#include "emcReader.h"

// DAQ_LEGACY_DEF(emc);
//  We can not use the predefined macro 
//  because very special naming convention is used for "emc" detector
//  One has to create the code "by hand"
//  see: http://docs.google.com/View?docid=dgv8pf9t_60dwhg3zd4
//  Table 1.

#define DAQ_LEGACY_EMC_DEF(xxx)         \
struct _NAME2_(xxx,_t) _NAME1_(xxx);\
                                    \
int _NAME2_(xxx,Reader)(char *m)  {  \
  if(!m) return -1;                 \
  daqReader *rrr = (daqReader *)m;  \
  daq_dta *dd= rrr->det("emc_pseudo")->get("legacy"); \
  int size = 0;                     \
  if (dd && (size = dd->iterate())) { \
  memcpy(&_NAME1_(xxx),dd->Void,dd->get_size_t());} \
  return (dd && size) ? dd->ncontent: 0; }
      
DAQ_LEGACY_EMC_DEF(emc);
