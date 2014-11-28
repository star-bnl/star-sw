#include <string.h>
#include <sys/types.h>
#include <assert.h>
#include "DAQ_READER/daqReader.h"
#include <DAQ_READER/daq_dta.h>
#include <DAQ_READER/daq_det.h>
#include "tpcReader.h"

struct tpc_t tpc;
                          
int tpcReader(char *m)  { assert(0); }

int tpcReader(char *m, int sector ) {
  if(!m) return -1;       
  daqReader *rrr = (daqReader *)m; 
  daq_dta *dd= rrr->det("tpx")->get("legacy",sector); 
  int size = 0;
  if (!dd) dd= rrr->det("tpc")->get("legacy",sector);                     
  if (dd && (size = dd->iterate())) {
     memcpy(&tpc,dd->Void,dd->get_size_t());
  }
  return dd ? dd->ncontent : 0; 
}
