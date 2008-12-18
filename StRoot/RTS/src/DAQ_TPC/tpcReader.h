#ifndef _TPC_READER_H_
#define _TPC_READER_H_

#include "daq_tpc.h"
// DAQ_LEGACY_DECL(tpc);
extern struct tpc_t tpc;
extern int tpcReader(char *mem);
extern int tpcReader(char *mem,int sector);

#endif
