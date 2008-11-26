#ifndef _TPC_READER_H_
#define _TPC_READER_H_

#ifndef _EMC_OLD_READER_H_
#define _EMC_OLD_READER_H_

#include "daq_tpc.h"
DAQ_LEGACY_DECL(tpc);
extern int tpcReader(char *m, int sector);

#endif
