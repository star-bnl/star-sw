#include <stdio.h>
#include <string.h>
#include <arpa/inet.h>
#include <rtsLog.h>
#include <rtsSystems.h>


//#include <evpSupport.h>

//#ifndef TRG_VERSION
#define TRG_VERSION 0x32
//#endif

#include <daqFormats.h>
#include <rts.h>  // for the sake of swap32 entry

#include "daq_trg.h"

extern int trgReader32(char *trgd, int bytes, int swap, struct trg_t *trg);

// 2007, read the pointer banks then delagate to version 0x32...

int trgReader10(char *buff, struct trg_t *trg_t)
{
  TRGD *trgd = (TRGD *)buff;
  
  int swap = 0;
  if(trgd->bh.byte_order != DAQ_RAW_FORMAT_ORDER) swap=1;

  buff += sizeof(bankHeader);  // skip TRGD bank header!

  TrgTowerTrnfer *trg = (TrgTowerTrnfer *)buff;

  int byteCount_Version = swap32(trg->byteCount_Version);
  
//--  int byteCount = byteCount_Version >> 8;
  int version = byteCount_Version & 0xff;
  
  if(version != 0x10) {
    LOG(ERR, "Incorrect trigger header version 0x%x rather than 0x10", version);
    return -1;
  }

  // offset to legacy trg banks...
  int evtdesc_off = swap32(trg->OffsetBlock[TRG_INDEX].offset);
  LOG(DBG, "evtdesc_off = %d sizeof(TrgTowerTrnfer-4)=%d",evtdesc_off,sizeof(TrgTowerTrnfer)-4);

  if(evtdesc_off == 0) {
    LOG(ERR, "No trigger data available, offset = 0");
    return -1;
  }

  EvtDescData *evtdesc = (EvtDescData *)(buff + evtdesc_off); 
//  u_int *x = (u_int *)evtdesc;

  int version2 = evtdesc->TrgDataFmtVer;
  
  if(version2 != 0x32) {
    LOG(ERR, "Incorrect trigger data version 0x%x rather than 0x32  (0x%x)", version2, *evtdesc);
    return -1;
  }
  
  return trgReader32((char *)evtdesc, swap32(trg->OffsetBlock[TRG_INDEX].length), swap, trg_t);
}
