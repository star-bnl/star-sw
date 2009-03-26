#include <stdio.h>
#include <string.h>
#include <arpa/inet.h>

#include <rtsLog.h>


#include <daqFormats.h>
#include <rtsSystems.h>
#include <rts.h>

#include "daq_sc.h"

int sc_reader(char *m, struct sc_t *sc, u_int driver)
{
  int swapdatap = 0;
  int swapscd = 0;

  if(m == NULL) return 0 ;
  
  DATAP *datap = (struct DATAP *)m ;
  int len, off ;


  // zap all first  memset(&sc,0,sizeof(sc)) ;
  
  datap = (struct DATAP *) m ;
  if(datap->bh.byte_order != DAQ_RAW_FORMAT_ORDER) 
    swapdatap = 1;


  len = qswap32(swapdatap, datap->det[EXT_ID].len);
  if(len == 0) return 0;

  off = qswap32(swapdatap, datap->det[EXT_ID].off);
  if(off == 0) return 0;

  // printf("Have datapx\n");
  DATAPX *datapx = (struct DATAPX *)(((u_int *)m)+off);
  if(checkBank(datapx->bh.bank_type,"DATAPX") < 0) {
    return 0;
  }


  int swapdatapx=0;
  if(datapx->bh.byte_order != DAQ_RAW_FORMAT_ORDER) 
    swapdatapx = 1;

  len = qswap32(swapdatapx, datapx->det[SC_ID-10].len);
  if(len == 0) return 0 ;
  len *= 4 ;

  off = qswap32(swapdatapx, datapx->det[SC_ID-10].off);
  if(off == 0) return 0;

  LOG(DBG,"SC raw len %d (0x%x), off %d(0x%x)",len,len,off,off,0) ;

  SCD *scd = (struct SCD *)(((u_int *)datapx)+off) ;
  if(checkBank(scd->bh.bank_type,"SCD") < 0) {	// wrong bank!
    return 0 ;				
  }

  if(scd->bh.byte_order != DAQ_RAW_FORMAT_ORDER)
    swapscd = 1;

  
  int sz = qswap32(swapscd, scd->bh.length);
  sz*=4;
  
  if((len != sz) ||
     ((u_int)len > sizeof(SCD))) {
    LOG(ERR, "SCD Sizes not consistent: datap=%d, bh=%d SCD=%d",
	len, sz, sizeof(scd),0,0);
    
    return 0;
  }

  LOG(DBG,"Need to swap SCD? %d",swapscd) ;  

  // copy scd data into sc
  sc->time = qswap32(swapscd, scd->time);
  int mag = qswap32(swapscd, scd->mag_field);
  memcpy(&sc->mag_field, &mag, 4);
  for(int i=0;i<16;i++) {
    sc->rich_scalers[i] = qswap32(swapscd, scd->rich_scalers[i]);
  }

  sc->timelag = qswap32(swapdatap, datap->time) - sc->time;

  int alag = sc->timelag > 0 ? sc->timelag : -sc->timelag;

  if(alag > 5) sc->valid = 0;
  else sc->valid = 1;

  return len;
}
