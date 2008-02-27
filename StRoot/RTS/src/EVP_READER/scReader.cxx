#include <stdio.h>
#include <string.h>
#include <arpa/inet.h>

#include <rtsLog.h>


#include <daqFormats.h>
#include <rtsSystems.h>

#include <evpReader.hh>
#include <evpSupport.h>
#include <scReader.h>



struct sc sc;


int DAQscReader(char *m);

int scReader(char *m)
{
  static int oldrun=-1;
  static int oldbytes=-1;
  
  if(m == NULL) return EVP_DATA_ERR ;	// error
  
  evpReader *rrr = (evpReader *)m;
  if(!rrr->mem) {
    LOG(DBG, "No datap:   mem=0x%x sfs=0x%x",rrr->mem,rrr->sfs);
    return EVP_NO_DET;
  }

  if(oldrun == (int)rrr->event_number) {
    return oldbytes;
  };

  oldrun = rrr->event_number;
  oldbytes = DAQscReader(rrr->mem);

  return oldbytes;
}

// reads the Triggers event descriptor from DATAP directly...
int DAQscReader(char *m) 
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
  //printf("len1=%d\n",len);
  if(len == 0) return EVP_NO_DET;
  off = qswap32(swapdatap, datap->det[EXT_ID].off);
  //printf("off1=%d\n",off);
  if(off == 0) return EVP_NO_DET;

  // printf("Have datapx\n");
  DATAPX *datapx = (struct DATAPX *)(((u_int *)m)+off);
  if(checkBank(datapx->bh.bank_type,"DATAPX") < 0) {
    printf("err?\n");
    return EVP_DATA_ERR;
  }

  //printf("ok\n");
  int swapdatapx=0;
  if(datapx->bh.byte_order != DAQ_RAW_FORMAT_ORDER) 
    swapdatapx = 1;

  len = qswap32(swapdatapx, datapx->det[SC_ID-10].len);
  //printf("len4 = %d\n",len);
  if(len == 0) return EVP_NO_DET ;
  len *= 4 ;

  off = qswap32(swapdatapx, datapx->det[SC_ID-10].off);
  //printf("off4 = %d\n",off); 
  if(off == 0) return EVP_NO_DET;

  LOG(DBG,"SC raw len %d (0x%x), off %d(0x%x)",len,len,off,off,0) ;

  SCD *scd = (struct SCD *)(((u_int *)datapx)+off) ;
  if(checkBank(scd->bh.bank_type,"SCD") < 0) {	// wrong bank!
    return EVP_DATA_ERR ;				
  }

  if(scd->bh.byte_order != DAQ_RAW_FORMAT_ORDER)
    swapscd = 1;

  if(scd->bh.token == 0) {
    LOG(DBG,"Token 0 - skipping...",0,0,0,0,0) ;
    return EVP_NO_DET ;
  }
  
  int sz = qswap32(swapscd, scd->bh.length);
  sz*=4;
  
  if((len != sz) ||
     ((u_int)len > sizeof(SCD))) {
    LOG(ERR, "SCD Sizes not consistent: datap=%d, bh=%d SCD=%d",
	len, sz, sizeof(scd),0,0);
    
    return EVP_DATA_ERR;
  }
  

  // copy scd data into sc
  sc.time = qswap32(swapscd, scd->time);
  int mag = qswap32(swapscd, scd->mag_field);
  memcpy(&sc.mag_field, &mag, 4);
  for(int i=0;i<16;i++) {
    sc.rich_scalers[i] = qswap32(swapscd, scd->rich_scalers[i]);
  }

  sc.timelag = qswap32(swapdatap, datap->time) - sc.time;

  int alag = sc.timelag > 0 ? sc.timelag : -sc.timelag;

  if(alag > 5) sc.valid = 0;
  else sc.valid = 1;

  return len;
}
