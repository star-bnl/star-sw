#define PP printf(
#include <stdio.h>
#include <unistd.h>
#include <assert.h>
#include <string.h>
#include <arpa/inet.h>

#include "daqFormats.h"
#include "rtsSystems.h"

#include "evpSupport.h"
#include "scReader.h"

struct sc_t sc;

// reads the Triggers event descriptor from DATAP directly...
int scReader(char *m) 
{
  int len ;
  u_int off ;
  struct SCD *scd ;
  struct DATAP *datap ;
  struct DATAPX *datapx ;
  

  if(m == NULL) return EVP_DATA_ERR ;
  
  datap = (struct DATAP *)m ;


  len = l2h32(datap->det[EXT_ID].len) ;
  if(len == 0) return EVP_NO_DET;
  assert(len>0&&len<99999); 

  off = l2h32(datap->det[EXT_ID].off);
  if(off == 0) return EVP_NO_DET;

  datapx = (struct DATAPX *)((u_int *)m+off);

  // verify bank
  if(checkBank(datapx->bh.bank_type,CHAR_DATAPX) < 0) assert (0);

  len = l2h32(datapx->det[SC_ID-10].len);
  if(datapx->bh.byte_order != 0x04030201) len = swap32(len);
  if(len == 0) return EVP_NO_DET ;
  len *= 4;

  off = l2h32(datapx->det[SC_ID-10].off);
  if(datapx->bh.byte_order != 0x04030201) off = swap32(off);
  if(off == 0) return EVP_NO_DET;

  //LOG(DBG,"SC raw len %d (0x%x), off %d(0x%x)",len,len,off,off,0) ;

  scd = (struct SCD *)(((u_int *)datapx)+off) ;
  if(checkBank((char *)scd,"SCD") < 0) assert(0);

  if(scd->bh.token == 0) {
    //LOG(DBG,"Token 0 - skipping...",0,0,0,0,0) ;
    return EVP_NO_DET ;
  }
  
  int sz = l2h32(scd->bh.length) * 4;
  
  if((len != sz) ||
     ((u_int)len > sizeof(SCD))) {
    //LOG(ERR, "SCD Sizes not consistent: datap=%d, bh=%d SCD=%d",
    //	len, sz, sizeof(scd),0,0);
    
    return EVP_DATA_ERR;
  }
  

  // copy scd data into sc
  sc.time = l2h32(scd->time);
  int mag = l2h32(scd->mag_field);
  memcpy(&sc.mag_field, &mag, 4);
  for(int i=0;i<16;i++) {
    sc.rich_scalers[i] = l2h32(scd->rich_scalers[i]);
  }

  sc.timelag = l2h32(datap->time) - sc.time;

  int alag = sc.timelag > 0 ? sc.timelag : -sc.timelag;

  if(alag > 5) sc.valid = 0;
  else sc.valid = 1;

  return len;
}

