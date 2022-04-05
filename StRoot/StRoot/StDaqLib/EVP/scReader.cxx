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
#define SWAP32(bk,x) ((bk->bh.byte_order==0x4030201)?(bk->x):swap32(bk->x))

using namespace OLDEVP;
namespace OLDEVP {
struct sc_t sc;
};

// reads the Triggers event descriptor from DATAP directly...
int OLDEVP::scReader(char *m) 
{
  int len ;
  u_int off ;
  struct SCD *scd ;
  struct DATAP *datap ;
  struct DATAPX *datapx ;
  

  if(m == NULL) return EVP_DATA_ERR ;
  
  datap = (struct DATAP *)m ;


  len = SWAP32(datap,det[EXT_ID].len) ;
  if(len == 0) return EVP_NO_DET;
  assert(len>0&&len<99999); 

  off = SWAP32(datap,det[EXT_ID].off);
  if(off == 0) return EVP_NO_DET;

  datapx = (struct DATAPX *)((u_int *)m+off);

  // verify bank
  assert(checkBank(datapx->bh.bank_type,CHAR_DATAPX) == 0);

  len = SWAP32(datapx,det[SC_ID-10].len);
  if(len == 0) return EVP_NO_DET ;

  off = SWAP32(datapx,det[SC_ID-10].off);
  if(off == 0) return EVP_NO_DET;

  //LOG(DBG,"SC raw len %d (0x%x), off %d(0x%x)",len,len,off,off,0) ;

  scd = (struct SCD *)(((u_int *)datapx)+off) ;
  if(checkBank((char *)scd,"SCD") < 0) assert(0);

  if(scd->bh.token == 0) {
    //LOG(DBG,"Token 0 - skipping...",0,0,0,0,0) ;
    return EVP_NO_DET ;
  }
  int sz = SWAP32(scd,bh.length);
  
  if((len != sz) ||
     ((u_int)len > sizeof(SCD))) {
    //LOG(ERR, "SCD Sizes not consistent: datap=%d, bh=%d SCD=%d",
    //	len, sz, sizeof(scd),0,0);
    
    return EVP_DATA_ERR;
  }
  

  // copy scd data into sc
  sc.time = SWAP32(scd,time);
  int mag = SWAP32(scd,mag_field);
  sc.mag_field= *((float*)&mag);
  for(int i=0;i<16;i++) {
    sc.rich_scalers[i] = SWAP32(scd,rich_scalers[i]);
  }

  sc.timelag = SWAP32(datap,time) - sc.time;

  int alag = sc.timelag > 0 ? sc.timelag : -sc.timelag;

  if(alag > 5) sc.valid = 0;
  else sc.valid = 1;

  return len;
}

