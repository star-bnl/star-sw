/*********************************************************************
 * $Id: EztTrigBlob.cxx,v 1.2 2004/11/29 15:55:07 mvl Exp $
 *********************************************************************
 * container for FULL STAR trigger data, requires Akio's calss to unpack it
 */

#include <TArrayC.h>
#include <TObjArray.h>

#include "EztTrigBlob.h"
ClassImp(EztTrigBlob)


//--------------------------------------------------
//
//--------------------------------------------------
EztTrigBlob ::  EztTrigBlob() {
  trgd=new TArrayC ;
  trgid=new TArrayC ;
  clear();
}

//--------------------------------------------------
//
//--------------------------------------------------
EztTrigBlob ::  ~EztTrigBlob() { 
  delete trgd;
  delete trgid;
}


//--------------------------------------------------
//
//--------------------------------------------------
void EztTrigBlob :: clear() {
  trgd->Reset();
  trgid->Reset();
  version=0;
}


//--------------------------------------------------
//--------------------------------------------------
void EztTrigBlob :: print(int k, FILE *fd) const{
  fprintf(fd,"EztTrigBlob::print() c-struct version=%d",version);
  fprintf(fd,"  size of banks: trgd=%d  trgid=%d\n",trgd->GetSize(),trgid->GetSize());
  if(k<=0) return;
  Char_t *d0=trgd->GetArray();
  unsigned char *d=(unsigned char *)d0;
  printf("Hex Dump of trgd\n");
  int i;
  for(i=0;i<trgd->GetSize();i++) {
    if((i%8)==0) printf(" ");
    if((i%24)==0) printf("\n");
    printf("%02x ",d[i]);
  }

}


