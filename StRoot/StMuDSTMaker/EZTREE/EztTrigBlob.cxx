/*********************************************************************
 * $Id: EztTrigBlob.cxx,v 1.1 2004/10/28 00:10:19 mvl Exp $
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
  unixTimeStamp=0;
}


//--------------------------------------------------
//--------------------------------------------------
void EztTrigBlob :: print(int k, FILE *fd) const{
  fprintf(fd,"EztTrigBlob:: print() ");
  fprintf(fd,"timeStamp=%d=%s" ,(int)unixTimeStamp,ctime((const time_t *)&unixTimeStamp));
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


