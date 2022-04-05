/*********************************************************************
 * $Id: EztFpdBlob.cxx,v 1.1 2004/11/29 15:55:55 mvl Exp $
 *********************************************************************
 * container for FPD raw data
 */

#include <TArrayS.h>
#include <TObjArray.h>

#include "EztFpdBlob.h"
ClassImp(EztFpdBlob)


//--------------------------------------------------
//
//--------------------------------------------------
EztFpdBlob ::  EztFpdBlob() {
  smd=new TArrayS ;
  clear();
}

//--------------------------------------------------
//
//--------------------------------------------------
EztFpdBlob ::  ~EztFpdBlob() { 
  delete smd;
}


//--------------------------------------------------
//
//--------------------------------------------------
void EztFpdBlob :: clear() {
  smd->Reset();
}


//--------------------------------------------------
//--------------------------------------------------
void EztFpdBlob :: print(int k, FILE *fd) const{
  fprintf(fd,"EztFpdBlob:: print() ");
  fprintf(fd,"  EztFpd size of bank smd=%d\n",smd->GetSize());
  if(k<=0) return;
  unsigned short *d=(unsigned short *)smd->GetArray();
  printf(" decymal dump of FPD smd:");
  int i;
  for(i=0;i<smd->GetSize();i++) {
    if((i%8)==0) printf(" ");
    if((i%24)==0) printf("\n");
    printf("%4d ",d[i]);
  }
  printf("\n");
}


