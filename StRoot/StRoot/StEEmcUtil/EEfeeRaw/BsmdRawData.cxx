#include "BsmdRawData.h"
 

#include <TArrayC.h>
#include <TArrayS.h>
#include <TObjArray.h>

ClassImp(BsmdRawData)


//--------------------------------------------------
//
//--------------------------------------------------
BsmdRawData ::  BsmdRawData() {
  used=new TArrayS ;
  caps=new TArrayC ;
  head=new TArrayS ;
  data=new TArrayS ;
  clear();
}

//--------------------------------------------------
//
//--------------------------------------------------
BsmdRawData ::  ~BsmdRawData() { 
  delete caps;
  delete used;
  delete head;
  delete data;
}


//--------------------------------------------------
//
//--------------------------------------------------
void BsmdRawData :: clear() {
  caps->Reset();
  used->Reset();
  head->Reset();
  data->Reset();
  nBlock=0;
}

//--------------------------------------------------
//--------------------------------------------------
int BsmdRawData ::headSize()  const{
return nBlock>0 ? head->GetSize()/ nBlock: 0;
}

//--------------------------------------------------
//--------------------------------------------------
int BsmdRawData ::dataBlockSize()  const{
return nBlock>0 ? data->GetSize()/ nBlock: 0;
}

//--------------------------------------------------
//--------------------------------------------------
void BsmdRawData :: print(int k, FILE *fd) const{
  fprintf(fd,"BsmdRawData:: print() \n");
  fprintf(fd,"used %.0f blocks\n", used->GetSum());

  unsigned char *capsA=(unsigned char*)caps->GetArray();
  unsigned short *usedA=(unsigned short *)used->GetArray();
  int i;
  for(i=0;i<used->GetSize();i++) {
    if(!usedA[i]) continue;
    printf("i=%d used=%d  caps=%d\n",i,usedA[i],capsA[i]);
  }

  int hSize=headSize();
  int bSize=dataBlockSize();
  fprintf(fd,"No. of nonzero blocks %d, size: heder=%d, dataBlock=%d\n", nBlock,hSize,bSize);
  if(nBlock<=0) return;

  if(k<=0) return;

  unsigned short *headA=(unsigned short *)head->GetArray();
  unsigned short *dataA=(unsigned short *)data->GetArray();
  int ib;

  for(ib=0;ib<nBlock;ib++) {
    int j;
    printf("\n======\nSMD BANK=%d header size=%d\n",ib,hSize);
    for(j = 0;j<hSize;j++)  {
      if(j%16==0) printf("\n");
      printf("0x%04x ",headA[ib*hSize+j]);
    }
    printf("\n");
    if(k<=1) continue;

    printf("\n======\nSMD BANK=%d data size=%d\n",ib,bSize);
    for(j = 0;j<bSize;j++)  {
      if(j%16==0) printf("\n");
      printf("0x%04x ",dataA[ib*bSize+j]);
    }
    printf("\n");
  }




}


