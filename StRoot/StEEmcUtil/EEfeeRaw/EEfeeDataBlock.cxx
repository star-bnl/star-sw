#include <cassert>

#include "EEfeeDataBlock.h"

ClassImp(EEfeeDataBlock)

 
const int EEfeeDataBlock::DefaultMaxHead  =   4;
const int EEfeeDataBlock::DefaultMaxData  = 128;


//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------

EEfeeDataBlock ::  EEfeeDataBlock() {
  MaxHead =  DefaultMaxHead;
  MaxData =  0 ; // DefaultMaxData;
  head = new UShort_t[MaxHead];
  //data = new UShort_t[MaxData];
  data = NULL;
}


EEfeeDataBlock::EEfeeDataBlock(const EEfeeDataBlock *b) {
  MaxData = b->getDataLen();
  MaxHead = b->getHeadLen();
  head = new UShort_t[MaxHead];
  data = new UShort_t[MaxData];
  set(b);
}




//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------
EEfeeDataBlock ::  ~EEfeeDataBlock() {
  if(head) delete [] head;
  if(data) delete [] data;
}
 
//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------
void EEfeeDataBlock :: print(int flag){
  printf("feeDataBlock Head: 0x%04hx 0x%04hx 0x%04hx 0x%04hx ",head[0],head[1],head[2],head[3]);
  printf("\n --> token=0x%2x  crateID=0x%x  trigType=0x%x  NpositiveData=%d\n",
	 getToken(),getCrateID(),getTrigType(),getNData(0));
  
  if(flag<=0) return;

  printf("Data:");
  for(int i=0;i<MaxData;i++) {
    if( i%8 == 0 ) printf("\n");
    printf("0x%04hx ",data[i]);

  }
  printf("\n");

}

//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------

void
EEfeeDataBlock ::  set(const EEfeeDataBlock *b) {
  setHead(b->getHead());
  setDataArray(b->getData(),b->getDataLen());
}


//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------
void EEfeeDataBlock ::setHead(UShort_t *h) {
  assert(h);
  memcpy(head,h,sizeof(head[0])*MaxHead);
}


//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------
int EEfeeDataBlock ::getNData(int thres) const {
  int n=0;
  int i;
  for(i=0;i<MaxData;i++) if(data[i]>thres) n++;
  return n;
}

//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------

void EEfeeDataBlock ::setDataArray(UShort_t *d, int size) {
  assert(d);
  if(size>MaxData) { 
    if(data) delete [] data;
    MaxData = size;
    data = new UShort_t[MaxData];
  } else {
    memset(data,0x0,sizeof(data[0])*MaxData);
  }
  memcpy(data,d,size*sizeof(data[0]));
}


//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------

void EEfeeDataBlock ::setData(int chan, UShort_t d){
  assert(chan>=0);
  if(chan>=MaxData) {
    Int_t     newsize = MaxData + DefaultMaxData;
    UShort_t *newdata = new UShort_t[newsize];
    if(data) { 
      memcpy(newdata,data,MaxData);
      delete [] data;
    }
    data    = newdata;
    MaxData = newsize;
  }
  data[chan]=d;
}


//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------

void EEfeeDataBlock :: clear(){
  if(head) memset(head,0,sizeof(head[0])*MaxHead);
  if(data) memset(data,0,sizeof(data[0])*MaxData);
}

//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------


