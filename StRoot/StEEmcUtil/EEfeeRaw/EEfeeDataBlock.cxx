#include <cassert>

#include "EEdims.h"

#include "EEfeeDataBlock.h"

ClassImp(EEfeeDataBlock)

const int EEfeeDataBlock::DefaultMaxHead=4;
const int EEfeeDataBlock::DefaultMaxData=192;


//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------

EEfeeDataBlock ::  EEfeeDataBlock() {
  MaxHead =  DefaultMaxHead;
  MaxData =  0 ; 
  head = new UShort_t[MaxHead];
  data = NULL;
}


EEfeeDataBlock::EEfeeDataBlock(const EEfeeDataBlock *b) {
  MaxData = b->getDataLen();
  MaxHead = b->getHeadLen();
  head=0;
  if(MaxHead>0) head = new UShort_t[MaxHead];
  data=0;
  if(MaxData>0) data = new UShort_t[MaxData];
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
  printf("\n --> token=0x%2x  crateID=0x%x  trigComm=0x%x  lenCount=0x%x  errFlag=0x%x\n   NpositiveData=%d\n",
	 getToken(),getCrateID(),getTrigComm(),getLenCount(),getErrFlag(),getNData(0));
  
  if(flag<=0) return;

  int nd=getDataLen();
  printf("Data[%3d]:",nd);
  for(int i=0;i<nd;i++) {
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
void EEfeeDataBlock ::setHead(const UShort_t *h) {
  if(h) 
    memcpy(head,h,sizeof(head[0])*MaxHead);
  else // empty header==>clear
    memset(head,0,sizeof(head[0])*MaxHead);

}


//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------
int EEfeeDataBlock ::getNData(int thres) const {
  int n=0;
  int i;
  const int nd=getValidDataLen();
  for(i=0;i<nd;i++) if(data[i]>thres) n++;
  return n;
}

//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------

void EEfeeDataBlock ::setDataArray(const UShort_t *dIn, int size) {
 const UShort_t x=0,*d=&x;
  if(dIn) {
    d=dIn;
  } else {
    size =1;
  }
  
  if(size!=MaxData) { // tmp, was '>' 
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
int  EEfeeDataBlock :: isValid(){
      	return (getCrateID()!=0x00ff);// add more conditions as emerge
}

//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------
int  EEfeeDataBlock 
::isHeadValid(int token, int crId, int len, int trigComm, int errFlag){
#if 0
  printf("ask/0x: %x %x %x %x %x\n", token,crId,len,trigComm,errFlag);
  print(0);
  printf("getCrateID()/0x = %x %x\n",getCrateID(),crId);
  printf("getToken()/0x = %x %x\n",getToken(),token);
  printf("getLenCount()/0x = %x %x\n",getLenCount(),len);
  printf("getTrigComm()/0x = %x %x\n",getTrigComm(),trigComm);
  printf("getErrFlag()/0x = %x %x\n",getErrFlag(),errFlag);
#endif

  if(getCrateID()!=crId) return 0; // discard
  if(getToken()!=token) return 0; // discard
  if(getLenCount()!=len) return 0;
  if(getTrigComm()!=trigComm) return 0;
  if(getErrFlag()!=errFlag) return 0;
  return 1; // good header
}



/*
 * $Log: EEfeeDataBlock.cxx,v $
 * Revision 1.13  2004/04/16 17:26:46  balewski
 * more header checking, some mess introduced
 *
 * Revision 1.12  2004/04/02 06:38:52  balewski
 * *** empty log message ***
 *
 * Revision 1.11  2004/03/25 16:54:58  balewski
 * cleanup of arguments
 *
 * Revision 1.10  2004/03/20 20:25:55  balewski
 * *** empty log message ***
 *
 * Revision 1.9  2004/01/27 07:09:37  balewski
 * slower but simpler
 *
 * Revision 1.8  2003/12/03 18:55:41  zolnie
 * fixed yet another bug
 *
 * Revision 1.7  2003/12/02 17:22:07  balewski
 * fix after version mixup
 *
 * Revision 1.5  2003/11/24 05:40:55  balewski
 * new stuff for miniDaq
 *
 * Revision 1.4  2003/11/20 16:01:46  balewski
 * towars run 4
 *
 */

