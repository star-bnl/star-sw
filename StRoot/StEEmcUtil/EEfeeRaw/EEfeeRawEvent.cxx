#include <assert.h>

#include "EEfeeDataBlock.h"
#include "EEfeeRawEvent.h"

ClassImp(EEfeeRawEvent)

//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------

EEfeeRawEvent ::  EEfeeRawEvent() {
  ID=-1;
  block= new TClonesArray("EEfeeDataBlock",10);
  block->Clear();
  // printf("EEfeeRawEvent constructed, add=%p\n",this);
}

//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------

EEfeeRawEvent ::  ~EEfeeRawEvent() {
  delete block;

}

//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------

void EEfeeRawEvent :: clear(){
  ID=-1;
  block->Delete(); // preserve memory
}

//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------

void EEfeeRawEvent :: print(int flag) const{
  printf("\nEEfeeRawEvent ID=%d with DataBlock entered=%d of %d\n",
	 ID,block->GetEntries(),block->GetSize());
  
  int i;
  for(i=0;i<block->GetEntries();i++) {
    ((EEfeeDataBlock *)(block->At(i)))->print(flag);
  }

}

//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------

void EEfeeRawEvent ::addFeeDataBlock(EEfeeDataBlock* b){
   // To avoid calling the very time consuming operator new for each track,
   // the standard but not well know C++ operator "new with placement"
   // is called. If tracks[i] is 0, a new Track object will be created
   // otherwise the previous Track[i] will be overwritten.

  assert(b);
  
  TClonesArray &Block=*block;
  int nB=Block.GetEntries();
  EEfeeDataBlock *bl1= new(Block[nB]) EEfeeDataBlock();
  bl1->set(b);
  //  bl1->print();
}

  


