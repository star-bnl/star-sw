#include <cassert>
#include <ctime>

#include "EEfeeDataBlock.h"
#include "EEfeeRawEvent.h"

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
    printf("%d-",i+1);
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


//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------

void EEfeeRawEvent::maskWrongCrates( long timeStamp, unsigned headToken) {

  /* check for:
     - token in every data block
     - crateID vs. positon in event
  */

  if(timeStamp< 1068744930) {// Thu Nov 13 12:35:30 2003
    printf(" maskWrongCrates() not implemented for time stamp   : %ld / %s",timeStamp,ctime((const time_t *)& timeStamp));
    assert(1==2);
  }

  // add more patterns below
  int listA[]={1,2,3,4,5,6};
  int listB[]={1,2,3,4,5,6,84,85,86};
  int listC[]={1,2,3,4,5,6,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99};

  int *list, dim;
  if (timeStamp< 1068761131)  //Thu Nov 13 17:05:31 2003
    { list=listA; dim=sizeof(listA)/sizeof(int); }
  else  if (timeStamp< 1070474417 ) //  Wed Dec  3 13:00:17 2003
    { list=listB; dim=sizeof(listB)/sizeof(int); }
  else 
    { list=listC; dim=sizeof(listC)/sizeof(int); }

  int ic;
  for(ic=0;ic<block->GetEntries();ic++) {
    EEfeeDataBlock *b=(EEfeeDataBlock *)block->At(ic);
    int crateID=b->getCrateID();
    if(ic>=dim ||  crateID!=list[ic] || headToken!=b->getToken()) b->maskCrate();
    //pr-intf("vvv %d %d \n",i,crateID);
  }
  
}

//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------

UShort_t  EEfeeRawEvent::getValue(int crateID, int channel) const {
  int i;
  for(i=0;i<block->GetEntries();i++) {
    EEfeeDataBlock *b=(EEfeeDataBlock *)block->At(i);
    if( crateID!=b->getCrateID()) continue;
    int nd=b->getValidDataLen();
    assert(channel>=0 );
    assert(channel<nd );
    UShort_t* data=b->getData();
    return data[channel];
  }
  return 0xffff; 
}


/*
 * $Log: EEfeeRawEvent.cxx,v $
 * Revision 1.12  2004/01/13 16:32:28  balewski
 * fix bug for sector 8 mapmt
 *
 * Revision 1.11  2003/12/10 04:43:19  balewski
 * first QA
 *
 * Revision 1.10  2003/12/04 18:29:25  balewski
 * I forgot
 *
 * Revision 1.9  2003/12/02 17:22:08  balewski
 * fix after version mixup
 *
 * Revision 1.7  2003/11/24 18:26:47  balewski
 * *** empty log message ***
 *
 * Revision 1.6  2003/11/24 05:40:55  balewski
 * new stuff for miniDaq
 *
 * Revision 1.5  2003/11/20 22:59:40  balewski
 * *** empty log message ***
 *
 * Revision 1.4  2003/11/20 16:01:46  balewski
 * towars run 4
 *
 */
