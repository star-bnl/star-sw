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

int EEfeeRawEvent::getNGoodBlock() {
  int ic;
  int nOK=0;
  for(ic=0;ic<block->GetEntries();ic++) {
    EEfeeDataBlock *b=(EEfeeDataBlock *)block->At(ic);
    if(!b->isValid()) continue;
    // printf("icr=%d san=%d ok=%d\n",ic,b->getSanity(),nOK);
    nOK++;
  }
  return nOK;
}



//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------

int EEfeeRawEvent::maskWrongCrates( long timeStamp, unsigned headToken, HeadVer headVersion) {

  /* check for:
     - token in every data block
     - crateID vs. positon in event
     - RETURN # of good crate headers
  */

  if(timeStamp< 1068744930) {// Thu Nov 13 12:35:30 2003
    printf(" maskWrongCrates() not implemented for time stamp   : %ld / %s",timeStamp,ctime((const time_t *)& timeStamp));
    assert(1==2);
  }

  // add more patterns below
  int listA[]={1,2,3,4,5,6};
  int listB[]={1,2,3,4,5,6,84,85,86};
  int listC[]={1,2,3,4,5,6,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,
 18,17,16,30,29,28,27,26,25,24,23,22,21,20,19,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15}; // etow+esmd+btow
  // summer 2004 shut down
  int listD[]={1,2,3,4,5,6,100,101,102,103,1,2,3,4,1,2,3,4,1,2,3,4}; // Renee, put here 4 carte ID's
  
  
  int *list, dim;
  if (timeStamp< 1068761131)  //Thu Nov 13 17:05:31 2003
    { list=listA; dim=sizeof(listA)/sizeof(int); }
  else  if (timeStamp< 1070474417 ) //  Wed Dec  3 13:00:17 2003
    { list=listB; dim=sizeof(listB)/sizeof(int); }
  else if (timeStamp< 1093360000  )// about  Aug 24 14:56:04 2004
    { list=listC; dim=sizeof(listC)/sizeof(int); }
  else 
    { list=listD; dim=sizeof(listD)/sizeof(int); }


  // printf("\n Header check:\n");
  int ic;
  int nOK=0;
  for(ic=0;ic<block->GetEntries();ic++) {
    EEfeeDataBlock *b=(EEfeeDataBlock *)block->At(ic);
    assert(ic<dim); // fix it, use DB for crIdSwitch
    // tmp, should be taken from DB as in StEEmcDataMaker.cxx
    int lenCount=0;
    int errFlag=0;
    int trigCommand=4; // physics, 9=laser/LED, 8=??

    if(ic<6) {// ETOW
      //lenCount=4+5*32; // real .daq content
      lenCount=5*32; //  old ezTree header
    } else if (ic<22) { // ESMD //tmp
      //lenCount=4+192;// real .daq content
      lenCount=12*16;//  old ezTree header
      errFlag=0x28;
    } else { //BTOW
      lenCount=10*16;//  old ezTree header
    };

    // this is messy, contact Jan before you change between  ezTree header & .daq header
          
    UChar_t  sick=0;
    switch (headVersion) {
    case headVer1:  // real .daq content
      sick  =b->isHeadValid(headToken,list[ic],lenCount,trigCommand,errFlag);
      break;
    case  headVer2: // old ezTree header from siew
      sick=b->isHeadValid(headToken,list[ic],errFlag,lenCount,trigCommand);
      break;
    case  headVer3:     // miniDaq2004 ezTree header 
      errFlag=9; 
      if(ic<6) errFlag=4; else errFlag=9; // yes, October 2004, JB
      sick=b->isHeadValid(headToken,list[ic],trigCommand,errFlag,lenCount);
      break;
    default:
      assert(1==2); // make up your mind men!
    }

    // printf(" ic=%d crID=%d sick=0x%02x\n",ic,b->getCrateID(),sick);

    // printf("XXX b=%d crID=%d=%d  tok=%d=%d ok=%d\n",ic, b->getCrateID() ,list[ic],b->getToken(), headToken,ok);
    
    if(!sick) nOK++;
   
  }
  //  printf("nOK=%d\n",nOK);
  return nOK;
}

//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------

void EEfeeRawEvent ::  maskBEMC(){  // kill BTOW crates

  // tmp,22  should be taken from DB as in StEEmcDataMaker.cxx
  for(int ic=22;ic<block->GetEntries();ic++) {
    EEfeeDataBlock *b=(EEfeeDataBlock *)block->At(ic);
    b->maskCrate();
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
 * Revision 1.21  2004/10/13 17:14:31  balewski
 * one more header change for miniDaq
 *
 * Revision 1.20  2004/09/07 20:32:01  balewski
 * more methods, remove questionable spin bits interpetation
 *
 * Revision 1.19  2004/08/24 20:30:08  balewski
 * for Renne LED test with mini
 *
 * Revision 1.18  2004/07/10 20:12:13  balewski
 * match miniDaq
 *
 * Revision 1.17  2004/07/09 02:38:05  balewski
 * BTOW data are not masked out any more but headres are checked as for EEMC
 *
 * Revision 1.16  2004/06/21 19:50:21  balewski
 * mre detailed monitoring of data corruption
 *
 * Revision 1.15  2004/06/01 16:05:18  balewski
 * forgoten update of data block headers check
 *
 * Revision 1.14  2004/04/16 17:26:46  balewski
 * more header checking, some mess introduced
 *
 * Revision 1.13  2004/01/27 15:13:57  balewski
 * it is tricky with BTOW
 *
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
