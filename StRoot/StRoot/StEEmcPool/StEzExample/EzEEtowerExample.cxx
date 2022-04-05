// $Id: EzEEtowerExample.cxx,v 1.2 2009/02/04 20:33:21 ogrebeny Exp $
 
#include <assert.h>
#include <stdlib.h>


#include <TClonesArray.h>

#include <TObjArray.h> 

#include "EzEEtowerExample.h"

#include "StEEmcUtil/EEfeeRaw/EEfeeRawEvent.h"
#include "StEEmcUtil/EEfeeRaw/EEstarTrig.h"
#include "StEEmcUtil/EEfeeRaw/EEmcEventHeader.h"

#include "StEEmcUtil/EEfeeRaw/EEfeeDataBlock.h"
#include "StEEmcUtil/EEfeeRaw/EEname2Index.h"

#include "StEEmcUtil/database/EEmcDbItem.h"


#ifdef StRootFREE
  #include "EEmcDb/EEmcDb.h"
#else
  #include "StEEmcUtil/database/StEEmcDb.h"
#endif


ClassImp(EzEEtowerExample)
//--------------------------------------------------
//--------------------------------------------------
EzEEtowerExample::EzEEtowerExample(){
  printf("EzEEtowerExample() constructed\n");
  eHead=0;
  eEve=0;
  eTrig=0;

}

//--------------------------------------------------
//--------------------------------------------------
void EzEEtowerExample::init (){
  printf("EzEEtowerExample() init\n");
  EEtower::init();
}

//--------------------------------------------------
//--------------------------------------------------
EzEEtowerExample::~EzEEtowerExample() {/* noop */}


//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------
void EzEEtowerExample::make(){
  EEtower::clear();
  unpackEzTree();

  //print();
  task1(); // do real analysis
}

//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------
void EzEEtowerExample::unpackEzTree(){
  nInpEve++;
  // printf(" EzEEtowerExample::getData()is called ......\n");

  // check CTB multiplicity 
  int ctbSum=0;
  int isl;
  for ( isl = 0; isl < 240; isl++ ) {
    ctbSum+= eTrig -> CTB[isl];
  }
  //printf("ctbSum=%d \n",ctbSum);
    
  int nCr=0;
  int ic;
  for(ic=0;ic<eEve->block->GetEntries();ic++) {// over dat ablocks
    EEfeeDataBlock *b=(EEfeeDataBlock *)eEve->block->At(ic);
    if( !b->isValid() ) continue; // reject corrupted crates corruption
    
    int crateID=b->getCrateID();    
    if(crateID>MaxTwCrateID) continue; // just tower crates
    nCr++; // count number of valid crates

    int chan;
    UShort_t* data=b->getData();
    int nd=b->getValidDataLen();
    
    for(chan=0;chan<nd;chan++) {
      const  EEmcDbItem  *x=eeDb->getByCrate(crateID,chan);
      if(x==0) continue; 
      if(x->fail ) continue; // drop broken channels

      float rawAdc=data[chan]; // raw ADC 
      if(rawAdc <x->thr) continue;

      // accept this hit
      float value=(rawAdc-x->ped)/x->gain;
      int iphi=(x->sec-1)*5+(x->sub-'A');
      int ieta=x->eta-1;
      assert(iphi>=0 && iphi<MaxPhiBins);
      assert(ieta>=0 && ieta<MaxEtaBins);
    
      towerE[ieta][iphi]=value;
    }
  }
  
  return ;

}
