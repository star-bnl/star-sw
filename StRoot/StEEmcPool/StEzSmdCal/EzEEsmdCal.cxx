// $Id: EzEEsmdCal.cxx,v 1.2 2004/06/15 20:03:26 balewski Exp $
 
#include <assert.h>
#include <stdlib.h>


#include <TClonesArray.h>

#include <TObjArray.h> 

#include "EzEEsmdCal.h"

#include "StEEmcUtil/EEfeeRaw/EEfeeRawEvent.h"
#include "StEEmcUtil/EEfeeRaw/EEstarTrig.h"
#include "StEEmcUtil/EEfeeRaw/EEmcEventHeader.h"

#include "StEEmcUtil/EEfeeRaw/EEfeeDataBlock.h"
#include "StEEmcUtil/EEfeeRaw/EEname2Index.h"

#include "StEEmcDbMaker/EEmcDbItem.h"


#ifdef StRootFREE
  #include "EEmcDb/EEmcDb.h"
#else
  #include "StEEmcDbMaker/StEEmcDbMaker.h"
#endif


ClassImp(EzEEsmdCal)

//--------------------------------------------------
//--------------------------------------------------
EzEEsmdCal::EzEEsmdCal(int sect){
  printf("EzEEsmdCal() constructed\n");
  eHead=0;
  eEve=0;
  eTrig=0;
  setSector(sect);
}

//--------------------------------------------------
//--------------------------------------------------
void EzEEsmdCal::init (){
  EEsmdCal::init();
}

//--------------------------------------------------
//--------------------------------------------------
EzEEsmdCal::~EzEEsmdCal() {/* noop */}


//--------------------------------------------------
//--------------------------------------------------
void EzEEsmdCal::make(){

  EEsmdCal::clear();
  unpackEzTree();
  findSectorMip();// do real analysis
 
}

//--------------------------------------------------
//--------------------------------------------------
void EzEEsmdCal::unpackEzTree(){
  nInpEve++;
  // printf(" EzEEsmdCal::getData()is called ......\n");

  // check CTB multiplicity 
  int ctbSum=0;
  int isl;
  for ( isl = 0; isl < 240; isl++ ) {
    ctbSum+= eTrig -> CTB[isl];
  }
  //printf("ctbSum=%d \n",ctbSum);

  unpackEzTail();
  unpackEzSmd();
  
  return ;

}

//--------------------------------------------------
//--------------------------------------------------
void EzEEsmdCal:: unpackEzTail(){
  // tower/pre/post

  int nCr=0;
  int ic;
  for(ic=0;ic<eEve->block->GetEntries();ic++) {// over dat ablocks
    EEfeeDataBlock *b=(EEfeeDataBlock *)eEve->block->At(ic);
    if( !b->isValid() ) continue; // reject corrupted crates corruption
    
    int crateID=b->getCrateID();    
    //    if(crateID>MaxTwCrateID) continue; // just tower crates
    nCr++; // count number of valid crates

    int chan;
    UShort_t* data=b->getData();
    int nd=b->getValidDataLen();
    
    for(chan=0;chan<nd;chan++) {
      const  EEmcDbItem  *x=eeDb->getByCrate(crateID,chan);
      if(x==0) continue; 
      if(x->isSMD()) break; // abort pre/post crates
      // (assuming pre/post/tw is not mixed with SMD pixels) 
     
      if(x->fail ) continue; // drop broken channels
      
      float rawAdc=data[chan];  

      // accept this hit
      int iphi=(x->sec-1)*MaxSubSec+(x->sub-'A');
      int ieta=x->eta-1;
      assert(iphi>=0 && iphi<MaxPhiBins);
      assert(ieta>=0 && ieta<MaxEtaBins);

      float adc=rawAdc-x->ped;
      int iT=-1;
      if(x->name[2]=='T')
	iT=0;
      else
	iT=x->name[2]-'P'+1;
      assert(iT>=0 && iT<kTile);
    
      tileAdc[iT][ieta][iphi]=adc; // store T,P,Q,R depending on 'iT'
      tileTag[iT][ieta][iphi]=rawAdc>x->thr;
      //printf("%s adc=%f out=%d thr=%f\n",x->name,adc,tileThr[iT][ieta][iphi],x->thr);
    }
  }
  
  return ;

}
//--------------------------------------------------
//--------------------------------------------------
void EzEEsmdCal:: unpackEzSmd(){
  //  printf("EzEEsmdCal:: unpackEzSmd()\n");
  int nCr=0;
  int ic;
  for(ic=0;ic<eEve->block->GetEntries();ic++) {// over dat ablocks
    EEfeeDataBlock *b=(EEfeeDataBlock *)eEve->block->At(ic);
    if( !b->isValid() ) continue; // reject corrupted crates corruption
    
    int crateID=b->getCrateID();    
    if(crateID<=MaxTwCrateID) continue; // drop tower crates
    nCr++; // count number of valid MAPMT crates

    int chan;
    UShort_t* data=b->getData();
    int nd=b->getValidDataLen();
    
    for(chan=0;chan<nd;chan++) {
      const  EEmcDbItem  *x=eeDb->getByCrate(crateID,chan);
      if(x==0) continue; 

      if(!x->isSMD()) break; // abort pre/post crates
      // (assuming pre/post is not mixed with SMD pixels) 

      if(x->fail ) continue; // drop broken channels
      if(x->gain<=0)continue; // drop channels w/o gains

      // tmp mask
      if(strstr(x->name,"05U035")) continue;
      if(strstr(x->name,"05V057")) continue;
      
      float rawAdc=data[chan];  
      float adc=rawAdc-x->ped;
      //     printf(" e=%f ",adc/x->gain); x->print();

      // accept this hit
      smdE[x->sec-1][x->plane-'U'][x->strip-1]=adc/x->gain;
    }
  }
  
  return ;
  
}
