// $Id: EzEEsoloPi0.cxx,v 1.1 2004/08/26 04:39:40 balewski Exp $
 
#include <assert.h>
#include <stdlib.h>


#include <TClonesArray.h>

#include <TObjArray.h> 

#include "EzEEsoloPi0.h"

#include "StEEmcUtil/EEfeeRaw/EEfeeRawEvent.h"
#include "StEEmcUtil/EEfeeRaw/EEstarTrig.h"
#include "StEEmcUtil/EEfeeRaw/EEmcEventHeader.h"

#include "StEEmcUtil/EEfeeRaw/EEfeeDataBlock.h"
#include "StEEmcUtil/EEfeeRaw/EEname2Index.h"

#include "StEEmcDbMaker/EEmcDbItem.h"

#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"


#ifdef StRootFREE
  #include "EEmcDb/EEmcDb.h"
#else
  #include "StEEmcDbMaker/StEEmcDbMaker.h"
#endif


ClassImp(EzEEsoloPi0)

//--------------------------------------------------
//--------------------------------------------------
EzEEsoloPi0::EzEEsoloPi0(){
  printf("EzEEsoloPi0() constructed\n");
  eHead=0;
  eEve=0;
  eTrig=0;
}
 
//--------------------------------------------------
//--------------------------------------------------
void EzEEsoloPi0::init (){
  EEsoloPi0::initRun(eHead->getRunNumber());
  EEsoloPi0::init();
}

//--------------------------------------------------
//--------------------------------------------------
EzEEsoloPi0::~EzEEsoloPi0() {/* noop */}


//--------------------------------------------------
//--------------------------------------------------
void EzEEsoloPi0::make(){

  EEsoloPi0::clear();
  unpackEzTree();

  findTowerClust();
  findTowerPi0();
  
}

//--------------------------------------------------
//--------------------------------------------------
void EzEEsoloPi0::unpackEzTree(){
  nInpEve++;
  // printf(" EzEEsoloPi0::getData()is called ......\n");

  unpackEzTail();
  unpackEzSmd();
  return ;
}

//--------------------------------------------------
//--------------------------------------------------
void EzEEsoloPi0:: unpackEzTail(){
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
      
      // accept this hit
      int iphi=(x->sec-1)*MaxSubSec+(x->sub-'A');
      int ieta=x->eta-1;
      assert(iphi>=0 && iphi<MaxPhiBins);
      assert(ieta>=0 && ieta<MaxEtaBins);
      int ispir=iphi*MaxEtaBins+ieta; // unified spiral index
      assert(ispir>=0 && ispir<EEsoloPi0::MxTw);

      int iT=-1;// store T,P,Q,R depending on 'iT'
      if(x->name[2]=='T'){
	iT=0;
      } else{
	iT=x->name[2]-'P'+1;
      }
      assert(iT>=0 && iT<mxTile);

      float rawAdc=data[chan];  
      float adc=rawAdc-x->ped; 
      adc=adc;//aa
      //aa      tileAdc[iT][ieta][iphi]=adc; 
      //aa tileThr[iT][ieta][iphi]=rawAdc>x->thr;

      if(x->gain<=0) continue;
      // ........ only elements with valid gains are processed below
      float  ene=adc/x->gain;
      float recoEner=ene/scaleFactor; // ideal 
     //aa tileEne[iT][ieta][iphi]=adc/x->gain; 
      if(iT==0)   soloMip[ispir].e= recoEner;
    }
  }
  
  return ;

}
//--------------------------------------------------
//--------------------------------------------------
void EzEEsoloPi0:: unpackEzSmd(){
  //  printf("EzEEsoloPi0:: unpackEzSmd()\n");
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
      // (assuming pre/post is not mixed in hrdware with SMD pixels) 

      if(x->fail ) continue; // drop broken channels
     
      float rawAdc=data[chan];
      float adc=rawAdc-x->ped;
      adc=adc;//aa
      //aa  smdAdc[x->plane-'U'][x->strip-1]=adc;
      
      if(x->gain<=0)continue; // drop channels w/o gains
      //aa smdEne[x->plane-'U'][x->strip-1]=adc/x->gain;
    }
  }
  
  return ;
  
}
