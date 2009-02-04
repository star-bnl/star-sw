// $Id: EzEEsmdCal.cxx,v 1.11 2009/02/04 20:33:22 ogrebeny Exp $
 
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

#include "StEEmcUtil/database/EEmcDbItem.h"

#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"

#ifdef StRootFREE
  #include "EEmcDb/EEmcDb.h"
#else
  #include "StEEmcUtil/database/StEEmcDb.h"
#endif


ClassImp(EzEEsmdCal)

//--------------------------------------------------
//--------------------------------------------------
EzEEsmdCal::EzEEsmdCal(int sect){
  printf("EzEEsmdCal() constructed\n");
  eHead=0;
  eEve=0;
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
      if(x->stat & killStat) continue; // drop masked chan 

      // accept this hit
      int iphi=(x->sec-1)*MaxSubSec+(x->sub-'A');
      int ieta=x->eta-1;
      assert(iphi>=0 && iphi<MaxPhiBins);
      assert(ieta>=0 && ieta<MaxEtaBins);

      int iT=-1;// store T,P,Q,R depending on 'iT'
      if(x->name[2]=='T'){
	iT=0;
      } else{
	iT=x->name[2]-'P'+1;
      }
      assert(iT>=0 && iT<mxTile);

      float rawAdc=data[chan];  
      float adc=rawAdc-x->ped;
      tileAdc[iT][ieta][iphi]=adc; 
      tileThr[iT][ieta][iphi]=rawAdc>x->thr;
      killT[iT][ieta][iphi]=false;
      if(x->gain<=0) continue;
      // ........ only elements with valid gains are processed below
      tileEne[iT][ieta][iphi]=adc/x->gain; 
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
      // (assuming pre/post is not mixed in hrdware with SMD pixels) 

      if(x->fail ) continue; // drop broken channels
      if(x->stat & killStat) continue; // drop masked chan 
      if(x->sec!=sectID ) continue; // drop data from other sectors

      float rawAdc=data[chan];
      float adc=rawAdc-x->ped;

      if(adc>maxStripAdc) continue; //drop 'jumpy ped'
      smdAdc[x->plane-'U'][x->strip-1]=adc;
      
      if(x->gain<=0)continue; // drop channels w/o gains
      smdEne[x->plane-'U'][x->strip-1]=adc/x->gain;     
    }
  }
  
  return ;
  
}
