#include "StTofMuDstEval.h"

#include "StChain.h"
#include "TF1.h"
#include "TRandom.h"

#include <iostream>
#include <fstream>
#include <stdlib.h>
#include <string>
#include <vector>
#include <math.h>
#include <cmath>

#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StThreeVector.hh"
#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"
#include "StLorentzVectorD.hh"

#include "StEvent.h"
#include "StTofCollection.h"
#include "StTofRawData.h"
#include "StTofUtil/StSortTofRawData.h"
#include "StTofUtil/StTofrGeometry.h"
#include "StTriggerIdCollection.h"
#include "StTriggerId.h"

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuTofHit.h"

#include "StTofMuDstEval.h"


//_________________________________________________
StTofMuDstEval::StTofMuDstEval(const char *name, StMuDstMaker *maker):StMaker(name)
{
  //  StTofMuDstEval Constructor
  // - zero all pointers defined in the header file
  mEvent = 0;
  mSortTofRawData = 0;
  mTofrGeom = 0;
  mTofCollection = 0;
  mMuDstMaker = maker;
  Clear("C");
}

//_________________________________________________
StTofMuDstEval::~StTofMuDstEval()
{
  Clear("C");
}

//_____________________________________________________________________________

void StTofMuDstEval::Clear(const char*)
{
  if(mEvent) delete mEvent;
  mEvent = 0;
  if(mSortTofRawData) delete mSortTofRawData;
  mSortTofRawData = 0;
  StMaker::Clear();
}

//_________________________________________________
Int_t StTofMuDstEval::Finish()
{
  if(mTofrGeom) delete mTofrGeom;

  return StMaker::Finish();
}


//_________________________________________________
Int_t StTofMuDstEval::Init()
{
  Clear("C");

  mTofrGeom = new StTofrGeometry("tofrGeom","tofrGeom in MuDstEval");
  if(!mTofrGeom->IsInitDone()) {
    gMessMgr->Info("TofrGemetry initialization..." ,"OS");
    TVolume *starHall = (TVolume *)GetDataSet("HALL");
    mTofrGeom->Init(starHall);
  }

  return kStOK;
}
//_________________________________________________
Int_t StTofMuDstEval::Make()
{
  cout << " StTofMuDstEval::Make()" << endl;

  if( mMuDstMaker != NULL ) {
    mMuDst = mMuDstMaker->muDst();
  }

  if(!mMuDst) return kStOK;

  mEvent = new StEvent();
  mTofCollection = new StTofCollection();
  mEvent->setTofCollection(mTofCollection);
  int nTofRawData = mMuDst->numberOfTofRawData();
  for(int i=0;i<nTofRawData;i++) {
    StTofRawData *aRawData;
    StTofRawData *aMuRawData = (StTofRawData *)mMuDst->tofRawData(i);
    if(aMuRawData) {
      unsigned short leteFlag = aMuRawData->leteFlag();
      unsigned short channel = aMuRawData->channel();
      unsigned int tdc = aMuRawData->tdc();
      unsigned short quality = aMuRawData->quality();
      aRawData = new StTofRawData(leteFlag,channel,tdc,quality);
    } else {
      aRawData = new StTofRawData(0, 0, 0, 0);
    }
    mTofCollection->addRawData(aRawData);
  }
  mSortTofRawData = new StSortTofRawData(mTofCollection);

  return kStOK;
}
//_________________________________________________
void StTofMuDstEval::GetPvpdNHits(int &neast, int &nwest)
{
  IntVec validchannel = mSortTofRawData->GetValidChannel();

  int used[mNPVPD]          = {0,0,0,0,0,0};
  int channum[mNPVPD]       ={-1,-1,-1,-1,-1,-1};
  float mPVPDLeTime[mNPVPD]   = {0.,0.,0.,0.,0.,0.};
  float mPVPDTot[mNPVPD] = {0.,0.,0.,0.,0.,0.};
  
  const Float_t mPVPDTotMin = 2.0;
  const Float_t mPVPDTotMax = 8.0;

  for(unsigned int ich=0;ich<validchannel.size();ich++){
    int chan = validchannel[ich];
    if(chan<mNTOFr5) continue;       // need only pvpd 
    int ichan = chan - mNTOFr5;
    if(ichan<0||ichan>=mNPVPD) continue;
    if(used[ichan]>0) continue;                   // skip multi hits
    used[ichan]++;
    // leading edge
    int tmptdc = (mSortTofRawData->GetLeadingTdc(chan))[0];    
    float pvpdletdc = tmptdc;
    mPVPDLeTime[ichan] = pvpdletdc * VHRBIN2PS/1000.;
    // Trailing edge
    tmptdc = (mSortTofRawData->GetTrailingTdc(chan))[0];    
    float pvpdtetdc = tmptdc;
    float tetime = pvpdtetdc * HRBIN2PS/1000.;
    mPVPDTot[ichan] = tetime - mPVPDLeTime[ichan];
    channum[ichan]=ichan;
  }
  
  // sort out the piled-up hit - remove those hits not from this event
  int nPVPDFired = 0;
  for(int i=0;i<mNPVPD;i++) {
    if(channum[i]!=i) {
      mPVPDLeTime[i] = 0.;
      mPVPDTot[i] = 0.;
    } else {
      nPVPDFired++;
    }
  }
  for(int i=0;i<mNPVPD;i++) {
    if(channum[i]!=i) continue;
    int n0 = 0;
    for(int j=0;j<mNPVPD;j++) {
      if(channum[j]!=j) continue;
      float dt = mPVPDLeTime[j] - mPVPDLeTime[i];
      if(fabs(dt)<200.) n0++;
    }

    if(n0>=nPVPDFired/2) {  // OK
    } else { // not from this event
      mPVPDLeTime[i] = 0.;
      mPVPDTot[i] = 0.;
    }
  }

  
  for(int i=0;i<mNPVPD/2;i++) {
    if(mPVPDLeTime[i]>0.&&mPVPDTot[i]>mPVPDTotMin&&mPVPDTot[i]<mPVPDTotMax) neast++;
    int j=i+mNPVPD/2;
    if(mPVPDLeTime[j]>0.&&mPVPDTot[j]>mPVPDTotMin&&mPVPDTot[j]<mPVPDTotMax) nwest++;
  }

  return;
}
//_________________________________________________
Float_t StTofMuDstEval::GetUncorrectedTot(StMuTofHit *tofHit)
{
  Float_t tot = -999.;

  int daqIndex = tofHit->daqIndex();
  IntVec validchannel = mSortTofRawData->GetValidChannel();
  for(unsigned int ich=0;ich<validchannel.size();ich++){
    int chan = validchannel[ich];
    if(chan==daqIndex) {
      int tmptdc = (mSortTofRawData->GetLeadingTdc(chan))[0];    
      float letime = tmptdc * VHRBIN2PS/1000.;
      tmptdc = (mSortTofRawData->GetTrailingTdc(chan))[0];    
      float tetime = tmptdc * HRBIN2PS/1000.;
      tot = tetime - letime;
      break;
    }
  }

  return tot;
}
//_________________________________________________
void StTofMuDstEval::GetLocalHitPosition(StMuTofHit *tofHit, Double_t* local)
{
  if(!tofHit) return;
  int itray = tofHit->trayIndex();
  int imodule = tofHit->moduleIndex();
  StTofrGeomSensor* sensor = mTofrGeom->GetGeomSensor(imodule,itray);
  if(!sensor) {
    gMessMgr->Warning("","OS") << " No sensitive module in the projection??? -- Something weird!!! " << endm;
    return;
  }

  Double_t global[3] = {0.,0.,0.};
  global[0] = tofHit->projectedPoint().x();
  global[1] = tofHit->projectedPoint().y();
  global[2] = tofHit->projectedPoint().z();

  sensor->Master2Local(&global[0],local);
  delete sensor;
  return;
}
