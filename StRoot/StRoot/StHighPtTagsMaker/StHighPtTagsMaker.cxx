/***************************************************************************
 *
 * $Id: StHighPtTagsMaker.cxx,v 1.2 2007/04/28 17:56:17 perev Exp $
 *
 * Author: Thomas Henry, July 2004, base on Gene Van Beuren, Feb 1999
 ***************************************************************************
 *
 * Description:  Maker to fill the High Pt Tags
 *
 ***************************************************************************
 *
 * $Log: StHighPtTagsMaker.cxx,v $
 * Revision 1.2  2007/04/28 17:56:17  perev
 * Redundant StChain.h removed
 *
 * Revision 1.1  2004/07/30 23:02:05  calderon
 * Revised entry after testing.  Commented out data members that are used
 * as local variables in fillTag().  Speeded up primary track loop.  Added
 * protection against null pointers.
 *
 * Revision 1.0  2004/07/27 12:02:00  thenry
 * Created
 *
 **************************************************************************/
#include "StHighPtTagsMaker.h"
#include "StEvent.h"
#include "StEventTypes.h"
#include "StTrack.h"
#include "TMath.h"
#include "PhysicalConstants.h"
#include "phys_constants.h"
#include "StThreeVector.hh"

ClassImp(StHighPtTagsMaker)

StHighPtTagsMaker::StHighPtTagsMaker(const char *name, const char *title)
    : StMaker(name, title)
{
  mEvent    = 0;
  mTagTable = 0;       // init pointer to tag table
}

StHighPtTagsMaker::~StHighPtTagsMaker()
{
  if(mTagTable != 0)
    delete mTagTable;
  mTagTable = 0;    // clean up
}

Int_t StHighPtTagsMaker::Init()
{
//   mHighPtTrack = 0;
//   mMaxRawEtBEMCTower = 0;
//   mMaxRawEtEEMCTower = 0;
  
  return StMaker::Init();
}

Int_t StHighPtTagsMaker::Make()
{
  mTagTable = new HighPtTag_st;
  mEvent = (StEvent *) GetInputDS("StEvent");
  if (!mEvent) return kStOK; // If no event, we're done
  
  fillTag();
  St_HighPtTag *HighPtTag = new St_HighPtTag("HighPtTag",1);
  AddData(HighPtTag);
  HighPtTag->AddAt(mTagTable,0);
  delete mTagTable; mTagTable=0;
  return kStOK;
}


HighPtTag_st* StHighPtTagsMaker::tag()
{
  return mTagTable;
}

void StHighPtTagsMaker::fillTag()
{
  //max Pt temporary variables
  Float_t   mHighPtTrack=0.0;
  Float_t   mMaxRawEtBEMCTower=0.0;
  Float_t   mMaxRawEtEEMCTower=0.0;
  
  // speed up the code by looking at primary tracks directly
  if (mEvent->primaryVertex()) {
      StSPtrVecPrimaryTrack& primTracks = mEvent->primaryVertex()->daughters();
      for(StSPtrVecPrimaryTrackIterator i=primTracks.begin(); i!=primTracks.end(); ++i) {
	  StTrack* mtrack = *i;
	  if (!mtrack) continue;
	  if(mtrack->flag()<=0) continue;
	  if(mtrack->fitTraits().numberOfFitPoints() < 15) continue;
	  StThreeVectorF mom = mtrack->geometry()->momentum();
	  if(mom.pseudoRapidity() > 1.5) continue;
	  if(mom.perp() > mHighPtTrack)
	      mHighPtTrack = mom.perp();
      }
  }
  StEmcCollection *emcCol = mEvent->emcCollection();
  if (emcCol) {
      StEmcDetector *bemc = emcCol->detector(kBarrelEmcTowerId);
      StEmcDetector *eemc = emcCol->detector(kEndcapEmcTowerId);
      if (bemc) {
	  Int_t bemcModules = bemc->numberOfModules();
	  for(int i = 0; i < bemcModules; i++) {
	      StEmcModule* mod = bemc->module(i+1);
	      StSPtrVecEmcRawHit &hits = mod->hits();
	      for(StSPtrVecEmcRawHitIterator it=hits.begin(); it!=hits.end(); ++it) {
		  if(mMaxRawEtBEMCTower < (*it)->energy())
		      mMaxRawEtBEMCTower = (*it)->energy();
	      }
	  } // module loop
      } // check for barrel detector
      if (eemc) {
	  Int_t eemcModules = eemc->numberOfModules();
	  for(int i = 0; i < eemcModules; i++) {
	      StEmcModule* mod = eemc->module(i+1);
	      StSPtrVecEmcRawHit &hits = mod->hits();
	      for(StSPtrVecEmcRawHitIterator it=hits.begin(); it!=hits.end(); ++it) {
		  if(mMaxRawEtEEMCTower < (*it)->energy())
		      mMaxRawEtEEMCTower = (*it)->energy();
	      }
	  }// module loop
      }// check for endcap detector
  }// check for emc collection

  mTagTable->HighPtTrack = mHighPtTrack;
  mTagTable->MaxRawEtBEMCTower = mMaxRawEtBEMCTower;
  mTagTable->MaxRawEtEEMCTower = mMaxRawEtEEMCTower;
}

void StHighPtTagsMaker::printTag(ostream& os) 
{
  os << "--- High Pt Tag Table ---" << endl; 
  if (!mTagTable) 
    os << "(empty)" << endl;
  else {
    os << "Pt of High Pt Track:           " << mTagTable->HighPtTrack << endl;
    os << "Maximum Raw Et of BEMC Towers: " << mTagTable->MaxRawEtBEMCTower << endl;
    os << "Maximum Raw Et of EEMC Towers: " << mTagTable->MaxRawEtEEMCTower << endl;
  }   
}
