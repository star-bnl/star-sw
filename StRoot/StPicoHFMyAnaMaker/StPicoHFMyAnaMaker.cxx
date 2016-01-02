#include <vector>

#include "TClonesArray.h"

#include "StThreeVectorF.hh"
#include "StLorentzVectorF.hh"

#include "StPicoDstMaker/StPicoDst.h"
#include "StPicoDstMaker/StPicoDstMaker.h"
#include "StPicoDstMaker/StPicoEvent.h"
#include "StPicoDstMaker/StPicoTrack.h"
#include "StPicoDstMaker/StPicoBTofPidTraits.h"

#include "StPicoHFMaker/StPicoHFEvent.h"
#include "StPicoHFMaker/StHFCuts.h"
#include "StPicoHFMaker/StHFPair.h"
#include "StPicoHFMaker/StHFTriplet.h"

#include "StPicoHFMyAnaMaker.h"

ClassImp(StPicoHFMyAnaMaker)

// _________________________________________________________
StPicoHFMyAnaMaker::StPicoHFMyAnaMaker(char const* name, StPicoDstMaker* picoMaker, char const* outputBaseFileName,  
					   char const* inputHFListHFtree = "") :
  StPicoHFMaker(name, picoMaker, outputBaseFileName, inputHFListHFtree),
  mDecayChannel(kChannel1) {
  // constructor
}

// _________________________________________________________
StPicoHFMyAnaMaker::~StPicoHFMyAnaMaker() {
  // destructor
}

// _________________________________________________________
int StPicoHFMyAnaMaker::InitHF() {
  // -- INITIALIZE USER HISTOGRAMS ETC HERE -------------------
  //    add them to the output list mOutList which is automatically written

  // EXAMPLE //  mOutList->Add(new TH1F(...));
  // EXAMPLE //  TH1F* hist = static_cast<TH1F*>(mOutList->Last());
  
  return kStOK;
}

// _________________________________________________________
void StPicoHFMyAnaMaker::ClearHF(Option_t *opt="") {
  return;
}

// _________________________________________________________
int StPicoHFMyAnaMaker::FinishHF() {
  return kStOK;
}

// _________________________________________________________
int StPicoHFMyAnaMaker::MakeHF() {
  // -- process event
  //    ADD YOUR PROCESSING CODE HERE
  //    ... it is usefull to use the methods below
  //     - createCandidates()
  //     - analyzeCandidates()

  if (isMakerMode() == StPicoHFMaker::kWrite) {
    createCandidates();
  }
  else if (isMakerMode() == StPicoHFMaker::kRead) {
    // -- the reading back of the perviously written trees happens in the background
    analyzeCandidates();
  }
  else if (isMakerMode() == StPicoHFMaker::kAnalyze) {
    createCandidates();
    analyzeCandidates();
  }

  return kStOK;
}

// _________________________________________________________
int StPicoHFMyAnaMaker::createCandidates() {
  // create candidate pairs/ triplet and fill them in arrays (in StPicoHFEvent)
  
  // -- ADD USER CODE TO CREATE PARTICLE CANDIDATES --------
  //    - vectors mIdxPicoKaons, mIdxPicoPions mIdxPicoProtons
  //      have been filled in the background using the cuts in HFCuts

  // -- Decay channel1 --- EXAMPLE
  if (mDecayChannel == StPicoHFMyAnaMaker::kChannel1) {

    for (unsigned short idxKaon = 0; idxKaon < mIdxPicoKaons.size(); ++idxKaon) {
      StPicoTrack const *kaon = mPicoDst->track(mIdxPicoKaons[idxKaon]);
      
      for (unsigned short idxPion = 0; idxPion < mIdxPicoPions.size(); ++idxPion) {
	StPicoTrack const *pion = mPicoDst->track(mIdxPicoPions[idxPion]);
	
	if (mIdxPicoKaons[idxKaon] == mIdxPicoPions[idxPion]) 
	  continue;
      
	StHFPair pair(kaon, pion,
		      mHFCuts->getHypotheticalMass(StHFCuts::kKaon), mHFCuts->getHypotheticalMass(StHFCuts::kPion),
		      mIdxPicoKaons[idxKaon], mIdxPicoPions[idxPion], mPrimVtx, mBField);
	if (!mHFCuts->isGoodSecondaryVertexPair(pair)) 
	  continue;
	mPicoHFEvent->addHFSecondaryVertexPair(&pair);
	
      } // for (unsigned short idxPion = 0; idxPion < mIdxPicoPions.size(); ++idxPion) {
    } // for (unsigned short idxKaon = 0; idxKaon < mIdxPicoKaons.size(); ++idxKaon) {
  } // else  if (mDecayChannel == StPicoHFMyAnaMaker::Channel1) {

 return kStOK;
}

// _________________________________________________________
int StPicoHFMyAnaMaker::analyzeCandidates() {
  // -- analyze pairs/triplet, which have been filled in arrays 
  //    (in StPicoHFEvent) before ( via createCandidates() or via reading back from trees )
  //    fill histograms or nTuples ... etc

  // -- ADD USER CODE TO CREATE PARTICLE CANDIDATES --------
  //    - vectors mIdxPicoKaons, mIdxPicoPions mIdxPicoProtons
  //      have been filled in the background using the cuts in HFCuts
  
  // -- Decay channel1
  if (mDecayChannel == StPicoHFMyAnaMaker::kChannel1) {

    // -- fill nTuple / hists for secondary pairs
    TClonesArray const * aCandidates= mPicoHFEvent->aHFSecondaryVertices();
    
    for (unsigned int idx = 0; idx <  mPicoHFEvent->nHFSecondaryVertices(); ++idx) {
      StHFPair const* pair = static_cast<StHFPair*>(aCandidates->At(idx));

      StPicoTrack const* kaon = mPicoDst->track(pair->particle1Idx());
      StPicoTrack const* pion = mPicoDst->track(pair->particle2Idx());

    } // for (unsigned int idx = 0; idx <  mPicoHFEvent->nHFSecondaryVertices(); ++idx) {
  } // else  if (mDecayChannel == StPicoHFMyAnaMaker::kChannel1) {

 return kStOK;
}

// _________________________________________________________
bool StPicoHFMyAnaMaker::isPion(StPicoTrack const * const trk) const {
  // -- good pion
  return true;
}

// _________________________________________________________
bool StPicoHFMyAnaMaker::isKaon(StPicoTrack const * const trk) const {
  // -- good kaon
  return (mHFCuts->isGoodTrack(trk) && mHFCuts->isTPCKaon(trk));
} 

// _________________________________________________________
bool StPicoHFMyAnaMaker::isProton(StPicoTrack const * const trk) const {
  // -- good proton
  return (mHFCuts->isGoodTrack(trk) && mHFCuts->isTPCProton(trk) && mHFCuts->isTOFProton(trk));
}

