/***************************************************************************
 *
 * $Id: StHbtMuDstMakerReaderG.cxx,v 1.1 2002/11/19 23:42:36 renault Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 **************************************************************************/
#include "StChain.h"

#include "StEvent/StEventTypes.h"

#include "StEventUtilities/StuRefMult.hh"
#include "StEventUtilities/StuProbabilityPidAlgorithm.h"

#include "StarClassLibrary/StPhysicalHelixD.hh"
#include "StarClassLibrary/StTimer.hh"

#include "StFlowMaker/StFlowMaker.h"
#include "StFlowMaker/StFlowSelection.h"
#include "StFlowMaker/StFlowEvent.h"

#include "StStrangeMuDstMaker/StStrangeMuDstMaker.h"
#include "StStrangeMuDstMaker/StStrangeEvMuDst.hh"
#include "StStrangeMuDstMaker/StV0MuDst.hh"
#include "StStrangeMuDstMaker/StV0Mc.hh"
#include "StStrangeMuDstMaker/StXiMuDst.hh"
#include "StStrangeMuDstMaker/StXiMc.hh"
#include "StStrangeMuDstMaker/StKinkMuDst.hh"
#include "StStrangeMuDstMaker/StKinkMc.hh"

#include "StMuDSTMaker/COMMON/StMuException.hh"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuDebug.h"
#include "StMuDSTMaker/COMMON/StMuCut.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"

#include "StHbtMuDstMakerReaderG.h"
#include "Infrastructure/StHbtEvent.hh"

#include "TFile.h"
#include "TTree.h"
#include "TClass.h"
#include "TChain.h"
#include "TStreamerInfo.h"
#include "TClonesArray.h"

ClassImp(StHbtMuDstMakerReaderG)

#if !(ST_NO_NAMESPACES)
  using namespace units;
#endif


//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
StHbtMuDstMakerReaderG::StHbtMuDstMakerReaderG(StMuDstMaker* maker) : 
  mMuDstMaker(maker), mFlowMaker(0),
  mTrackType(primary), mReadTracks(1), 
  mReadV0s(1), mReadXis(1), mReadKinks(1), mFinish(0),
  mHbtEvent(0)
{
  mEventCounter=0;
}

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
StHbtMuDstMakerReaderG::~StHbtMuDstMakerReaderG(){
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StHbtMuDstMakerReaderG::clear(){
  DEBUGMESSAGE1("");
  //  if (mHbtEvent) { delete mHbtEvent; mHbtEvent=0; }
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
int StHbtMuDstMakerReaderG::Init(){
  DEBUGMESSAGE1("");
//   if (!ioMaker) mIOMaker = (StIOMaker*)GetMaker("IOMaker");
//   if (!mStStrangeMuDstMaker) mStStrangeMuDstMaker = (StStrangeMuDstMaker*)GetMaker("StrangeMaker");
//   if (!mFlowMaker)  = (StFlowMaker*)GetMaker("FlowMaker");
  return 0;
}

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StHbtMuDstMakerReaderG::Clear(){
  DEBUGMESSAGE1("");
  clear();
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
StHbtEvent* StHbtMuDstMakerReaderG::ReturnHbtEvent(){
  DEBUGMESSAGE1("");
  StTimer timer;
  timer.start();
  clear();
  mMuDst=mMuDstMaker->muDst();
  if (mMuDst) {
    DEBUGVALUE3(mMuDst);
    mMuDst->fixTrackIndices();
    if (mReadV0Daughters){mHbtEvent = new StHbtEvent(mMuDst, mTrackType,mReadV0Daughters);}
    else mHbtEvent = new StHbtEvent(mMuDst, mTrackType); // Gael 24 Sept 02
  }

  if (mFlowMaker && mHbtEvent ) {
    int harmonic = 1;
    mFlowMaker->FlowSelection()->SetHarmonic(harmonic);
    float psi = mFlowMaker->FlowEventPointer()->Psi(mFlowMaker->FlowSelection());
    mHbtEvent->SetReactionPlane(psi);
  }

  return mHbtEvent;
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StHbtMuDstMakerReaderG::Finish() {
  if (mFinish) {
    for ( int i=0; i<10; i++) {
      cout << "why are you calling the Finish() again  ???????" << endl;
      cout << "are you the stupid chain destructor ???????????" << endl;
    }
  }
  else {
    mFinish = true;
  }
  return;
}
void StHbtMuDstMakerReaderG::setProbabilityPidFile(const char* file) {
  if (mProbabilityPidAlgorithm)
    mProbabilityPidAlgorithm->readParametersFromFile(file);
}
/***************************************************************************
 *
 * $Log: StHbtMuDstMakerReaderG.cxx,v $
 * Revision 1.1  2002/11/19 23:42:36  renault
 * Adding new function to enable reading V0 daughters
 *
 *
 **************************************************************************/


















