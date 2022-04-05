/***************************************************************************
 *
 * $Id: StHbtMuDstMakerReader.cxx,v 1.5 2004/10/12 13:48:53 kisiel Exp $
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

#include "StHbtMuDstMakerReader.h"
#include "StHbtMaker/Infrastructure/StHbtEvent.hh"
#include "StHbtMaker/Base/StHbtEventCut.h"

#include "TFile.h"
#include "TTree.h"
#include "TClass.h"
#include "TChain.h"
#include "TStreamerInfo.h"
#include "TClonesArray.h"

ClassImp(StHbtMuDstMakerReader)

#if !(ST_NO_NAMESPACES)
  using namespace units;
#endif


//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
StHbtMuDstMakerReader::StHbtMuDstMakerReader(StMuDstMaker* maker) : 
  mFlowMaker(0), mMuDstMaker(maker), 
  mTrackType(primary), mReadTracks(1), 
  mReadV0s(1), mReadXis(1), mReadKinks(1), mFinish(0),
  mHbtEvent(0)
{
  mEventCounter=0;
  mReaderStatus = 0;
  
}

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
StHbtMuDstMakerReader::~StHbtMuDstMakerReader(){
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StHbtMuDstMakerReader::clear(){
  DEBUGMESSAGE1("");
  //  if (mHbtEvent) { delete mHbtEvent; mHbtEvent=0; }
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
int StHbtMuDstMakerReader::Init(){
  DEBUGMESSAGE1("");
//   if (!ioMaker) mIOMaker = (StIOMaker*)GetMaker("IOMaker");
//   if (!mStStrangeMuDstMaker) mStStrangeMuDstMaker = (StStrangeMuDstMaker*)GetMaker("StrangeMaker");
//   if (!mFlowMaker)  = (StFlowMaker*)GetMaker("FlowMaker");
  return 0;
}

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StHbtMuDstMakerReader::Clear(){
  DEBUGMESSAGE1("");
  clear();
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
StHbtEvent* StHbtMuDstMakerReader::ReturnHbtEvent(){
  DEBUGMESSAGE1("");
  StTimer timer;
  timer.start();
  clear();
  mMuDst=mMuDstMaker->muDst();
  mHbtEvent=0;
  if (mMuDst && mMuDst->event() ) {
    DEBUGVALUE3(mMuDst);
    mMuDst->fixTrackIndices();
    mHbtEvent = new StHbtEvent(mMuDst, mTrackType);

    // Apply event cut, if available
    if (mEventCut) {
      if (!(mEventCut->Pass(mHbtEvent))){
        delete mHbtEvent;
        mHbtEvent = 0;
        return 0;
      }
    }
  }
  else 
    {
      // DTSMaker returned NULL, so we are not OK
      mReaderStatus = 1;
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
void StHbtMuDstMakerReader::Finish() {
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
void StHbtMuDstMakerReader::setProbabilityPidFile(const char* file) {
  if (mProbabilityPidAlgorithm)
    mProbabilityPidAlgorithm->readParametersFromFile(file);
}
/***************************************************************************
 *
 * $Log: StHbtMuDstMakerReader.cxx,v $
 * Revision 1.5  2004/10/12 13:48:53  kisiel
 * Properly hadle mReaderStatus to make frontLoadedEventCut work
 *
 * Revision 1.4  2004/02/24 19:50:00  magestro
 * Added optional event cut
 *
 * Revision 1.3  2003/02/13 19:02:20  magestro
 * Removed offending cout statement
 *
 * Revision 1.2  2003/01/31 20:22:57  magestro
 * Small changes to eliminate compiler warnings
 *
 * Revision 1.1  2002/08/27 18:21:26  laue
 * New reader. Wrapper around the StMuDstMaker
 *
 *
 **************************************************************************/


















