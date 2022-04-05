/*******************************************************************
 *
 * $Id: StBTofMatchEffMaker.cxx,v 1.4 2018/02/26 23:26:51 smirnovd Exp $
 *
 * Author: Xin Dong
 *****************************************************************
 *
 * Description: TPC->TOF match efficiency maker
 *
 *******************************************************************/
#include <iostream>
#include "StEventTypes.h"
#include "Stypes.h"
#include "StThreeVectorF.hh"
#include "StHelix.hh"
#include "StTrackGeometry.h"
#include "StDedxPidTraits.h"
#include "StTrackPidTraits.h"
#include "StBTofPidTraits.h"
#include "StarClassLibrary/StParticleTypes.hh"
#include "StarClassLibrary/StParticleDefinition.hh"
#include "StTpcDedxPidAlgorithm.h"
#include "StEventUtilities/StuRefMult.hh"
#include "PhysicalConstants.h"
#include "StHelix.hh"
#include "TH1.h"
#include "TH2.h"
#include "TFile.h"
#include "TTree.h"
#include "StMessMgr.h"
#include "StMemoryInfo.hh"
#include "StTimer.hh"
#include "StBTofMatchEffMaker.h"
//#include "TMemStat.h"


//---------------------------------------------------------------------------
StBTofMatchEffMaker::StBTofMatchEffMaker(const Char_t *name): StMaker(name){
  // set default values
  mEventCounter = 0;
  mAcceptedEventCounter = 0;
  mTofEventCounter = 0;
  mAcceptAndBeam = 0;

  setMinFitPointsPerTrack(15);
  setMinFitPointsOverMax(0.52);
  setMaxDCA(3.);

  setCreateHistoFlag(kFALSE);
  setHistoFileName("tofana.root");
  doPrintMemoryInfo = kFALSE;
  doPrintCpuInfo    = kFALSE;
}

StBTofMatchEffMaker::~StBTofMatchEffMaker(){ /* nope */}

//void StBTofMatchEffMaker::Clear(Option_t *opt){StMaker::Clear();}

//---------------------------------------------------------------------------
Int_t StBTofMatchEffMaker::Init(){
  LOG_INFO << "StBTofMatchEffMaker -- initializing ..." << endm;
  if(Debug()) {
    LOG_INFO << "Minimum fitpoints per track: " << mMinFitPointsPerTrack << endm;
    LOG_INFO << "Minimum fitpoints over max: " << mMinFitPointsOverMax << endm;
    LOG_INFO << "Maximum DCA: " << mMaxDCA << endm;
  }
  
  // m_Mode can be set by SetMode() method
  if(m_Mode) {
//    setHistoFileName("tofana.root");
  } else {
    setHistoFileName("");
  }

  if (mHisto){
    bookHistograms();
    LOG_INFO << "Histograms are booked" << endm;
    if (mHistoFileName!="") {
      LOG_INFO << "Histograms will be stored in " << mHistoFileName.c_str() << endm;
    }
  }

  // reset event counters
  mEventCounter = 0;
  mAcceptedEventCounter = 0;
  mTofEventCounter = 0;
  mAcceptAndBeam = 0;
  
  return kStOK;
}

//---------------------------------------------------------------------------
Int_t StBTofMatchEffMaker::Finish(){

  LOG_INFO << "StBTofMatchEffMaker -----  RUN SUMMARY ----- (Finish)\n"
       << "\tProcessed "  << mEventCounter << " events."
       << " Accepted  "   << mAcceptedEventCounter << " events."
       << " Rejected  "   << mEventCounter - mAcceptedEventCounter << " events\n"
       << "\tTOF events " << mTofEventCounter
       << "\t Accept & Beam   "   << mAcceptAndBeam   << " events" << endm;
  
  //if (mHisto) writeHistogramsToFile();
  if (mHistoFileName!="") writeHistogramsToFile();
  return kStOK;
}


//---------------------------------------------------------------------------
Int_t StBTofMatchEffMaker::Make(){
  LOG_INFO << "StBTofMatchEffMaker -- welcome" << endm;
  if(Debug()) LOG_INFO << " processing event ... " << endm;

  if(mHisto) mEventCounterHisto->Fill(0);
  // event selection ...
  mEvent = (StEvent *) GetInputDS("StEvent");
  if (!validEvent(mEvent)){
    LOG_INFO << "StBTofMatchEffMaker -- nothing to do ... bye-bye" << endm;
    return kStOK;
  }

  // timing & memory info -only when requested-
  StTimer timer;
  if (doPrintCpuInfo) timer.start();
  if (doPrintMemoryInfo) StMemoryInfo::instance()->snapshot();

  //.........................................................................
  // check for tofCollection and fill local copy with ADC and TDC data
  StBTofCollection *theTof = mEvent->btofCollection(); if (theTof){};

  //.........................................................................
  /// loop over primary tracks
  //

  StSPtrVecTrackNode& nodes = mEvent->trackNodes();
  Int_t nAllTracks=0;
  for (unsigned int iNode=0; iNode<nodes.size(); iNode++){
    StPrimaryTrack *theTrack = dynamic_cast<StPrimaryTrack*>(nodes[iNode]->track(primary));
    if(!validTrack(theTrack)) continue;

    StThreeVectorF mom = theTrack->geometry()->momentum();
    float pt = mom.perp();
    float eta = mom.pseudoRapidity();
    int q = theTrack->geometry()->charge();

    float  nSigmaPion       = -999.;
    float  nSigmaKaon       = -999.;
    float  nSigmaProton     = -999.;
    static StPionPlus* Pion = StPionPlus::instance();
    static StKaonPlus* Kaon = StKaonPlus::instance();
    static StProton* Proton = StProton::instance();

    static StTpcDedxPidAlgorithm PidAlgorithm;
    const StParticleDefinition* pd = theTrack->pidTraits(PidAlgorithm);
    if (pd) {
      nSigmaPion = PidAlgorithm.numberOfSigma(Pion);
      nSigmaKaon = PidAlgorithm.numberOfSigma(Kaon);
      nSigmaProton = PidAlgorithm.numberOfSigma(Proton);
    }

    nAllTracks++;

    if(mHisto) {
      if(fabs(nSigmaPion)<2.) mPionDen->Fill(pt, eta);
      if(fabs(nSigmaKaon)<2.) mKaonDen->Fill(pt, eta);
      if(fabs(nSigmaProton)<2.) {
        if(q>0) mProtonDen->Fill(pt, eta);
        else mAntiPDen->Fill(pt, eta);
      }
    }

    const StPtrVecTrackPidTraits& theTofPidTraits = theTrack->pidTraits(kTofId);
    if(theTofPidTraits.size()) continue;

    StTrackPidTraits *theSelectedTrait = theTofPidTraits[theTofPidTraits.size()-1];
    if(!theSelectedTrait) continue;

    StBTofPidTraits *pidTof = dynamic_cast<StBTofPidTraits *>(theSelectedTrait);
    if(!pidTof) continue;

    if(pidTof->matchFlag()<=0) continue;   // no match

    if(mHisto) {
      if(fabs(nSigmaPion)<2.) mPionNum->Fill(pt, eta);
      if(fabs(nSigmaKaon)<2.) mKaonNum->Fill(pt, eta);
      if(fabs(nSigmaProton)<2.) {
        if(q>0) mProtonNum->Fill(pt, eta);
        else mAntiPNum->Fill(pt, eta);
      }
    }


  } // loop over nodes


  if (doPrintMemoryInfo) {
        StMemoryInfo::instance()->snapshot();
        StMemoryInfo::instance()->print();
  }
  if (doPrintCpuInfo) {
    timer.stop();
    LOG_INFO << "CPU time for StBTofMatchEffMaker::Make(): "
	 << timer.elapsedTime() << " sec" << endm;
  }

  LOG_INFO << "StBTofMatchEffMaker -- bye-bye" << endm;

  return kStOK;
}

//---------------------------------------------------------------------------
// Book histograms and create ordered collection for easy manipulation
void StBTofMatchEffMaker::bookHistograms(void){

  mEventCounterHisto = new TH1D("eventCounter","eventCounter",20,0,20);

  mPionDen   = new TH2D("pionDen","pionDen",500,0.,5.,40,-1.,1.);
  mKaonDen   = new TH2D("kaonDen","kaonDen",500,0.,5.,40,-1.,1.);
  mProtonDen = new TH2D("protonDen","protonDen",500,0.,5.,40,-1.,1.);
  mAntiPDen  = new TH2D("antiPDen","antiPDen",500,0.,5.,40,-1.,1.);

  mPionNum   = new TH2D("pionNum","pionNum",500,0.,5.,40,-1.,1.);
  mKaonNum   = new TH2D("kaonNum","kaonNum",500,0.,5.,40,-1.,1.);
  mProtonNum = new TH2D("protonNum","protonNum",500,0.,5.,40,-1.,1.);
  mAntiPNum  = new TH2D("antiPNum","antiPNum",500,0.,5.,40,-1.,1.);

  return;
}


//---------------------------------------------------------------------------
// store histograms in a seperate root file
void StBTofMatchEffMaker::writeHistogramsToFile(){
  // Output file
  TFile *theHistoFile =  new TFile(mHistoFileName.c_str(), "RECREATE");
  LOG_INFO << "StBTofMatchEffMaker::writeHistogramsToFile()"
       << " histogram file " <<  mHistoFileName << endm;

  theHistoFile->cd();

  if(mHisto) {

    mEventCounterHisto->Write();

    mPionDen->Write();
    mKaonDen->Write();
    mProtonDen->Write();
    mAntiPDen->Write();

    mPionNum->Write();
    mKaonNum->Write();
    mProtonNum->Write();
    mAntiPNum->Write();
    
    theHistoFile->Write();  
    theHistoFile->Close(); 
   
  }

  return;
}

//---------------------------------------------------------------------------
// determine whether this is a valid TOF beam event
bool StBTofMatchEffMaker::validEvent(StEvent *event){
  mEventCounter++;
  // 1. must have non-zero pointer
  if (!event) return false;
  if(mHisto) mEventCounterHisto->Fill(1);

  // 2. must have a valid primary vertex 
  if (!event->primaryVertex()) return false;
  mAcceptedEventCounter++;
  if(mHisto) mEventCounterHisto->Fill(2);

  return true;
}


//---------------------------------------------------------------------------
// determine whether this is a valid TPC track
bool StBTofMatchEffMaker::validTrack(StTrack *track){
  // 1. no track, no go.
  if (!track) return false;

  // 2. track quality flag, should be >0
  if (track->flag()<=0) return false;

  // 3. minimum #hits per track - not necessary now in ITTF
//  if (track->topologyMap().numberOfHits(kTpcId) < mMinHitsPerTrack) return false;
  // 4. minimum #fit points per track
  if (track->fitTraits().numberOfFitPoints(kTpcId) < mMinFitPointsPerTrack) return false;
  // 5. minimum #fit points over #maximum points
  float ratio = (1.0*track->fitTraits().numberOfFitPoints(kTpcId)) / (1.0*track->numberOfPossiblePoints(kTpcId));
  if (ratio < mMinFitPointsOverMax) return false;
  // 6. maximum dca
  if (track->impactParameter() > mMaxDCA) return false;

  return true;
}

/*****************************************************************
 *
 * $Log: StBTofMatchEffMaker.cxx,v $
 * Revision 1.4  2018/02/26 23:26:51  smirnovd
 * StTof: Remove outdated ClassImp macro
 *
 * Revision 1.3  2018/02/26 23:13:20  smirnovd
 * Move embedded CVS log messages to the end of file
 *
 * Revision 1.2  2010/01/28 18:16:53  perev
 * WarningOff
 *
 * Revision 1.1  2009/02/26 21:23:17  dongx
 * first release - example to calculate the TPC->TOF matching efficiency
 *
 */
