/**********************************************************************
 *
 * $Id: StEbyeDSTMaker.cxx,v 1.1.1.1 2000/08/01 13:57:55 jgreid Exp $
 *
 * Author: Jeff Reid, UW, July 2000
 *         incorporates elements of code by
 *         Poskanzer, Snellings, & Voloshin
 *
 **********************************************************************
 *
 * Description:  This is an maker designed to read a STAR dst and
 *               summarize it into an EbyeDST which contains only
 *               the information necessary for EbyE analysis.
 *               The EbyEDST elements are intentionally independent
 *               of any STAR infrastructure so that data from other
 *               experiments (i.e. NA49) can be written in the same
 *               format and accessed and analyzed outside of RCF/BNL.
 *
 **********************************************************************
 *
 * $Log: StEbyeDSTMaker.cxx,v $
 * Revision 1.1.1.1  2000/08/01 13:57:55  jgreid
 * EbyE DST creation and access tools
 *
 *
 *********************************************************************/

#include "StEbyeDSTMaker.h"
#include "StChain.h"
#include "StRun.h"
#include "StEventTypes.h"
#include "StGlobalTrack.h"
#include "SystemOfUnits.h"
#include "StPionPlus.hh"
#include "StPionMinus.hh"
#include "StKaonPlus.hh"
#include "StKaonMinus.hh"
#include "StProton.hh"
#include "StElectron.hh"
//#include "StuProbabilityPidAlgorithm.h"
#include "StTpcDedxPidAlgorithm.h"
#include "StMessMgr.h"

#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

ClassImp(StEbyeDSTMaker)

StEbyeDSTMaker::StEbyeDSTMaker(const Char_t *name, const Char_t *title) : StMaker(name, title) {
  SetFilename("EbyeDST.root");
}

StEbyeDSTMaker::~StEbyeDSTMaker() {
}

Int_t StEbyeDSTMaker::Make() {

  // Get the current event from StEvent
#if 0
  StEventReaderMaker* evMaker = (StEventReaderMaker*) gStChain->Maker("events");
  if (! event()) return kStOK; // If no event, we're done
  StEvent& event = *(evMaker->event());
#endif
  StEvent* mEvent = (StEvent *) GetInputDS("StEvent");
  if (!mEvent) return kStOK; // If no event, we're done
  StEvent& event = *mEvent;
  StRun *run;
  run = (StRun *) GetInputDS("StRun");

  // define the necessary variables
  StEbyeTrack *ebyeTrack = new StEbyeTrack();

  StVertex *primeVertex;
  StTrack *currentTrack;

  StThreeVectorD origin(0,0,0);
  StThreeVectorD primaryVertexPosition;

  Int_t i,j;
  UInt_t k,l;
  Int_t npvtx; // # of primary vertices in this event
  Int_t initialN;

  Double_t s;

  Int_t currentCharge;
  Float_t spi,ska,se,sp;

  StThreeVectorD p,dca;

  StPtrVecTrackPidTraits traits;
  StDedxPidTraits* pid;

  StTpcDedxPidAlgorithm tpcDedxAlgo;
  //StuProbabilityPidAlgorithm stuPidAlgo;

  // If there is no found vertex exit with no error
  //   (kStWarn) so that the DST can be written
  npvtx = event.numberOfPrimaryVertices();
  if (npvtx == 0) return kStWarn;
  
  // loop over the vertices and choose the one with the most 
  //  daughters to be the primary vertex 
  //  (or default to primaryVertex(0) if there is only one)
  //
  // just use primaryVertex(0), it's the best one!

  primeVertex = event.primaryVertex(0);
  //for (i = 1 ; i < npvtx ; i++) {
  //  if (event.primaryVertex(i)->numberOfDaughters() > primeVertex->numberOfDaughters())
  //    primeVertex = event.primaryVertex(i);
  //} 
  
  // if we have a primary vertex go ahead and get
  //  the track nodes and relevant event data 
  if (primeVertex) {
    primaryVertexPosition = primeVertex->position();
    initialN = primeVertex->numberOfDaughters();
    const StSPtrVecTrackNode& theNodes = event.trackNodes();

    // set the event parameters for the EbyeDST
    mEbyeEvent->SetOrigMult(initialN);
    mEbyeEvent->SetCentrality(initialN);  // use multiplicity as centrality measure

    mEbyeEvent->SetEventID((Int_t) event.id());
    mEbyeEvent->SetRunID((Int_t) event.runId());

    for (j = 0 ; j < 32 ; j++) 
      mEbyeEvent->SetCTBarray(j,event.l0Trigger()->coarsePixelArray(j));
     
    mEbyeEvent->SetZDCe(event.triggerDetectorCollection()->zdc().adcSum(east));
    mEbyeEvent->SetZDCw(event.triggerDetectorCollection()->zdc().adcSum(west));
  
    mEbyeEvent->SetVx(primaryVertexPosition.x());
    mEbyeEvent->SetVy(primaryVertexPosition.y());
    mEbyeEvent->SetVz(primaryVertexPosition.z());

    // define a PID functor
    //StuProbabilityPidAlgorithm myPID(event);

    // *** track loop ***
    for (k = 0 ; k < theNodes.size() ; k++) {

      // get the next track, and if it is a primary
      //  track go ahead and fill the EbyeDST track parameters
      currentTrack = theNodes[k]->track(primary);
      if (currentTrack) {

        // calculate eta & phi
        ebyeTrack->SetEta(currentTrack->geometry()->momentum().pseudoRapidity());
        ebyeTrack->SetPhi(currentTrack->geometry()->momentum().phi());
 
        // get the momenta of the current track
        ebyeTrack->SetPx(currentTrack->geometry()->momentum().x());
        ebyeTrack->SetPy(currentTrack->geometry()->momentum().y());
        ebyeTrack->SetPz(currentTrack->geometry()->momentum().z());

        // calculate distance of closest approach to the primary vertex position
        s = currentTrack->geometry()->helix().pathLength(primaryVertexPosition);
        p = currentTrack->geometry()->helix().at(s);
        dca = p-primaryVertexPosition;

        // get impact parameter information
        ebyeTrack->SetBx(dca.x()/centimeter);
        ebyeTrack->SetBy(dca.y()/centimeter);
        ebyeTrack->SetBz(dca.z()/centimeter);

        // dE/dx
        traits = currentTrack->pidTraits(kTpcId);
        for (l = 0; l < traits.size(); l++) {
          pid = dynamic_cast<StDedxPidTraits*>(traits[l]);
          if (pid && pid->method()==kTruncatedMeanId) break;
        }
        ebyeTrack->SetDedx(pid->mean());

        // put PID info into the mEbyeEvent
        currentTrack->pidTraits(tpcDedxAlgo);       // initialize

        currentCharge = currentTrack->geometry()->charge();
        if (currentCharge > 0) {
          spi = (Float_t) tpcDedxAlgo.numberOfSigma(StPionPlus::instance());
          ska = (Float_t) tpcDedxAlgo.numberOfSigma(StKaonPlus::instance());
	} else {
          spi = (Float_t) tpcDedxAlgo.numberOfSigma(StPionMinus::instance());
          ska = (Float_t) tpcDedxAlgo.numberOfSigma(StKaonMinus::instance());
	}
        se = (Float_t) tpcDedxAlgo.numberOfSigma(StElectron::instance());
        sp = (Float_t) tpcDedxAlgo.numberOfSigma(StProton::instance());

        ebyeTrack->SetPIDe(se);
        ebyeTrack->SetPIDp(sp);
        ebyeTrack->SetPIDpi(spi);
        ebyeTrack->SetPIDk(ska);

        // !!!!!!!! update PID when appropriate !!!!!!!!!!
        // invoke the PID functor
        // StParticleDefinition* def=theTrack->pidTraits(myPID);
        // get an electron at 90% confidence
        // if( def->name()=="e-" && myPID.getProbability(0) > 0.9 )

        // put chi2 info into mEbyeEvent
        ebyeTrack->SetChi2(currentTrack->fitTraits().chi2());

        // get track length information
        ebyeTrack->SetNFitPoints(currentTrack->fitTraits().numberOfFitPoints());
        ebyeTrack->SetNFoundPoints(0);   // not defined for STAR
        ebyeTrack->SetNMaxPoints(currentTrack->numberOfPossiblePoints());
  
        // get the track id information
        ebyeTrack->SetDetectorID(0);   // not defined for STAR (yet...)
        ebyeTrack->SetFlag(currentTrack->flag());

        // get charge
        ebyeTrack->SetCharge(currentCharge);

        // add this track to the mEbyeEvent
        mEbyeEvent->AddTrack(ebyeTrack);

      } // *** end if (currentTrack) ***        

    } // *** end of track loop ***

    // fill the tree and clear event (mEbyeEvent)
    mEbyeTree->Fill();
    mEbyeEvent->Clear();

  }

  delete ebyeTrack;

  return kStOk;

}

Int_t StEbyeDSTMaker::Init() {
 
  Int_t split  = 1;       // by default split Event into sub branches
  Int_t comp   = 1;       // by default file is compressed
  Int_t bufsize = 256000;
  if (split) bufsize /= 4;
 
  // create the EbyeEvent and an output file
  mEbyeEvent = new StEbyeEvent();

  mEbyeDST = new TFile(mDSTFilename,"RECREATE","Ebye DST");
  if (!mEbyeDST) {
    cout << "##### EbyeDSTMaker: Warning: no EbyeDST file = " << mDSTFilename << endl;
    return kStFatal;
  }

  // need to set the output file to "old" style
  //  so it is readable by plain ROOT!
  mEbyeDST->SetFormat(1);
 
  mEbyeDST->SetCompressionLevel(comp);
  gMessMgr->Info() << "##### EbyeDSTMaker: EbyeDST file = " << mDSTFilename << endm;
 
  // Create a ROOT Tree and one superbranch
  mEbyeTree = new TTree("EbyeTree", "Ebye DST Tree");
  if (!mEbyeTree) {
    cout << "##### EbyeDSTMaker: Warning: No EbyeTree" << endl;
    return kStFatal;
  }
 
  mEbyeTree->SetAutoSave(10000000);  // autosave when 10 Mbyte written
  mEbyeTree->Branch("EbyeDSTBranch", "StEbyeEvent", &mEbyeEvent,
                    bufsize, split);

  // read in the PID parameters file
  //StuProbabilityPidAlgorithm::readParametersFromFile("nhitsBin_0_10_20_45_ptBin_0_Inf_dcaBin_0_2_50000_Amp.root");

  return kStOK;
}

void StEbyeDSTMaker::Clear(Option_t *opt) {
  StMaker::Clear();
}

Int_t StEbyeDSTMaker::Finish() {

  mEbyeDST->Write();
  mEbyeDST->Close();

  return kStOk;

}
