#include <cmath>
#include "TH1F.h"
#include "TH2F.h"
#include "TFile.h"
#include "TString.h"
#include "StPicoDstMaker/StPicoEvent.h"
#include "StPicoPrescales/StPicoPrescales.h"
#include "StPicoHFEvent.h"
#include "StHFPair.h"
#include "StHFTriplet.h"

#include "StHFHists.h"

class StPicoPrescales;
ClassImp(StHFHists)


StHFHists::StHFHists() : TNamed("StHFHists", "StHFHists"),
  mEventList(NULL), mSecondaryPairList(NULL), mTertiaryPairList(NULL), mTripletList(NULL), mPrescales(NULL), mNRuns(0) {
}


StHFHists::StHFHists(const char* name) : TNamed(name, name),
  mEventList(NULL), mSecondaryPairList(NULL), mTertiaryPairList(NULL), mTripletList(NULL), mPrescales(NULL), mNRuns(0){
}


StHFHists::~StHFHists()
{

  if (mPrescales)
    delete mPrescales;
  mPrescales = NULL;
  // note that histograms are owned by mOutFile. They will be destructed 
  // when the file is closed.
}


void StHFHists::init (TList * outList, unsigned int mode){
  // -- init method to set up internal lists /hists

  // path to lists of triggers prescales
  // lists are obtained from http://www.star.bnl.gov/protected/common/common2014/trigger2014/plots_au200gev/
  const char * prescalesFilesDirectoryName = "./run14AuAu200GeVPrescales";
  mPrescales = new StPicoPrescales(prescalesFilesDirectoryName); // fix dir name
  mNRuns = mPrescales->numberOfRuns();
   

  // -- event list
  outList->Add(new TList);
  mEventList = static_cast<TList*>(outList->Last());
  mEventList->SetOwner(kTRUE);
  mEventList->SetName("baseHFEventHists");
  // -- create event hists
  mEventList->Add(new TH1F("mh1TotalEventsInRun","totalEventsInRun;runIndex;totalEventsInRun",mNRuns+1,0,mNRuns+1));
  //  mEventList->Add(new TH1F("mh1TotalHftTracksInRun","totalHftTracksInRun;runIndex;totalHftTracksInRun",mNRuns+1,0,mNRuns+1));
  mEventList->Add(new TH1F("mh1TotalGRefMultInRun","totalGRefMultInRun;runIndex;totalGRefMultInRun",mNRuns+1,0,mNRuns+1));
  mEventList->Add(new TH1F("mh1TotalHFSecondaryVerticesInRun","totalHFSecondaryVerticesInRun;runIndex;totalHFSecondaryVerticesInRun",mNRuns+1,0,mNRuns+1));
  mEventList->Add(new TH1F("mh1TotalHFTertiaryVerticesInRun","totalHFTertiaryVerticesInRun;runIndex;totalHFTertiaryVerticesInRun",mNRuns+1,0,mNRuns+1));
  mEventList->Add(new TH2F("mh2NHFSecondaryVsNHFTertiary","nHFSecondaryVsnHFTertiary;nHFTertiary;nHFSecondary",300,0,300,300,0,300));
 

  if (mode == StPicoHFEvent::kTwoParticleDecay || mode == StPicoHFEvent::kTwoAndTwoParticleDecay) {
    // -- secondaryPair histogram list
    outList->Add(new TList);
    mSecondaryPairList = static_cast<TList*>(outList->Last());
    mSecondaryPairList->SetOwner(kTRUE);
    mSecondaryPairList->SetName("baseHFSecondaryPairHists");
    // -- create secondaryPair candidate hists
    mSecondaryPairList->Add(new TH2F("mh2SecondaryPairParticle1DcaVsPt","secondaryPairParticle1DcaVsPt;p_{T}(secondaryPair)(GeV/c));secondaryPairParticle1Dca(cm)",120,0,12,200,0,0.02));
    mSecondaryPairList->Add(new TH2F("mh2SecondaryPairParticle2DcaVsPt","secondaryPairParticle2DcaVsPt;p_{T}(secondaryPair)(GeV/c));secondaryPairParticle2Dca(cm)",120,0,12,200,0,0.02));
    mSecondaryPairList->Add(new TH2F("mh2SecondaryPairCosThetaStarVsPt","secondaryPairCosThetaStarVsPt;p_{T}(secondaryPair)(GeV/c));cos(#theta)",120,0,12,550,0,1.1));
    mSecondaryPairList->Add(new TH2F("mh2SecondaryPairDcaDaughtersVsPt","secondaryPairDcaDaughtersVsPt;p_{T}(secondaryPair)(GeV/c));dcaDaughters(cm)",120,0,12,200,0,0.02));
    mSecondaryPairList->Add(new TH2F("mh2SecondaryPairDecayLenghtVsPt","secondaryPairDecayLenghtVsPt;p_{T}(secondaryPair)(GeV/c));secondaryPairDecayLenght(cm)",120,0,12,500,0,0.2));
  }
  

  if (mode == StPicoHFEvent::kTwoAndTwoParticleDecay) {
    // -- tertiaryPair% histogram list
    outList->Add(new TList);
    mTertiaryPairList = static_cast<TList*>(outList->Last());
    mTertiaryPairList->SetOwner(kTRUE);
    mTertiaryPairList->SetName("baseHFTertiaryPairHists");
    // -- create tertiaryPair candidate hists
    mTertiaryPairList->Add(new TH2F("mh2TertiaryPairParticle1DcaVsPt","tertiaryPairParticle1DcaVsPt;p_{T}(tertiaryPair)(GeV/c));tertiaryPairParticle1Dca(cm)",120,0,12,200,0,0.02));
    mTertiaryPairList->Add(new TH2F("mh2TertiaryPairParticle2DcaVsPt","tertiaryPairParticle2DcaVsPt;p_{T}(tertiaryPair)(GeV/c));tertiaryPairParticle2Dca(cm)",120,0,12,200,0,0.02));
    mTertiaryPairList->Add(new TH2F("mh2TertiaryPairCosThetaStarVsPt","tertiaryPairCosThetaStarVsPt;p_{T}(tertiaryPair)(GeV/c));cos(#theta)",120,0,12,550,0,1.1));
    mTertiaryPairList->Add(new TH2F("mh2TertiaryPairDcaDaughtersVsPt","tertiaryPairDcaDaughtersVsPt;p_{T}(tertiaryPair)(GeV/c));dcaDaughters(cm)",120,0,12,200,0,0.02));
    mTertiaryPairList->Add(new TH2F("mh2TertiaryPairDecayLenghtVsPt","tertiaryPairDecayLenghtVsPt;p_{T}(tertiaryPair)(GeV/c));tertiaryPairDecayLenght(cm)",120,0,12,500,0,0.2));
  }

  if (mode == StPicoHFEvent::kThreeParticleDecay || mode == StPicoHFEvent::kTwoAndTwoParticleDecay) {
    // -- triplet histogram list
    outList->Add(new TList);
    mTripletList = static_cast<TList*>(outList->Last());
    mTripletList->SetOwner(kTRUE);
    mTripletList->SetName("baseHFTripletHists");
    // -- create triplet candidate hists
    mTripletList->Add(new TH2F("mh2TripletParticle1DcaVsPt","tripletParticle1DcaVsPt;p_{T}(triplet)(GeV/c));dcaDaughters(cm)",120,0,12,200,0,0.02));
    mTripletList->Add(new TH2F("mh2TripletParticle2DcaVsPt","tripletParticle2DcaVsPt;p_{T}(triplet)(GeV/c));dcaDaughters(cm)",120,0,12,200,0,0.02));
    mTripletList->Add(new TH2F("mh2TripletParticle3DcaVsPt","tripletParticle3DcaVsPt;p_{T}(triplet)(GeV/c));dcaDaughters(cm)",120,0,12,200,0,0.02));
    mTripletList->Add(new TH2F("mh2TripletCosThetaStarVsPt","tripletCosThetaStarVsPt;p_{T}(triplet)(GeV/c));cos(#theta)",120,0,12,550,0,1.1));
    mTripletList->Add(new TH2F("mh2TripletDcaDaughters12VsPt","tripletDcaDaughters12VsPt;p_{T}(triplet)(GeV/c));dcaDaughters(cm)",120,0,12,200,0,0.02));
    mTripletList->Add(new TH2F("mh2TripletDcaDaughters23VsPt","tripletDcaDaughters23VsPt;p_{T}(triplet)(GeV/c));dcaDaughters(cm)",120,0,12,200,0,0.02));
    mTripletList->Add(new TH2F("mh2TripletDcaDaughters31VsPt","tripletDcaDaughters31VsPt;p_{T}(triplet)(GeV/c));dcaDaughters(cm)",120,0,12,200,0,0.02));
    mTripletList->Add(new TH2F("mh2TripletDecayLenghtVsPt","tripletDecayLenghtVsPt;p_{T}(triplet)(GeV/c));tripletDecayLenght(cm)",120,0,12,500,0,0.2));
  }
}


//-----------------------------------------------------------------------
// fill general histograms for all events
//void StHFHists::fillEventHists(StPicoEvent const& picoEvent,StPicoHFEvent const & picoHFEvent,unsigned int const nHftTracks)
void StHFHists::fillEventHists(StPicoEvent const& picoEvent,StPicoHFEvent const & picoHFEvent)
{
  int runIndex = mPrescales->runIndex(picoHFEvent.runId());
  (static_cast<TH1F*>(mEventList->FindObject("mh1TotalEventsInRun")))->Fill(runIndex);
  //(static_cast<TH1F*>(mEventList->FindObject("mh1TotalHftTracksInRun")))->Fill(runIndex,nHftTracks);
  (static_cast<TH1F*>(mEventList->FindObject("mh1TotalGRefMultInRun")))->Fill(runIndex,picoEvent.grefMult());
  (static_cast<TH1F*>(mEventList->FindObject("mh1TotalHFSecondaryVerticesInRun")))->Fill(runIndex,picoHFEvent.nHFSecondaryVertices());
  (static_cast<TH1F*>(mEventList->FindObject("mh1TotalHFTertiaryVerticesInRun")))->Fill(runIndex,picoHFEvent.nHFTertiaryVertices());
  (static_cast<TH2F*>(mEventList->FindObject("mh2NHFSecondaryVsNHFTertiary")))->Fill(picoHFEvent.nHFTertiaryVertices(),picoHFEvent.nHFSecondaryVertices());
}

// fill general histograms for good events
void StHFHists::fillGoodEventHists(StPicoEvent const& picoEvent,StPicoHFEvent const & picoHFEvent)
{ 
  //  int runIndex = mPrescales->runIndex(picoHFEvent.runId()); 
}


// fill histograms for pair candidates
void StHFHists::fillSecondaryPairHists(StHFPair const* const t, bool const fillMass)
{
  (static_cast<TH2F*>(mSecondaryPairList->FindObject("mh2SecondaryPairParticle1DcaVsPt")))->Fill(t->pt(),t->particle1Dca());
  (static_cast<TH2F*>(mSecondaryPairList->FindObject("mh2SecondaryPairParticle2DcaVsPt")))->Fill(t->pt(),t->particle2Dca());
  (static_cast<TH2F*>(mSecondaryPairList->FindObject("mh2SecondaryPairCosThetaStarVsPt")))->Fill(t->pt(),t->cosThetaStar());
  (static_cast<TH2F*>(mSecondaryPairList->FindObject("mh2SecondaryPairDcaDaughtersVsPt")))->Fill(t->pt(),t->dcaDaughters());
  (static_cast<TH2F*>(mSecondaryPairList->FindObject("mh2SecondaryPairDecayLenghtVsPt")))->Fill(t->pt(),t->decayLength());
//vertex position histos?
}

// fill histograms for pair candidates
void StHFHists::fillTertiaryPairHists(StHFPair const* const t, bool const fillMass)
{
  (static_cast<TH2F*>(mTertiaryPairList->FindObject("mh2TertiaryPairParticle1DcaVsPt")))->Fill(t->pt(),t->particle1Dca());
  (static_cast<TH2F*>(mTertiaryPairList->FindObject("mh2TertiaryPairParticle2DcaVsPt")))->Fill(t->pt(),t->particle2Dca());
  (static_cast<TH2F*>(mTertiaryPairList->FindObject("mh2TertiaryPairCosThetaStarVsPt")))->Fill(t->pt(),t->cosThetaStar());
  (static_cast<TH2F*>(mTertiaryPairList->FindObject("mh2TertiaryPairDcaDaughtersVsPt")))->Fill(t->pt(),t->dcaDaughters());
  (static_cast<TH2F*>(mTertiaryPairList->FindObject("mh2TertiaryPairDecayLenghtVsPt")))->Fill(t->pt(),t->decayLength());
//vertex position histos?
}

// fill histograms for triplet candidates
void StHFHists::fillTripletHists(StHFTriplet const* const t, bool const fillMass)
{
  (static_cast<TH2F*>(mTripletList->FindObject("mh2TripletParticle1DcaVsPt")))->Fill(t->pt(),t->particle1Dca());
  (static_cast<TH2F*>(mTripletList->FindObject("mh2TripletParticle2DcaVsPt")))->Fill(t->pt(),t->particle2Dca());
  (static_cast<TH2F*>(mTripletList->FindObject("mh2TripletParticle3DcaVsPt")))->Fill(t->pt(),t->particle3Dca());
  (static_cast<TH2F*>(mTripletList->FindObject("mh2TripletCosThetaStarVsPt")))->Fill(t->pt(),t->cosThetaStar());
  (static_cast<TH2F*>(mTripletList->FindObject("mh2TripletDcaDaughters12VsPt")))->Fill(t->pt(),t->dcaDaughters12());
  (static_cast<TH2F*>(mTripletList->FindObject("mh2TripletDcaDaughters23VsPt")))->Fill(t->pt(),t->dcaDaughters23());
  (static_cast<TH2F*>(mTripletList->FindObject("mh2TripletDcaDaughters31VsPt")))->Fill(t->pt(),t->dcaDaughters31());
  (static_cast<TH2F*>(mTripletList->FindObject("mh2TripletDecayLenghtVsPt")))->Fill(t->pt(),t->decayLength());
  //vertex position histos?
}
