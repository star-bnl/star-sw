#include "TString.h"
#include "TFile.h"
#include "TTree.h"

#include "StPicoDstMaker/StPicoEvent.h"
#include "StPicoKfVertexEvent.h"

StPicoKfVertexEvent::StPicoKfVertexEvent(char const* fileBaseName): mOutputFile(NULL), mTree(NULL)
{
   TString baseName(fileBaseName);
   mOutputFile = new TFile(Form("%s.kfVertex.root", fileBaseName), "RECREATE");
   mOutputFile->SetCompressionLevel(1);
   int BufSize = (int)pow(2., 16.);
   mTree = new TTree("kfEvent", "event information and kfVertex", BufSize);
   mTree->SetAutoSave(1000000); // autosave every 1 Mbytes

   mTree->Branch("mRunId   ", &mRunId   , "mRunId/I");
   mTree->Branch("mEventId ", &mEventId , "mEventId/I");
   mTree->Branch("mRefMult ", &mRefMult , "mRefMult/I");
   mTree->Branch("mGRefMult", &mGRefMult, "mGRefMult/I");
   mTree->Branch("mNTracks" , &mNTracks,  "mNTracks/I");
   mTree->Branch("mNTracksSubEvt1" , &mNTracksSubEvt1,  "mNTracksSubEvt1/I");
   mTree->Branch("mNTracksSubEvt2" , &mNTracksSubEvt2,  "mNTracksSubEvt2/I");

   mTree->Branch("mVx      ", &mVx      , "mVx/F");
   mTree->Branch("mVy      ", &mVy      , "mVy/F");
   mTree->Branch("mVz      ", &mVz      , "mVz/F");

   mTree->Branch("mKfVx    ", &mKfVx    , "mKfVx/F");
   mTree->Branch("mKfVy    ", &mKfVy    , "mKfVy/F");
   mTree->Branch("mKfVz    ", &mKfVz    , "mKfVz/F");

   mTree->Branch("mKfSubEvt1Vx    ", &mKfSubEvt1Vx    , "mKfSubEvt1Vx/F");
   mTree->Branch("mKfSubEvt1Vy    ", &mKfSubEvt1Vy    , "mKfSubEvt1Vy/F");
   mTree->Branch("mKfSubEvt1Vz    ", &mKfSubEvt1Vz    , "mKfSubEvt1Vz/F");

   mTree->Branch("mKfSubEvt2Vx    ", &mKfSubEvt2Vx    , "mKfSubEvt2Vx/F");
   mTree->Branch("mKfSubEvt2Vy    ", &mKfSubEvt2Vy    , "mKfSubEvt2Vy/F");
   mTree->Branch("mKfSubEvt2Vz    ", &mKfSubEvt2Vz    , "mKfSubEvt2Vz/F");
}

void StPicoKfVertexEvent::closeFile()
{
  mOutputFile->cd();
  mOutputFile->Write();
  mOutputFile->Close();
}

void StPicoKfVertexEvent::addEvent(StPicoEvent const& picoEvent,StThreeVectorF const* const kfFullEvent,
                                   StThreeVectorF const* const kfSubEvt1,StThreeVectorF const* const kfSubEvt2,
                                   int const nTracksFullEvt,int const nTracksSubEvt1,int const nTracksSubEvt2)
{
  mRunId    = picoEvent.runId();
  mEventId  = picoEvent.eventId();
  mRefMult  = picoEvent.refMult();
  mGRefMult = picoEvent.grefMult();

  mNTracks        = nTracksFullEvt;
  mNTracksSubEvt1 = nTracksSubEvt1;
  mNTracksSubEvt2 = nTracksSubEvt2;

  mVx       = picoEvent.primaryVertex().x();
  mVy       = picoEvent.primaryVertex().y();
  mVz       = picoEvent.primaryVertex().z();

  mKfVx     = kfFullEvent->x();
  mKfVy     = kfFullEvent->y();
  mKfVz     = kfFullEvent->z();

  if(kfSubEvt1)
  {
    mKfSubEvt1Vx     = kfSubEvt1->x();
    mKfSubEvt1Vy     = kfSubEvt1->y();
    mKfSubEvt1Vz     = kfSubEvt1->z();
  }

  if(kfSubEvt2)
  {
    mKfSubEvt2Vx     = kfSubEvt2->x();
    mKfSubEvt2Vy     = kfSubEvt2->y();
    mKfSubEvt2Vz     = kfSubEvt2->z();
  }

  mTree->Fill();
}
