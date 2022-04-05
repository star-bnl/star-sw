#include "StTofMuDstReader.h"

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

#include "StTriggerIdCollection.h"
#include "StTriggerId.h"

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuTofHit.h"

#include "StTofMuDstEval.h"
#include "tofHitTree.h"



//_________________________________________________
StTofMuDstReader::StTofMuDstReader(const char *name, const char *file, StMuDstMaker* maker):StMaker(name)
{
  //  StTofMuDstReader Constructor
  // - zero all pointers defined in the header file
  mMuDstMaker = maker;
  mOutputFile = new TFile(file,"RECREATE");
  mOutputFile->SetCompressionLevel(1);
  int BufSize=(int)pow(2.,16.);
  int Split=1; 
  mTree = new TTree("T","T",BufSize);
  mTree->SetAutoSave(10000); // autosave every 10 Mbytes
  mTree->Branch("runId",&mTofHit.runId,"runId/I");
  mTree->Branch("evtId",&mTofHit.evtId,"evtId/I");
  mTree->Branch("vz",&mTofHit.vz,"vz/F");
  mTree->Branch("mult",&mTofHit.mult,"mult/I");
  mTree->Branch("neast",&mTofHit.neast,"neast/I");
  mTree->Branch("nwest",&mTofHit.nwest,"nwest/I");  
  mTree->Branch("module",&mTofHit.module,"module/I");
  mTree->Branch("cell",&mTofHit.cell,"cell/I");
  mTree->Branch("tot",&mTofHit.tot,"tot/F");
  mTree->Branch("tof",&mTofHit.tof,"tof/F");
  mTree->Branch("L",&mTofHit.L,"L/F");
  mTree->Branch("beta",&mTofHit.beta,"beta/F");
  mTree->Branch("localY",&mTofHit.localY,"localY/F");
  mTree->Branch("localZ",&mTofHit.localZ,"localZ/F");
  mTree->Branch("deltaY",&mTofHit.deltaY,"deltaY/F");
  mTree->Branch("m2",&mTofHit.m2,"m2/F");
  mTree->Branch("trkId",&mTofHit.trkId,"trkId/I");
  mTree->Branch("nHitsFit",&mTofHit.nHitsFit,"nHitsFit/I");
  mTree->Branch("pt",&mTofHit.pt,"pt/F");
  mTree->Branch("eta",&mTofHit.eta,"eta/F");
  mTree->Branch("phi",&mTofHit.phi,"phi/F");
  mTree->Branch("dca",&mTofHit.dca,"dca/F");
  mTree->Branch("dEdx",&mTofHit.dEdx,"dEdx/F");
  mTree->Branch("nSigmaPi",&mTofHit.nSigmaPi,"nSigmaPi/F");
  mTree->Branch("nSigmaK",&mTofHit.nSigmaK,"nSigmaK/F");
  mTree->Branch("nSigmaP",&mTofHit.nSigmaP,"nSigmaP/F");
  mTree->Branch("nSigmaE",&mTofHit.nSigmaE,"nSigmaE/F");
  mTree->Branch("nSigmaMu",&mTofHit.nSigmaMu,"nSigmaMu/F");
  mTree->Branch("nSigmaTofPi",&mTofHit.nSigmaTofPi,"nSigmaTofPi/F");
  mTree->Branch("nSigmaTofK",&mTofHit.nSigmaTofK,"nSigmaTofK/F");
  mTree->Branch("nSigmaTofP",&mTofHit.nSigmaTofP,"nSigmaTofP/F");
  mTree->Branch("nSigmaTofE",&mTofHit.nSigmaTofE,"nSigmaTofE/F");
  eventNumber=0;
  oldEventRunId=0;
}

//_________________________________________________
StTofMuDstReader::~StTofMuDstReader()
{
}

//_____________________________________________________________________________

void StTofMuDstReader::Clear(const char*)
{
  mTofHit.runId = -1;
  mTofHit.evtId = -1;
  mTofHit.vz = -999.;
  mTofHit.mult = -999;
  mTofHit.neast = 0;
  mTofHit.nwest = 0;
  mTofHit.module = 0;
  mTofHit.cell = 0;
  mTofHit.tot = -999.;
  mTofHit.tof = -999.;
  mTofHit.L = -999.;
  mTofHit.beta = -999.;
  mTofHit.localY = -999;
  mTofHit.localZ = -999.;
  mTofHit.deltaY = -999.;
  mTofHit.m2 = -999.;
  mTofHit.trkId = -1;
  mTofHit.nHitsFit = 0;
  mTofHit.pt = -999.;
  mTofHit.eta = -999.;
  mTofHit.phi = -999.;
  mTofHit.dca = -999.;
  mTofHit.dEdx = -999.;
  mTofHit.nSigmaPi = -999.;
  mTofHit.nSigmaK = -999.;
  mTofHit.nSigmaP = -999.;
  mTofHit.nSigmaE = -999.;
  mTofHit.nSigmaMu = -999.;
  mTofHit.nSigmaTofPi = -999.;
  mTofHit.nSigmaTofK = -999.;
  mTofHit.nSigmaTofP = -999.;
  mTofHit.nSigmaTofE = -999.;

  StMaker::Clear();
}

//_________________________________________________
Int_t StTofMuDstReader::Finish()
{
  mOutputFile->Write();
  mOutputFile->Close();
  return StMaker::Finish();
}


//_________________________________________________
Int_t StTofMuDstReader::Init()
{
  Clear("C");
  return kStOK;
}
//_________________________________________________
Int_t StTofMuDstReader::Make()
{
  cout << "StTofMuDstReader::Make()" << endl;

  if( mMuDstMaker != NULL ) {
    mMuDst = mMuDstMaker->muDst();
  }

  if(!mMuDst) return kStOK;

  StTofMuDstEval *tofEval = (StTofMuDstEval *)GetMaker("tofMuDstEval");
  if(!tofEval) return kStOK;
  
  StMuPrimaryVertex *vtx = mMuDst->primaryVertex();
  if(!vtx) return kStOK;

  StMuEvent *muEvent = mMuDst->event();
  if(!muEvent) return kStOK;

  StThreeVectorF vertexPos = muEvent->primaryVertexPosition();
  if(fabs(vertexPos.x())<1.0e-5 && fabs(vertexPos.y())<1.0e-5 && fabs(vertexPos.z())<1.0e-5) return kStOK;

  int runId = muEvent->runId();
  int evtId = muEvent->eventId();
  int refMult = muEvent->refMultPos() + muEvent->refMultNeg();

  int nEast = 0;
  int nWest = 0;
  tofEval->GetPvpdNHits(nEast , nWest);
  cout << " nEast = " << nEast << " nWest = " << nWest << endl;

  int nTofHits = mMuDst->numberOfTofHit();
  int n = mMuDst->numberOfPrimaryTracks();
  cout << " There are " << nTofHits << " TofHits in this event." << endl;
  for(int i=0;i<nTofHits;i++) {
    StMuTofHit *tof = (StMuTofHit*)mMuDst->tofHit(i);
    if(!tof) continue;
    int trkId = tof->associatedTrackId();
    int index = -1;
    for(int j=0;j<n;j++) {
      StMuTrack* t = (StMuTrack*)mMuDst->primaryTracks(j);
      if(!t) continue;
      if(trkId==t->id()) {
	index = j;
	break;
      }
    } // end track loop
    if(index<0||index>=n) {
      cout << "Warning! no matched TPC track, strange!" << endl;
      continue;
    }
    StMuTrack *trk = (StMuTrack*)mMuDst->primaryTracks(index);
    if(!trk) continue;

    Double_t local[3];
    tofEval->GetLocalHitPosition(tof, &local[0]);
    Double_t yCenter = (tof->cellIndex()-1-2.5)*3.45;

    float m2 = pow(trk->pt()*TMath::CosH(trk->eta()),2.0)*(1./tof->beta()/tof->beta()-1.);

    mTofHit.runId = runId;
    mTofHit.evtId = evtId;
    mTofHit.vz = vertexPos.z();
    mTofHit.mult = refMult;
    mTofHit.neast = nEast;
    mTofHit.nwest = nWest;
    mTofHit.module = tof->moduleIndex();
    mTofHit.cell = tof->cellIndex();
    mTofHit.tot = tofEval->GetUncorrectedTot(tof);
    mTofHit.tof = tof->timeOfFlight();
    mTofHit.L = tof->pathLength();
    mTofHit.beta = tof->beta();
    mTofHit.localY = local[1];
    mTofHit.localZ = local[2];
    mTofHit.deltaY = local[1]-yCenter;
    mTofHit.m2 = m2;
    mTofHit.trkId = index;
    mTofHit.nHitsFit = trk->nHitsFit(kTpcId)*trk->charge();
    mTofHit.pt = trk->pt();
    mTofHit.eta = trk->eta();
    mTofHit.phi = trk->phi();
    mTofHit.dca = abs(trk->dcaGlobal());
    mTofHit.dEdx = trk->dEdx()*1.e+6;
    mTofHit.nSigmaPi = trk->nSigmaPion();
    mTofHit.nSigmaK = trk->nSigmaKaon();
    mTofHit.nSigmaP = trk->nSigmaProton();
    mTofHit.nSigmaE = trk->nSigmaElectron();
    mTofHit.nSigmaMu = -999.;
    mTofHit.nSigmaTofPi = tof->sigmaPion()/2.;
    mTofHit.nSigmaTofK = tof->sigmaKaon()/2.;
    mTofHit.nSigmaTofP = tof->sigmaProton()/2.;
    mTofHit.nSigmaTofE = tof->sigmaElectron()/2.;

    mTree->Fill();

  }

  
  
  return kStOK;
}
