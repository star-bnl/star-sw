#include "StarMuEventReader.h"
#include "StarGenerator/EVENT/StarGenEvent.h"
#include "StarGenerator/EVENT/StarGenParticle.h"
#include "TUnixTime.h"
#include "StEvtHddr.h"
//________________________________________________________________________________
Int_t StarMuEventReader::Init()
{
  TString MuDstF(SAttr("InputFile"));
  fMuDstIter = new TTreeIter();
  fMuDstIter->AddFile(MuDstF);
  return StMaker::Init();
};
// ----------------------------------------------------------------------------
StarGenParticle *StarMuEventReader::AddParticle()
{
  StarGenParticle *p = mEvent->AddParticle();
  p->SetStatus( StarGenParticle::kFinal );
  return p;
}
// ----------------------------------------------------------------------------
StarGenParticle *StarMuEventReader::AddParticle( const Char_t *type )
{
  TParticlePDG *pdg = StarParticleData::instance().GetParticle(type);  /* data(type); */ assert(pdg);
  Int_t id = pdg->PdgCode();
  StarGenParticle *p = AddParticle();
  p->SetStatus( StarGenParticle::kFinal );
  p->SetMass( pdg->Mass() );
  p->SetId( id );
  return p;
}	
//________________________________________________________________________________

Int_t StarMuEventReader::Generate() {
  return ReadEvent();
}
//________________________________________________________________________________
Int_t StarMuEventReader::ReadEvent(Int_t N) 
{
  static const Int_t*&      RunId                                    = (*fMuDstIter)("MuEvent.mEventInfo.mRunId");
  static const Int_t*&      Id                                       = (*fMuDstIter)("MuEvent.mEventInfo.mId");
  static const Int_t*&      MuEvent_mEventInfo_mTime                 = (*fMuDstIter)("MuEvent.mEventInfo.mTime"); 
  //  static const Int_t*&      MuEvent_mEventSummary_mNumberOfTracks    = (*fMuDstIter)("MuEvent.mEventSummary.mNumberOfTracks");
  static const Int_t&       NoPrimaryVertices                        = (*fMuDstIter)("PrimaryVertices");
  static const Float_t*&    PrimaryVertices_mPosition_mX1            = (*fMuDstIter)("PrimaryVertices.mPosition.mX1");
  static const Float_t*&    PrimaryVertices_mPosition_mX2            = (*fMuDstIter)("PrimaryVertices.mPosition.mX2");
  static const Float_t*&    PrimaryVertices_mPosition_mX3            = (*fMuDstIter)("PrimaryVertices.mPosition.mX3");
  static const Int_t&       NoPrimaryTracks                          = (*fMuDstIter)("PrimaryTracks");
  static const Int_t*&      PrimaryTracks_mIndex2Global              = (*fMuDstIter)("PrimaryTracks.mIndex2Global");
  static const Int_t*&      PrimaryTracks_mVertexIndex               = (*fMuDstIter)("PrimaryTracks.mVertexIndex");
  static const Float_t*&    PrimaryTracks_mP_mX1                     = (*fMuDstIter)("PrimaryTracks.mP.mX1");
  static const Float_t*&    PrimaryTracks_mP_mX2                     = (*fMuDstIter)("PrimaryTracks.mP.mX2");
  static const Float_t*&    PrimaryTracks_mP_mX3                     = (*fMuDstIter)("PrimaryTracks.mP.mX3");
  static const Short_t*&    PrimaryTracks_mHelix_mQ                  = (*fMuDstIter)("PrimaryTracks.mHelix.mQ");
  static const Int_t*&      PrimaryTracks_mNSigmaElectron            = (*fMuDstIter)("PrimaryTracks.mNSigmaElectron");
  static const Int_t*&      PrimaryTracks_mNSigmaPion                = (*fMuDstIter)("PrimaryTracks.mNSigmaPion");
  static const Int_t*&      PrimaryTracks_mNSigmaKaon                = (*fMuDstIter)("PrimaryTracks.mNSigmaKaon");
  static const Int_t*&      PrimaryTracks_mNSigmaProton              = (*fMuDstIter)("PrimaryTracks.mNSigmaProton");
  static const Int_t&       NoGlobalTracks                           = (*fMuDstIter)("GlobalTracks");
  static const Short_t*&    GlobalTracks_mFlag                       = (*fMuDstIter)("GlobalTracks.mFlag");
  static const Double_t __SIGMA_SCALE__ = 1000.;
  if (! fMuDstIter->Next()) {fStatus =  kStEOF; return fStatus;}
  if (N) return kStOk;
  Int_t id, it;
  TUnixTime ut(MuEvent_mEventInfo_mTime[0]); ut.GetGTime(id,it);
  StEvtHddr *fEvtHddr = (StEvtHddr*) StMaker::GetChain()->GetDataSet("EvtHddr");
  if (fEvtHddr) {                            // Standalone run
    fEvtHddr->SetDateTime(id,it);
    fEvtHddr->SetRunNumber(RunId[0]);
    fEvtHddr->SetEventNumber(Id[0]);
    if (Debug()) fEvtHddr->Print();
  }
  Float_t v[4];
  Float_t plab[4];
  Int_t nvtx = 0;
  Int_t ntrack = 0;
  static const Char_t *types[4][2] = {
    {"e-","e+"},
    {"K+","K-"},
    {"proton","antiproton"},
    {"pi+","pi-"}
  };
  if (Debug()) {
    cout << "NoPrimaryVertices = " << NoPrimaryVertices 
	 << "\tNoPrimaryTracks = " << NoPrimaryTracks
	 << "\tNoGlobalTracks  = " << NoGlobalTracks << endl;
  }
  for (Int_t l = 0; l < NoPrimaryVertices; l++) {
    if (l) continue; // only 1st primary vertex
    v[0] = PrimaryVertices_mPosition_mX1[l];
    v[1] = PrimaryVertices_mPosition_mX2[l];
    v[2] = PrimaryVertices_mPosition_mX3[l];
    v[3] = 0;
    nvtx++;
    for (Int_t k = 0; k < NoPrimaryTracks; k++) {
      if (l != PrimaryTracks_mVertexIndex[k]) continue;
      Int_t kg = PrimaryTracks_mIndex2Global[k];
      if (GlobalTracks_mFlag[kg] < 100) continue;
      plab[0] = PrimaryTracks_mP_mX1[k];
      plab[1] = PrimaryTracks_mP_mX2[k];
      plab[2] = PrimaryTracks_mP_mX3[k];
      Double_t nSigma[4] = {
	PrimaryTracks_mNSigmaElectron[k]/__SIGMA_SCALE__,
	PrimaryTracks_mNSigmaKaon[k]/__SIGMA_SCALE__,
	PrimaryTracks_mNSigmaProton[k]/__SIGMA_SCALE__,
	PrimaryTracks_mNSigmaPion[k]/__SIGMA_SCALE__};
      Int_t s = 0;
      if (PrimaryTracks_mHelix_mQ[k] < 0) s = 1;
      Int_t t = 3;
      Double_t nSigmaMin = 1e9;
      for (Int_t i = 0; i < 4; i++) {
	if (TMath::Abs(nSigma[i]) <  nSigmaMin) {
	  nSigmaMin = TMath::Abs(nSigma[i]); 
	  t = i;
	}
      }
      if (nSigmaMin < 2) {t = 3;}
      StarGenParticle *p = AddParticle(types[t][s]);
      //      Int_t pdg = p->GetId();
      Double_t mass = p->GetMass();
      plab[3] = TMath::Sqrt(mass*mass + plab[0]*plab[0] + plab[1]*plab[1] + plab[2]*plab[2]);
      p->SetPx( plab[0] );
      p->SetPy( plab[1] );
      p->SetPz( plab[2] );
      p->SetEnergy( plab[3] );
      
      p->SetVx( v[0] ); 
      p->SetVy( v[1] );
      p->SetVz( v[2] );
      p->SetTof( v[3] );
      ntrack++;
    }
  }
  if ( Debug() ) mEvent->Print();
  if (! nvtx || ! ntrack) return kStWarn;
  return kStOK;
};
//________________________________________________________________________________
Int_t StarMuEventReader::Skip(Int_t Nskip) {
  for (Int_t i = 0; i < Nskip; i++) {
    if (ReadEvent(1)) return kStEOF; 
  }
  return kStOK;
}
