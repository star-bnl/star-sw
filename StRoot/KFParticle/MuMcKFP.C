/* 
   root.exe -q -b lMuDstMZ.C MuMcKFP.C+; 
*/
//#define  __TMVA__
#if !defined(__CINT__) || defined(__MAKECINT__)

//#define KFPARTICLE
#define STIPV

#include <assert.h>
#include <map>
#include <utility>
#include <cstdlib>
#include <string>
#include "Riostream.h"
#include "Rtypes.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TF1.h"
#include "TProfile.h"
#include "TProfile3D.h"
#include "TTree.h"
#include "TNtuple.h"
#include "TChain.h"
#include "TCanvas.h"
#include "TLegend.h"
#include "TClassTable.h"
#include "TFile.h"
#include "TChain.h"
#include "TString.h"
#include "TObjString.h"
#include "TArrayF.h"
#include "TArrayD.h"
#include "SystemOfUnits.h"
#include "StMuDSTMaker/COMMON/StMuTimer.h"
#include "StMuDSTMaker/COMMON/StMuDebug.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StMuDSTMaker/COMMON/StMuMcVertex.h"
#include "KFParticle/KFVertex.h"
#include "KFParticle/KFParticle.h"

#include "KFParticle/KFParticleTopoReconstructor.h"
#if 1
#include "TPCCATrackerPerformance/AliHLTTPCCAPerformance.h"
#include "TPCCATrackerPerformance/AliHLTTPCCAMCTrack.h"
#endif
#include "TDatabasePDG.h"

#include "KFParticle/KFPVertex.h"

#ifndef __RC__
#include "StMuDSTMaker/COMMON/StMuMcTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryTrackCovariance.h"
#endif /* !__RC__ */
#include "StBTofHeader.h"
#include "StDcaGeometry.h"
#include "TRSymMatrix.h"
#include "THelixTrack.h"
// #include "Names.h"
#include "StBichsel/Bichsel.h"
#define ClassStMessMgr
#define StMessMgr Int_t
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#undef  StMessMgr
#undef ClassStMessMgr
#else /* defined(__CINT__) && ! defined(__MAKECINT__) */
#ifndef __MAKECINT__
#define BIT(n)       (1 << (n))
#define SETBIT(n,i)  ((n) |= (1 << i))
#define CLRBIT(n,i)  ((n) &= ~(1 << i))
#define TESTBIT(n,i) ((Bool_t)(((n) & (1 << i)) != 0))
class StMuDstMaker;
#endif /* __MAKECINT__ */
#endif /* !defined(__CINT__) || defined(__MAKECINT__) */
#include "Ask.h"
StMuDstMaker* maker = 0;
//________________________________________________________________________________

void MuMcKFP(Long64_t Nevent = 999999,
	     const char* file="./*.MuDst.root",
	     const  char* outFile="MuMcPrV23") { 
  TString OutFile(outFile);
  StMuDebug::setLevel(0);  
  maker = new StMuDstMaker(0,0,"",file,"st:MuDst.root",1e9);   // set up maker in read mode
  //                       0,0                        this mean read mode
  //                           dir                    read all files in this directory
  //                               file               bla.lis real all file in this list, if (file!="") dir is ignored
  //                                    filter        apply filter to filenames, multiple filters are separated by ':'
  //                                          10      maximum number of file to read
  maker->SetStatus("*",0);
  const Char_t *ActiveBranches[] = {
    "MuEvent",
    "PrimaryVertices",
    "StStMuMcVertex","StStMuMcTrack",
    "GlobalTracks",
    "CovGlobTrack",
    "KFTracks",
    "KFVertices"
  }; 
  Int_t Nb = sizeof(ActiveBranches)/sizeof(Char_t *);
  for (Int_t i = 0; i < Nb; i++) maker->SetStatus(ActiveBranches[i],1); // Set Active braches
  StMuDebug::setLevel(0);  
  TChain *tree = maker->chain();
  Long64_t nentries = tree->GetEntries();
  Long64_t nevent = TMath::Min(Nevent,nentries);
  cout << nentries << " events in chain " << nevent << " will be read." << endl;
  //  if (nentries < 100) return;
  tree->SetCacheSize(-1);        //by setting the read cache to -1 we set it to the AutoFlush value when writing
  tree->SetCacheLearnEntries(1); //one entry is sufficient to learn
  tree->SetCacheEntryRange(0,nevent);

  
  int nTracksAll = 0;
  int nTracksGhost = 0;
  int nStiVertex = 0;
  int nKFVertex = 0;
#if 1
  AliHLTTPCCAPerformance *perf = 0;
  perf = &AliHLTTPCCAPerformance::Instance();
  TFile* perfHistoFile = new TFile("HLTTPCCATrackerPerformance.root","RECREATE");
  if ( perf )     perf->SetOutputFile(perfHistoFile);
#endif  
  
  for (Long64_t ev = 0; ev < nevent; ev++) {
    if (maker->Make()) break;
    StMuDst* mu = maker->muDst();   // get a pointer to the StMuDst class, the class that points to all the data
    StMuEvent* muEvent = mu->event(); // get a pointer to the class holding event-wise information
    if (_debugAsk) cout << "Read event #" << ev << "\tRun\t" << muEvent->runId() << "\tId: " << muEvent->eventId() << endl;
#if 0
    Int_t referenceMultiplicity = muEvent->refMult(); // get the reference multiplicity
    if (_debugAsk) cout << " refMult= "<< referenceMultiplicity;
#endif
    
    
    TClonesArray *PrimaryVertices   = mu->primaryVertices(); 
    TClonesArray *MuMcVertices   = mu->mcArray(0); 
    Int_t NoMuMcVertices = MuMcVertices->GetEntriesFast(); if (_debugAsk) cout << "\t" << StMuArrays::mcArrayTypes[0] << " " << NoMuMcVertices << std::endl;
    for (Int_t l = 0; l < NoMuMcVertices; l++) {
      StMuMcVertex *mcVertex = (StMuMcVertex *) MuMcVertices->UncheckedAt(l);
      if (! mcVertex) continue;
//       cout << "McVx: " << *mcVertex << endl;
    }
    
    TClonesArray *MuMcTracks     = mu->mcArray(1); 
    Int_t NoMuMcTracks = MuMcTracks->GetEntriesFast(); if (_debugAsk) cout << "\t" << StMuArrays::mcArrayTypes[1] << " " << NoMuMcTracks<< std::endl;

    vector<AliHLTTPCCAMCTrack> mcTracks(NoMuMcTracks);

    for (Int_t k = 0; k < NoMuMcTracks; k++) {
      StMuMcTrack *mcTrack = (StMuMcTrack *) MuMcTracks->UncheckedAt(k);
      if (! mcTrack) continue;
//       cout << "McTk: " << *mcTrack << endl;

      AliHLTTPCCAMCTrack &mcTrackKF = mcTracks[k];
      
      Float_t q = mcTrack->Charge();

      int pdgRoot = TDatabasePDG::Instance()->ConvertGeant3ToPdg(mcTrack->GePid());
      if(pdgRoot == 0 && q>0) pdgRoot = 211;
      if(pdgRoot == 0 && q<0) pdgRoot = -211;
      
      mcTrackKF.SetPDG( pdgRoot );
      
//       TParticlePDG *part = TDatabasePDG::Instance()->GetParticle( mcTrackKF.PDG() );
//       Float_t      q     = part->Charge();

      mcTrackKF.SetP ( mcTrack->Ptot() );
      mcTrackKF.SetPt( mcTrack->pT() );
        //      mcTrackKF.SetNHits( mcTrack->n_tpc_hit ); // it is not really what we need. See below
      float pXYZ[3] = {mcTrack->Pxyz().x(), mcTrack->Pxyz().y(), mcTrack->Pxyz().z()};
      for (Int_t i = 0; i < 3; i++) {
        mcTrackKF.SetPar( 3+i, pXYZ[i]/mcTrackKF.P() );
      }
      mcTrackKF.SetPar(6, q/mcTrackKF.P()/3 ); // q=3q      
      mcTrackKF.SetNTurns( 1 ); // TODO: read from somewhere
      
      mcTrackKF.SetNMCPoints(mcTrack->No_tpc_hit());

      mcTrackKF.SetMotherId(-1);
      
      Int_t IdV = mcTrack->IdVx() - 1;
      StMuMcVertex *mcVertex = (StMuMcVertex *) MuMcVertices->UncheckedAt(IdV);
      Int_t IdP = mcVertex->IdParTrk() - 1;
      if( IdP < 0 ) IdP = -(IdV+1);
      mcTrackKF.SetMotherId(IdP);

      float vXYZ[3] = {mcVertex->XyzV().x(), mcVertex->XyzV().y(), mcVertex->XyzV().z()};

      for (Int_t i = 0; i < 3; i++) { // TODO

        mcTrackKF.SetPar( i,  vXYZ[i] );
      }
// int motherPDG = -1;
// if(IdP >= 0 )
//   motherPDG = TDatabasePDG::Instance()->ConvertGeant3ToPdg( ((StMuMcTrack *) MuMcTracks->UncheckedAt(IdP))->GePid())    ;
// std::cout << "Id " << k+1 <<"  Pdg " << mcTrackKF.PDG() << " motherPDG " << motherPDG << " MotherId " << (mcVertex->IdParTrk() - 1) << std::endl;
    }
    
    TClonesArray *KFTracks = mu->KFTracks();
    Int_t NoKFTracks = KFTracks->GetEntriesFast();                cout << "\tKFTracks " << NoKFTracks;
    TClonesArray *KFVertices = mu->KFVertices();
    Int_t NoKFVertices = KFVertices->GetEntriesFast();            cout << "\tKFVertices " << NoKFVertices;
    //    const Double_t field = muEvent->magneticField()*kilogauss;
    if (! NoMuMcVertices || ! NoMuMcTracks) {
      cout << "Ev. " << ev << " has no MC information ==> skip it" << endl;
      continue;
    }
    
    nTracksAll += NoKFTracks;
    nStiVertex += NoKFVertices;
    
    Int_t NoPrimaryVertices = PrimaryVertices->GetEntriesFast();  if (_debugAsk) cout << "\tPrimaryVertices " << NoPrimaryVertices<< std::endl;
    for (Int_t l = 0; l < NoPrimaryVertices; l++) {
      StMuPrimaryVertex *Vtx = (StMuPrimaryVertex *) PrimaryVertices->UncheckedAt(l);
//       Vtx->Print();
      UShort_t noTracks = Vtx->noTracks();
      if (! noTracks) continue;
      Int_t idd = Vtx->idTruth();
      // Check Mc
      if (idd > 0 && idd <= NoMuMcVertices) {
        StMuMcVertex *mcVertex = (StMuMcVertex *) MuMcVertices->UncheckedAt(idd-1);
        if (mcVertex->Id() != idd) {
          cout << "Mismatched idTruth " << idd << " and mcVertex Id " <<  mcVertex->Id() 
          << " The vertex is ignored" <<  endl;
        }
// 	mcVertex->Print();
      }
    }
    
#ifdef STIPV
    vector<KFVertex> PrimVertex(NoKFVertices);
    vector< vector<short int> > PrimTracks(NoKFVertices);
    map<int,int> pvIndexMap;
#endif //STIPV
    for (Int_t l = 0; l < NoKFVertices; l++) {
      KFVertex *vx = (KFVertex *) KFVertices->UncheckedAt(l);
      if (! vx) continue;
#ifdef STIPV
      KFVertex &pv = PrimVertex[l];      
      pv = *vx;
      pvIndexMap[vx->GetID()] = l;
#endif //STIPV
//       cout << "KFV:" << *vx << endl;
    }
//     cout << "========================================" << endl;
    
#ifdef STIPV
    vector<KFParticle> particles(NoKFTracks);
    vector<int> mcIndexes(NoKFTracks, -1);

    for (Int_t k = 0; k < NoKFTracks; k++) {
       KFParticle *tk = (KFParticle *) KFTracks->UncheckedAt(k);
       particles[k] = *tk;
       if (! tk) continue;
       mcIndexes[k] = tk->IdTruth()-1;
//        if(mcIndexes[k]<0) nTracksGhost++;
//        cout << "KFP:" << *tk << endl;
       int vertexID = tk->GetParentID();
       if(vertexID > 0)
       {
         std::map<int, int>::iterator it;
         it=pvIndexMap.find(vertexID);
         if(it != pvIndexMap.end())
         {
           vector<short int> &tracksPV = PrimTracks[pvIndexMap[vertexID]];
           tracksPV.push_back(k);
         }
       }
    }
#endif

    if(NoKFTracks < 1) continue;
    
#ifndef STIPV
    TClonesArray *GlobalTracks     = mu->array(muGlobal);  
    Int_t NoGlobalTracks = GlobalTracks->GetEntriesFast();     //   {cout << "\tGlobalTracks " << NoGlobalTracks;}
    TClonesArray *CovPrimTrack     = mu->covPrimTrack();       //   {cout << "\tCovPrimTrack " << CovPrimTrack->GetEntriesFast();}
    TClonesArray *CovGlobTrack     = mu->covGlobTrack();       //   {cout << "\tCovGlobTrack " << CovGlobTrack->GetEntriesFast();}


    vector<KFParticle> particles(NoGlobalTracks);
    vector<int> mcIndexes(NoGlobalTracks);
    vector<int> particlesPdg(NoGlobalTracks);
    int nPartSaved = 0;
    for (Int_t kg = 0; kg < NoGlobalTracks; kg++) {
      StMuTrack *gTrack = (StMuTrack *) GlobalTracks->UncheckedAt(kg);
      
      if (! gTrack)            continue;
      //if (! gTrack->idTruth()) return kFALSE;
      if (! gTrack->charge())  continue;
      if (  gTrack->flag() < 100 ||  gTrack->flag()%100 == 11) continue; // bad fit or short track pointing to EEMC
      if (  gTrack->flag() > 1000) continue;  // pile up track in TPC
      if (  gTrack->nHitsFit() < 10) continue;
  //  if (  gTrack->qaTruth() < 90) return kFALSE;
  
      Int_t kgc = gTrack->index2Cov();
      if (kgc < 0) continue;
      StDcaGeometry *dcaG = (StDcaGeometry *) CovGlobTrack->UncheckedAt(kgc);
      if (! dcaG) continue;
//       cout << "dcaG:" <<  *dcaG << endl;
      Double_t xyzp[6], CovXyzp[21];
      dcaG->GetXYZ(xyzp,CovXyzp);
      static KFPTrack track;
      track.SetParameters(xyzp);
      track.SetCovarianceMatrix(CovXyzp);
      track.SetNDF(1);
      //    track.SetChi2(GlobalTracks_mChiSqXY[k]);
      track.SetID(kg);
      Int_t q   = 1;
      Int_t pdg = 211;
      if (dcaG->charge() < 0) {
        q = -1;
        pdg = -211;
      } 
      track.SetCharge(q);
      
      KFParticle particle(track, pdg);
      particle.SetID(kg);
      particle.SetIdTruth(gTrack->idTruth(),gTrack->qaTruth());
//        cout << "GB " << particle << endl;
      
      particles[nPartSaved] = particle;
      mcIndexes[nPartSaved] = particle.IdTruth()-1;
      
      if(mcIndexes[nPartSaved] > -1)
      {
        particlesPdg[nPartSaved] = mcTracks[mcIndexes[nPartSaved]].PDG();
      }
      else
      {
        particlesPdg[nPartSaved] = -1;
      }
      
      nPartSaved++;
    }
    particles.resize(nPartSaved);
    mcIndexes.resize(nPartSaved);
    particlesPdg.resize(nPartSaved);
#endif
    const Double_t field = muEvent->magneticField();
//     std::cout << "Field!!!!!!!     " << field << std::endl;
    KFParticleTopoReconstructor* topoReconstructor = new KFParticleTopoReconstructor(); // TODO don't recreate with each event
    if ( perf ) {
      perf->SetTopoReconstructor(topoReconstructor);
      perf->SetMCTracks(mcTracks);
    }

    TStopwatch timer;
    timer.Start();
  
    topoReconstructor->SetField(field);
//     topoReconstructor->Init( particles, &particlesPdg );
       topoReconstructor->Init( particles );

#ifdef STIPV
    if(NoKFVertices>0)
    {
      for(int iPV=0; iPV<NoKFVertices; iPV++)
      {
        topoReconstructor->AddPV(PrimVertex[iPV], PrimTracks[iPV]);
      }
    }
    else
    {
      KFPVertex primVtx_tmp;
      primVtx_tmp.SetXYZ(0, 0, 0);
      primVtx_tmp.SetCovarianceMatrix( 0, 0, 0, 0, 0, 0 );
      primVtx_tmp.SetNContributors(0);
      primVtx_tmp.SetChi2(-100);

      vector<short int> tracks;
      KFVertex pv(primVtx_tmp);
      topoReconstructor->AddPV(pv, tracks);
    }
#else //STIPV
    topoReconstructor->SetBeamLine( *((KFParticle *) KFTracks->UncheckedAt(0)) );
    topoReconstructor->ReconstructPrimVertex();
#endif //STIPV
    //    topoReconstructor->ReconstructParticles(0);
    topoReconstructor->ReconstructParticles();
    timer.Stop();
//   topoReconstructor->SetTime(timer.RealTime());
    nKFVertex += topoReconstructor->NPrimaryVertices();
#if 1
    if ( perf ) {
      perf->InitSubPerformances();
      perf->SetRecoData(mcIndexes);
      perf->GetSubPerformance("Topo Performance")->Exec(0);
    }
#endif
    if(topoReconstructor) delete topoReconstructor;
  }

#if 1
// #ifdef DO_TPCCATRACKER_EFF_PERFORMANCE
//   if ( AliHLTTPCCAPerformance::Instance().GetSubPerformance("Global Performance")->fMCTracks->Size() > 0 )
//   for(unsigned int iTr=0; iTr<tmpTracks.size(); iTr++)
//   {
//     int iMC = AliHLTTPCCAPerformance::Instance().GetSubPerformance("Global Performance")->GetRecoData()[tmpTracks[iTr].Id()].GetMCTrackId();
//     int PDG = (*AliHLTTPCCAPerformance::Instance().GetSubPerformance("Global Performance")->fMCTracks)[iMC].PDG();
//     vTrackPDG[iTr] = PDG;
//   }
// #endif
  if ( perf) {
    perf->WriteHistos();
  }
  perfHistoFile->Close();
#endif  
  double GhostTracksRate = double(nTracksGhost)/double(nTracksAll);
  
  std::cout << "Ghost Tracks: N = " <<nTracksGhost << " All tracks: " <<  nTracksAll << " Ghost Rate: " << GhostTracksRate << std::endl;
  std::cout << "nKFVertex " << nKFVertex << " nStiVertex "  << nStiVertex << std::endl;
  // Count no. track at a vertex with TPC reconstructable traks.
  //  fOut->Write();
}
