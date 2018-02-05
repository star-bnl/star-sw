/*
  root.exe -q -b lMuDst.C MuMcKFP.C+
 */
#include "Riostream.h"
#include "TFile.h"
#include "TChain.h"
#include "TString.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StMuDSTMaker/COMMON/StMuMcVertex.h"
#include "KFVertex.h"
#include "KFParticle.h"
#include "KFPTrack.h"
#include "StKFParticleInterface/StKFParticleInterface.h"
#include "StKFParticleInterface/StKFParticlePerformanceInterface.h"

#include "TDatabasePDG.h"

#include "StMuDSTMaker/COMMON/StMuMcTrack.h"

#define ClassStMessMgr
#define StMessMgr Int_t
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#undef  StMessMgr
#undef ClassStMessMgr


#define StRootPV //St root 

int nK0=0;

void MuMcKFP(Long64_t Nevent = 999999,
             const char* file="/star/subsys/tpc/fisyak/reco/2014/V0B/*MuDst.root",
             const  char* outFile="MuMcPrV23ST.root")
{ 
  
  StKFParticleInterface mStKFParticleInterface;
  StKFParticlePerformanceInterface mStKFParticlePerformanceInterface(mStKFParticleInterface.GetTopoReconstructor(), outFile);
    
  StMuDstMaker* maker = new StMuDstMaker(0,0,"",file,"st:MuDst.root",1e9);   // set up maker in read mode
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
    "PrimaryTracks",
    "CovGlobTrack",
    "KFTracks",
    "KFVertices"
  }; 

  Int_t Nb = sizeof(ActiveBranches)/sizeof(Char_t *);

  for (Int_t i = 0; i < Nb; i++) maker->SetStatus(ActiveBranches[i],1); // Set Active braches

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
  
  for (Long64_t ev = 0; ev < nevent; ev++) {
    if (maker->Make()) break;
//     if (_debugAsk) cout << "Read event #" << ev << "\tRun\t" << StMuDst::instance()->event()->runId() << "\tId: " << StMuDst::instance()->event()->eventId() << endl;
#if 0
    Int_t referenceMultiplicity = StMuDst::instance()->event()->refMult(); // get the reference multiplicity
    if (_debugAsk) cout << " refMult= "<< referenceMultiplicity;
#endif
    
    
    TClonesArray *PrimaryVertices   = StMuDst::instance()->primaryVertices(); 
    TClonesArray *MuMcVertices   = StMuDst::instance()->mcVertices(); 
    Int_t NoMuMcVertices = StMuDst::instance()->numberOfMcVertices(); //if (_debugAsk) cout << "\t" << StMuArrays::mcArrayTypes[0] << " " << NoMuMcVertices << std::endl;
#if 0
//     std::cout << "PV indexes:  ";
    for (Int_t l = 0; l < NoMuMcVertices; l++) {
      StMuMcVertex *mcVertex = StMuDst::instance()->MCvertex(l);
      mcVertex->XyzV().x();
      mcVertex->XyzV().y();
      
      
      Int_t IdP = mcVertex->IdParTrk() ;
//       std::cout << IdP << " ";

      if (! mcVertex) continue;
//       cout << "McVx: " << *mcVertex << endl;
    }
//     std::cout << std::endl;
#endif    
    Int_t NoMuMcTracks = StMuDst::instance()->numberOfMcTracks();

    vector<KFMCTrack> mcTracks(NoMuMcTracks);

    for (Int_t k = 0; k < NoMuMcTracks; k++) {
      StMuMcTrack *mcTrack = StMuDst::instance()->MCtrack(k);
      if (! mcTrack) continue;
//       cout << "McTk: " << *mcTrack << endl;

      KFMCTrack &mcTrackKF = mcTracks[k];
      mcTrack->FillKFMCTrack(mcTrackKF);
#if 0      
      Float_t q = mcTrack->Charge();

      int pdgRoot = TDatabasePDG::Instance()->ConvertGeant3ToPdg(mcTrack->GePid());
      if(pdgRoot == 0 && q>0) pdgRoot = 211;
      if(pdgRoot == 0 && q<0) pdgRoot = -211;
      
      mcTrackKF.SetPDG( pdgRoot );
      
      float pXYZ[3] = {mcTrack->Pxyz().x(), mcTrack->Pxyz().y(), mcTrack->Pxyz().z()};
      for (Int_t i = 0; i < 3; i++) {
        mcTrackKF.SetPar( 3+i, pXYZ[i] );
      }
      mcTrackKF.SetPar(6, q/mcTrackKF.P()/3 ); // q=3q            
      mcTrackKF.SetNMCPoints(mcTrack->No_tpc_hit());
      mcTrackKF.SetMotherId(-1);
      
      Int_t IdV = mcTrack->IdVx() - 1;
      StMuMcVertex *mcVertex = StMuDst::instance()->MCvertex(IdV);
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
#endif
    }
    Int_t NoKFTracks =  StMuDst::instance()->numberOfKFTracks();
    Int_t NoKFVertices = StMuDst::instance()->numberOfKFVertices();
    if (! NoMuMcVertices || ! NoMuMcTracks) {
      cout << "Ev. " << ev << " has no MC information ==> skip it" << endl;
      continue;
    }
    
    nTracksAll += NoKFTracks;
    nStiVertex += NoKFVertices;
    
#ifdef StRootPV    
    Int_t NoPrimaryVertices = StMuDst::instance()->numberOfPrimaryVertices();  //if (_debugAsk) cout << "\tPrimaryVertices " << NoPrimaryVertices<< std::endl;
    const int NoStVertices = 1;
   //  const int NoStVertices = NoPrimaryVertices;
    vector<KFVertex> PrimVertex(NoStVertices);
    //vector<KFVertex> PrimVertex(NoPrimaryVertices);
    vector< vector<int> > PrimTracks(NoPrimaryVertices);
    
    float bestRank=-1000000;
    int bestPV=0;

    for (Int_t l = 0; l < NoPrimaryVertices; l++) {
      StMuPrimaryVertex *Vtx = StMuDst::instance()->primaryVertex(l);
//       Vtx->Print();
      if (bestRank>Vtx->ranking()) continue;
      bestRank=Vtx->ranking();
      bestPV=l;
      //convert StMuPrimaryVertex to KFVertex
      KFPVertex primVtx_tmp;
      primVtx_tmp.SetXYZ(Vtx->position().x(), Vtx->position().y(), Vtx->position().z());
      double dx = Vtx->posError().x();
      double dy = Vtx->posError().y();
      double dz = Vtx->posError().z();
      primVtx_tmp.SetCovarianceMatrix( dx*dx, 0, dy*dy, 0, 0, dz*dz );
      UShort_t noTracks = Vtx->noTracks();
      primVtx_tmp.SetNContributors(noTracks);
      primVtx_tmp.SetChi2(Vtx->chiSquared());
      vector<int> tracks;
      PrimVertex[l] = KFVertex(primVtx_tmp);
      Int_t idd = Vtx->idTruth();
      // Check Mc
      if (idd > 0 && idd <= NoMuMcVertices) {
        StMuMcVertex *mcVertex = StMuDst::instance()->MCvertex(idd-1);
        if (mcVertex->Id() != idd) {
          cout << "Mismatched idTruth " << idd << " and mcVertex Id " <<  mcVertex->Id() 
          << " The vertex is ignored" <<  endl;
        }
//      mcVertex->Print();

      }
    }
#endif


    Int_t NoGlobalTracks = StMuDst::instance()->numberOfGlobalTracks();

    std::map<int,int> trackIdMap;
    std::map<int,int> trackIdMap2;
    
    vector<KFParticle> particles(NoGlobalTracks*4);
    vector<int> mcIndexes(NoGlobalTracks*4);
    vector<int> particlesPdg(NoGlobalTracks*4);
    int nPartSaved = 0;
    for (Int_t kg = 0; kg < NoGlobalTracks; kg++) {
      StMuTrack *gTrack = StMuDst::instance()->globalTracks(kg);
      if (! gTrack)            continue;

      //if (! gTrack->idTruth()) return kFALSE;
//       if (! gTrack->charge())  continue;
//       if (  gTrack->flag() < 100 ||  gTrack->flag()%100 == 11) continue; // bad fit or short track pointing to EEMC
//       if (  gTrack->flag() > 1000) continue;  // pile up track in TPC
//       if (  gTrack->nHitsFit() < 10) continue;
  //  if (  gTrack->qaTruth() < 90) return kFALSE;

  
      Int_t kgc = gTrack->index2Cov();
      if (kgc < 0) continue;
      StDcaGeometry *dcaG = StMuDst::instance()->covGlobTracks(kgc);
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
      

      
      trackIdMap[gTrack->id()] = nPartSaved;
      trackIdMap2[nPartSaved] = gTrack->id();
      
#ifdef StRootPV    
      bool isSecondary = true;
      const StMuTrack *primTrack = gTrack->primaryTrack();
      if(primTrack)
      {
        const int iPV = primTrack->vertexIndex(); 
//         //continue;
//          //if ((iPV!=bestPV)) continue;
//         {
//           vector<int> &tracksPV = PrimTracks[iPV];
//           tracksPV.push_back(nPartSaved);
//         }
         
        if ( iPV==bestPV )
        {
          KFParticle particle(track, pdg);
          particle.SetId(kg);
          
          particles[nPartSaved] = particle;
          mcIndexes[nPartSaved] = gTrack->idTruth()-1;
          
          if(mcIndexes[nPartSaved] > -1)
          {
            particlesPdg[nPartSaved] = mcTracks[mcIndexes[nPartSaved]].PDG();
            mcTracks[mcIndexes[nPartSaved]].SetReconstructed();
          }
          else
          {
            particlesPdg[nPartSaved] = -1;
          }
          nPartSaved++;
          isSecondary = false;
        }
      }
      
      if(isSecondary)
      {
        int pdg[3] = {211, 2212, -11};
        if(q<0)
        {
          pdg[0] = -211;
          pdg[1] = -2212;
          pdg[2] = 11;
        }
        
        for(int iHypo=0; iHypo<3; iHypo++)
        {
          KFParticle particle(track, pdg[iHypo]);
          particle.SetId(kg);
          
          particles[nPartSaved] = particle;
          mcIndexes[nPartSaved] = gTrack->idTruth()-1;
          
          if(mcIndexes[nPartSaved] > -1)
            mcTracks[mcIndexes[nPartSaved]].SetReconstructed();

          particlesPdg[nPartSaved] = pdg[iHypo];
          nPartSaved++;
        }
      }
#else
      KFParticle particle(track, pdg);
      particle.SetId(kg);
      
      particles[nPartSaved] = particle;
      mcIndexes[nPartSaved] = gTrack->idTruth()-1;
      
      if(mcIndexes[nPartSaved] > -1)
      {
        particlesPdg[nPartSaved] = mcTracks[mcIndexes[nPartSaved]].PDG();
        mcTracks[mcIndexes[nPartSaved]].SetReconstructed();
      }
      else
      {
        particlesPdg[nPartSaved] = -1;
      }
      nPartSaved++;
#endif
    }

    particles.resize(nPartSaved);
    mcIndexes.resize(nPartSaved);
    particlesPdg.resize(nPartSaved);

    const Double_t field = StMuDst::instance()->event()->magneticField();

    mStKFParticleInterface.SetField(field);
    if(NoKFTracks > 0)
      mStKFParticleInterface.SetBeamLine( *(StMuDst::instance()->KFtrack(0)));
    
    mStKFParticleInterface.SetParticles(particles);
    mStKFParticleInterface.SetParticlesPdg(particlesPdg);

#ifdef StRootPV

    mStKFParticleInterface.InitParticles();
    if(NoPrimaryVertices>0)
    {
      for(int iPV=0; iPV<NoPrimaryVertices; iPV++)
      {
        if (iPV==bestPV)
          mStKFParticleInterface.AddPV(PrimVertex[iPV], PrimTracks[iPV]);
      }
    }
    else
    {
      KFPVertex primVtx_tmp;
      primVtx_tmp.SetXYZ(0, 0, 0);
      primVtx_tmp.SetCovarianceMatrix( 0, 0, 0, 0, 0, 0 );
      primVtx_tmp.SetNContributors(0);
      primVtx_tmp.SetChi2(-100);

      vector<int> tracks;
      KFVertex pv(primVtx_tmp);
      mStKFParticleInterface.AddPV(pv, tracks);
    }
#if 0 /* Maksym reconstruction */
    mStKFParticleInterface.ReconstructParticles();
#else
    for(UInt_t iPart=0; iPart<particles.size(); iPart++)
    {
      particles[iPart].SetId(iPart);
      particles[iPart].AddDaughterId(iPart);
      mStKFParticleInterface.AddParticle(particles[iPart]);
    }
    
    for (Int_t l = 0; l < NoKFVertices; l++)
    {
      KFVertex *vx = StMuDst::instance()->KFvertex(l);
      if (! vx) continue;
      //if( vx->GetNDF() != 1 ) continue;
            
      KFParticle particle = *vx;

      if(particle.NDaughters() != 2) continue;
      Int_t np = particles.size();
      if(particle.DaughterIds()[0] >= np || particle.DaughterIds()[1] >= np ) 
        continue;

      vector<int> newIds;
      newIds.push_back( trackIdMap[particle.DaughterIds()[0]]);
      newIds.push_back( trackIdMap[particle.DaughterIds()[1]]);
            
      particle.CleanDaughtersId();
      particle.AddDaughterId(newIds[0]);
      particle.AddDaughterId(newIds[1]);
      
      mStKFParticleInterface.AddParticle(particle);
      if(vx->GetNDF() > 2)
        mStKFParticleInterface.AddCandidate(particle, 0);
      else if(vx->GetNDF() > 1)
        mStKFParticleInterface.AddCandidate(particle, 0);
    }
#endif
#else
    mStKFParticleInterface.ReconstructTopology();
#endif //StRootPV
    

    mStKFParticlePerformanceInterface.SetMCTracks(mcTracks);
    mStKFParticlePerformanceInterface.SetMCIndexes(mcIndexes);    
    mStKFParticlePerformanceInterface.SetPrintEffFrequency(nevent);
    mStKFParticlePerformanceInterface.PerformanceAnalysis();
  }
  
  double GhostTracksRate = double(nTracksGhost)/double(nTracksAll);
  
  std::cout << "Ghost Tracks: N = " <<nTracksGhost << " All tracks: " <<  nTracksAll << " Ghost Rate: " << GhostTracksRate << std::endl;
  std::cout << "nKFVertex " << nKFVertex << " nStiVertex "  << nStiVertex << std::endl;
  // Count no. track at a vertex with TPC reconstructable traks.
  //  fOut->Write();
}
