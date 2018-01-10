//----------------------------------------------------------------------------
// Implementation of the KFParticle class
// .
// @author  I.Kisel, I.Kulakov, M.Zyzak
// @version 1.0
// @since   20.08.13
// 
// 
//  -= Copyright &copy ALICE HLT and CBM L1 Groups =-
//____________________________________________________________________________

#ifdef DO_TPCCATRACKER_EFF_PERFORMANCE

#include "KFTopoPerformance.h"

#include "AliHLTTPCCounters.h"

#include "AliHLTTPCCATrackPerformanceBase.h"
#include "AliHLTTPCCAPerformance.h"
#include "AliHLTTPCCAGlobalPerformance.h"

#ifdef MAIN_DRAW
#include "AliHLTTPCCADisplay.h"
#endif //DRAW

#include "AliHLTTPCCAGBHit.h"
#include "AliHLTTPCCAMCTrack.h"
#include "AliHLTTPCCAMCPoint.h"
#include "AliHLTTPCCAOutTrack.h"
#include "AliHLTTPCCAGBTrack.h"
#include "AliHLTTPCCAGBTracker.h"
#include "AliHLTTPCCAGBTracker.h"
#include "KFParticleTopoReconstructor.h"

#include "AliHLTTPCCATracker.h"

#include "AliHLTTPCCADisplay.h"

#include "KFParticleSIMD.h"
#include "TParticlePDG.h"
#include "TDatabasePDG.h"

#include "TMath.h"
#include "TROOT.h"
#include "Riostream.h"
#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "TStyle.h"

#include <algorithm>
using std::sort;

KFTopoPerformance::KFTopoPerformance():fTopoReconstructor(0),fPrimVertices()
{
}

KFTopoPerformance::~KFTopoPerformance()
{
}

void KFTopoPerformance::SetNewEvent(
                            const AliHLTTPCCAGBTracker * const tracker,
                            AliHLTResizableArray<AliHLTTPCCAHitLabel> *hitLabels,
                            AliHLTResizableArray<AliHLTTPCCAMCTrack> *mcTracks,
                            AliHLTResizableArray<AliHLTTPCCALocalMCPoint> *localMCPoints)
{
  assert( AliHLTTPCCAPerformance::Instance().GetSubPerformance("Global Performance") != 0 );
  
  KFParticlePerformanceBase::SetNewEvent(tracker, hitLabels, mcTracks, localMCPoints);

  if(fTracker)
    nRecoTracks = fTracker->NTracks();
  else
    nRecoTracks = 0;
} // void KFTopoPerformance::SetNewEvent

void KFTopoPerformance::SetNewEvent2( const KFParticleTopoReconstructor * const TopoReconstructor)
{  
  fTopoReconstructor = TopoReconstructor;
} // void KFTopoPerformance::SetNewEvent2

void KFTopoPerformance::CheckMCTracks()
{
  mcData = AliHLTTPCCAPerformance::Instance().GetSubPerformance("Global Performance")->GetMCData();
  fMCTrackToMCPVMatch.clear();
  fPrimVertices.clear();

    // find prim vertex
  if (fMCTracks->Size() <= 0) return;
  
  fMCTrackToMCPVMatch.resize(fMCTracks->Size(),-1);
  
  vector<int> pvIndex;
  for(int iTr=0; iTr<fMCTracks->Size(); iTr++)
  {
    AliHLTTPCCAMCTrack &mctr = (*fMCTracks)[iTr];
    int motherID = mctr.MotherId();
    if(motherID <0 )
    {
      bool newPV = 1;
      for(unsigned int iPV=0; iPV<pvIndex.size(); iPV++)
      {
        if(motherID == pvIndex[iPV])
        {
          fPrimVertices[iPV].AddDaughterTrack(iTr);
          fMCTrackToMCPVMatch[iTr] = iPV;
          newPV = 0;
        }
      }
      if(newPV)
      {
        KFMCVertex primVertex;
        primVertex.SetX( mctr.X() );
        primVertex.SetY( mctr.Y() );
        primVertex.SetZ( mctr.Z() );
        primVertex.AddDaughterTrack(iTr);
        if(motherID == -1)
          primVertex.SetTriggerPV();
        fPrimVertices.push_back(primVertex);
        fMCTrackToMCPVMatch[iTr] = pvIndex.size();
        pvIndex.push_back(motherID);
      }
    }
  }
  

  
//   std::cout << "MCTracks: " << std::endl;
//   for(int i = 0; i<fMCTracks->Size(); i++ )
//   {
//     AliHLTTPCCAMCTrack &m = (*fMCTracks)[i];
//     std::cout << "i " << i << " pdg " << m.PDG() << " mother " << m.MotherId() << " X " << m.X() << " Y " << m.Y() << " Z " << 
//     m.Z() << " px " << m.Px() << " py " << m.Py() << " pz " << m.Pz() << std::endl;
//     
//   }
//   std::cout << "MCPV: " << std::endl;
//   for(int i=0; i<fPrimVertices.size(); i++)
//   {
//     std::cout << "i " << i << " x " << fPrimVertices[i].X() << " y " << fPrimVertices[i].Y() <<" z " << fPrimVertices[i].Z()  << std::endl;
//     std::cout << "    ";
//     for(int id=0; id < fPrimVertices[i].NDaughterTracks(); id++)
//       std::cout << fPrimVertices[i].DaughterTrack(id) <<" ";
//     std::cout << endl;
//   }
} // void KFTopoPerformance::CheckMCTracks()

void KFTopoPerformance::GetMCParticles()
{
  // convert MC tracks into KF MC Particles

  vMCParticles.clear();
  // all MC tracks are copied into KF MC Particles
  for(int iMC=0; iMC < nMCTracks; iMC++)
  {
    AliHLTTPCCAMCTrack &mtra = (*fMCTracks)[iMC];
    KFMCParticle part;
    part.SetMCTrackID( iMC );
    part.SetMotherId ( mtra.MotherId() );
    part.SetPDG      ( mtra.PDG() );
    vMCParticles.push_back( part );
  }
    // find relations between mother and daughter MC particles
  const unsigned int nMCParticles = vMCParticles.size();
  for ( unsigned int iP = 0; iP < nMCParticles; iP++ ) {
    KFMCParticle &part = vMCParticles[iP];
    for(unsigned int iP2 = 0; iP2 < nMCParticles; iP2++) {
      KFMCParticle &part2 = vMCParticles[iP2];

      if(part.GetMotherId() == part2.GetMCTrackID()) {
        part2.AddDaughter(iP);
      }
    }
  }

  for(unsigned int iMC=0; iMC < vMCParticles.size(); iMC++)
  {
    KFMCParticle &part = vMCParticles[iMC];
    part.SetMCTrackID( iMC );
//     if(part.NDaughters() > 0 && part.GetPDG() == 310)
//     {
//       std::cout << iMC << " " << part.GetPDG() << " "<<std::endl;
//       for(int iD=0; iD<part.NDaughters(); iD++)
//       {
//         int dId = part.GetDaughterIds()[iD];
//         AliHLTTPCCAMCTrack &mtra = (*fMCTracks)[vMCParticles[dId].GetMCTrackID()];
//         std::cout << "   d" <<iD<<" " << dId << " " << vMCParticles[dId].GetPDG() <<" " << mtra.X() << " " << mtra.Y() << " " << mtra.Z() << " " << mtra.Px() << " " << mtra.Py() << " " << mtra.Pz() <<std::endl;;
//       }
//     }
  }
}

void KFTopoPerformance::FindReconstructableMCParticles()
{
  const unsigned int nMCParticles = vMCParticles.size();

  for ( unsigned int iP = 0; iP < nMCParticles; iP++ ) {
    KFMCParticle &part = vMCParticles[iP];
    CheckMCParticleIsReconstructable(part);
  }
}

void KFTopoPerformance::CheckMCParticleIsReconstructable(KFMCParticle &part)
{
  if ( part.IsReconstructable(0) ) return;

    // tracks
  if ( /*part.NDaughters() == 0*/ part.GetPDG() ==  -211 ||
                                  part.GetPDG() ==   211 ||
                                  part.GetPDG() ==  2212 ||
                                  part.GetPDG() == -2212 ||
                                  part.GetPDG() ==   321 ||
                                  part.GetPDG() ==  -321 ||
                                  part.GetPDG() ==    11 ||
                                  part.GetPDG() ==   -11 ||
                                  part.GetPDG() ==    13 ||
                                  part.GetPDG() ==   -13 ) { // TODO other particles

    part.SetAsReconstructable(0);

    int iMCPart = part.GetMCTrackID();
    AliHLTTPCCAPerformanceMCTrackData &mc = 
      AliHLTTPCCAPerformance::Instance().GetSubPerformance("Global Performance")->GetMCData()[iMCPart];

    int iMCTrack = part.GetMCTrackID();
    AliHLTTPCCAMCTrack &mcTrack = (*fMCTracks)[iMCTrack];
      
    if(mcTrack.NMCPoints() >= 15)
      part.SetAsReconstructable(1);   
//     if(mc.IsReconstructable())
//       part.SetAsReconstructable2();

    if(mc.IsReconstructed())
      part.SetAsReconstructable(2);
  }
    //  mother particles
  else
  {
    //Check if the particle is V0
    
    if(part.NDaughters() >= 2)
    {
      bool isPositiveDaughter[3] = {0,0,0};
      bool isNegativeDaughter[3] = {0,0,0};
      
      int nRecoDaughters[3] = {0,0,0};
      
      for(int iD=0; iD < part.NDaughters(); iD++)
      {
        KFMCParticle &daughter = vMCParticles[part.GetDaughterIds()[iD]];
        CheckMCParticleIsReconstructable(daughter);
        
        TParticlePDG* particlePDG = TDatabasePDG::Instance()->GetParticle(daughter.GetPDG());
        Double_t charge = (particlePDG) ? particlePDG->Charge()/3 : 0;
        
        for(int iEff=0; iEff<3; iEff++)
        {
          if(charge > 0)
            isPositiveDaughter[iEff] |= daughter.IsReconstructable(iEff);
          else
            isNegativeDaughter[iEff] |= daughter.IsReconstructable(iEff);
          
          if(daughter.IsReconstructable(iEff))
            nRecoDaughters[iEff]++;
        }
      }
      
//       for(int iEff=0; iEff<3; iEff++)
//         if(isPositiveDaughter[iEff] && isNegativeDaughter[iEff])
//           part.SetAsReconstructableV0(iEff);
      for(int iEff=0; iEff<3; iEff++)
        if(nRecoDaughters[iEff] > 1)
          part.SetAsReconstructableV0(iEff);
    }
    
    for(int iPart=0; iPart<fParteff.nParticles; iPart++)
    {
      if(part.GetPDG() == fParteff.partPDG[iPart])
      {
        const unsigned int nDaughters = fParteff.partDaughterPdg[iPart].size();
        if( part.GetDaughterIds().size() != nDaughters ) return;
        vector<int> pdg(nDaughters);

        for(unsigned int iD=0; iD<nDaughters; iD++)
          pdg[iD] = vMCParticles[part.GetDaughterIds()[iD]].GetPDG();

        vector<bool> isDaughterFound(nDaughters);
        for(unsigned int iDMC=0; iDMC<nDaughters; iDMC++)
          isDaughterFound[iDMC] = 0;

        bool isReco = 1;
        for(unsigned int iDMC=0; iDMC<nDaughters; iDMC++)
          for(unsigned int iD=0; iD<nDaughters; iD++)
            if(pdg[iD] == fParteff.partDaughterPdg[iPart][iDMC]) isDaughterFound[iDMC] = 1;

        for(unsigned int iDMC=0; iDMC<nDaughters; iDMC++)
          isReco = isReco && isDaughterFound[iDMC];

        if(!isReco) return;
      }
    }


    const vector<int>& dIds = part.GetDaughterIds();
    const unsigned int nD = dIds.size();
    bool reco1 = 1;
    bool reco2 = 1;
    bool reco3 = 1;

    for ( unsigned int iD = 0; iD < nD && (reco1 || reco2 || reco3); iD++ ) {
      KFMCParticle &dp = vMCParticles[dIds[iD]];
      CheckMCParticleIsReconstructable(dp);
      reco1 &= dp.IsReconstructable(0);
      reco2 &= dp.IsReconstructable(1);
      reco3 &= dp.IsReconstructable(2);
    }
    if (reco1) part.SetAsReconstructable(0);
    if (reco2) part.SetAsReconstructable(1);
    if (reco3) part.SetAsReconstructable(2);
  }
}


void KFTopoPerformance::FindReconstructableMCVertices()
{
  const unsigned int nMCVertices = fPrimVertices.size();

  for ( unsigned int iV = 0; iV < nMCVertices; iV++ ) {
    KFMCVertex &vert = fPrimVertices[iV];
        
    int nReconstructableDaughters = 0;
    int nMCReconstructableDaughters = 0;

    for(int iP=0; iP<vert.NDaughterTracks(); iP++)
    {
      int idDaughter = vert.DaughterTrack(iP);
      KFMCParticle &part = vMCParticles[idDaughter];
      
      if(part.IsReconstructable(2)) nReconstructableDaughters++;
      if(part.IsReconstructable(1)) nMCReconstructableDaughters++;
    }
    
    if(nReconstructableDaughters >= 2) vert.SetReconstructable();
    else vert.SetUnReconstructable();
    
    if(nMCReconstructableDaughters >= 2) vert.SetMCReconstructable();
    else vert.SetMCUnReconstructable();
  }
}

void KFTopoPerformance::MatchParticles()
{
    // get all reco particles ( temp )
  MCtoRParticleId.clear();
  RtoMCParticleId.clear();
  MCtoRParticleId.resize(vMCParticles.size());
  RtoMCParticleId.resize(fTopoReconstructor->GetParticles().size() );
  
    // match tracks ( particles which are direct copy of tracks )
  for( unsigned int iRP = 0; iRP < fTopoReconstructor->GetParticles().size(); iRP++ ) {
//    CbmKFParticle &rPart = fTopoReconstructor->GetParticles()[iRP];
    const KFParticle &rPart = fTopoReconstructor->GetParticles()[iRP];

    if (rPart.NDaughters() != 1) continue;
    
    const int rTrackId = rPart.DaughterIds()[0];
    const int mcTrackId = recoData[rTrackId].GetMCTrackId();

// #ifdef MAIN_DRAW
//   if ( AliHLTTPCCADisplay::Instance().DrawType() == 3 ) {
//   if( mcTrackId == 2346 || mcTrackId == 2347 )
//   {
//     if(iRP==0)
//     {
//       AliHLTTPCCADisplay::Instance().ClearView();
//       AliHLTTPCCADisplay::Instance().SetTPCView();
//       AliHLTTPCCADisplay::Instance().DrawTPC();
//     }
// 
//     AliHLTTPCCADisplay::Instance().DrawGBTrack( rTrackId, kBlue, 2. );
// 
//     AliHLTTPCCADisplay::Instance().SpecDrawMCTrackPointsGlobal( (*AliHLTTPCCAPerformance::Instance().GetSubPerformance("Global Performance")->fMCTracks)[mcTrackId], AliHLTTPCCAPerformance::Instance().GetSubPerformance("Global Performance")->fLocalMCPoints, kRed, 0.3 );
// 
//     AliHLTTPCCADisplay::Instance().DrawGBPoint( rPart.X(), rPart.Y(), rPart.Z(), kGreen );
//     KFParticle rPart1 = rPart;
//     const Double_t vtx[3] = {0,0,18};
//     rPart1.TransportToPoint(vtx);
//     AliHLTTPCCADisplay::Instance().DrawGBPoint( rPart1.X(), rPart1.Y(), rPart1.Z(), kMagenta );
//     const KFParticle &mPart = fTopoReconstructor->GetParticles()[2];
//     AliHLTTPCCADisplay::Instance().DrawGBPoint( mPart.X(), mPart.Y(), mPart.Z(), kOrange );
// 
//     AliHLTTPCCADisplay::Instance().Ask();
//   }
//   }
// #endif

    if ( recoData[rTrackId].IsGhost(PParameters::MinTrackPurity) ) continue;

    for ( unsigned int iMP = 0; iMP < vMCParticles.size(); iMP++ ) {
      KFMCParticle &mPart = vMCParticles[iMP];
      if ( mPart.GetMCTrackID() == mcTrackId ) { // match is found
        if( mPart.GetPDG() == rPart.GetPDG() ) {
          MCtoRParticleId[iMP].ids.push_back(iRP);
          RtoMCParticleId[iRP].ids.push_back(iMP);
        }
        else {
          MCtoRParticleId[iMP].idsMI.push_back(iRP);
          RtoMCParticleId[iRP].idsMI.push_back(iMP);
        }
// KFMCParticle &mmPart = vMCParticles[mPart.GetMotherId()];
// if(mmPart.GetPDG()==310) std::cout << "!!!!! " << rTrackId << " " << iMP << " " << mcTrackId<< std::endl;
      }
    }
  }

    // match created mother particles
  for( unsigned int iRP = 0; iRP < fTopoReconstructor->GetParticles().size(); iRP++ ) {
//    CbmKFParticle &rPart = fTopoReconstructor->GetParticles()[iRP];
    const KFParticle &rPart = fTopoReconstructor->GetParticles()[iRP];
    const unsigned int NRDaughters = rPart.NDaughters();
    if (NRDaughters < 2) continue;
    
    unsigned int iD = 0;
    vector<int> mcDaughterIds;
    int mmId = -2; // mother MC id
    {
      const int rdId = rPart.DaughterIds()[iD];
      if ( !RtoMCParticleId[rdId].IsMatched() ) continue;
      const int mdId = RtoMCParticleId[rdId].GetBestMatch();
      mcDaughterIds.push_back(mdId);
      mmId = vMCParticles[mdId].GetMotherId();
    }
    iD++;
    for ( ; iD < NRDaughters; iD++ ) {
      const int rdId = rPart.DaughterIds()[iD];

      if ( !RtoMCParticleId[rdId].IsMatched() ) break;
      const int mdId = RtoMCParticleId[rdId].GetBestMatch();
      mcDaughterIds.push_back(mdId);
      if( vMCParticles[mdId].GetMotherId() != mmId ) break;
    }
    
    int nClones = 0;
    sort(mcDaughterIds.begin(), mcDaughterIds.end());
    for(unsigned int ie=1; ie<mcDaughterIds.size(); ie++)
    {
      if(mcDaughterIds[ie] == mcDaughterIds[ie-1])
        nClones++;
    }
    if(nClones > 0) continue;

    if ( iD == NRDaughters && mmId > -1 ) { // match is found and it is not primary vertex
      KFMCParticle &mmPart = vMCParticles[mmId];
      
      if( mmPart.GetPDG()     == rPart.GetPDG()     &&
          mmPart.NDaughters() == rPart.NDaughters() ) {
        MCtoRParticleId[mmId].ids.push_back(iRP);
        RtoMCParticleId[iRP].ids.push_back(mmId);
      }
      else {
        MCtoRParticleId[mmId].idsMI.push_back(iRP);
        RtoMCParticleId[iRP].idsMI.push_back(mmId);
      }
    }
  }
}

void KFTopoPerformance::MatchPV()
{
  MCtoRPVId.clear();
  RtoMCPVId.clear();
  MCtoRPVId.resize(fPrimVertices.size());
  RtoMCPVId.resize(fTopoReconstructor->NPrimaryVertices() );

  fPVPurity.clear();
  fPVPurity.resize(fTopoReconstructor->NPrimaryVertices(), 0.);
  fNCorrectPVTracks.clear();
  fNCorrectPVTracks.resize(fTopoReconstructor->NPrimaryVertices(), 0);

  for(int iE = 0; iE<4; iE++)
  {
    fPVTracksRate[iE].clear();
    fPVTracksRate[iE].resize(fTopoReconstructor->NPrimaryVertices(),0);
  }
  
  for( unsigned int iMCPV=0; iMCPV<fPrimVertices.size(); iMCPV++)
  {
    KFMCVertex &mcPV = fPrimVertices[iMCPV];
    int nReconstructedDaughters = 0;
    for(int iD=0; iD<mcPV.NDaughterTracks(); iD++)
      if(MCtoRParticleId[ mcPV.DaughterTrack(iD) ].IsMatched())
        nReconstructedDaughters++;
        
    mcPV.SetNReconstructedDaughters(nReconstructedDaughters);
  }

  for( int iPV = 0; iPV < fTopoReconstructor->NPrimaryVertices(); iPV++ ) {

    vector<short int> &tracks = fTopoReconstructor->GetPVTrackIndexArray(iPV);
    
    int nPVTracks = tracks.size()>0 ? tracks.size() : -1;//tracks.size();
    
    vector<short int> nTracksFromMCPV(fPrimVertices.size() + vMCParticles.size(), 0);
    vector< vector<int> > mcIDs(fPrimVertices.size() + vMCParticles.size());

    int nGhostTracks = 0;
    int nTriggerTracks = 0;
    int nPileupTracks = 0;
    int nBGTracks = 0;
        
    for(int iRP=0; iRP < nPVTracks; iRP++)
    {
      const int rTrackId = tracks[iRP];
      if ( !RtoMCParticleId[rTrackId].IsMatched() )
      {
        nGhostTracks++;
        continue;
      }
            
      int iMCPart = RtoMCParticleId[rTrackId].GetBestMatch();
      KFMCParticle &mcPart = vMCParticles[iMCPart];
      int iMCTrack = mcPart.GetMCTrackID();
      AliHLTTPCCAMCTrack &mcTrack = (*fMCTracks)[iMCTrack];
      
      int motherId = mcTrack.MotherId();

      if(motherId < 0) //real PV
      {
        if(motherId == -1)
          nTriggerTracks++;
        if(motherId < -1)
          nPileupTracks++;
        
        int iMCPV = fMCTrackToMCPVMatch[iMCTrack];
        if(iMCPV < 0)
        {
          std::cout << "Error!!!  iMCPV < 0" << std::endl;
          continue;
        }
      
        nTracksFromMCPV[iMCPV]++;
        mcIDs[iMCPV].push_back(iMCTrack);
      }
      else // pchysics background
      {        
        if(motherId >-1)
        {
          nBGTracks++;

          int iMCPV = motherId + fPrimVertices.size();
      
          nTracksFromMCPV[iMCPV]++;
          mcIDs[iMCPV].push_back(iMCTrack);
        }
      }
    }
    


    for(unsigned int iMCPV=0; iMCPV<nTracksFromMCPV.size(); iMCPV++)
    {
      int nClones = 0;
      sort(mcIDs[iMCPV].begin(), mcIDs[iMCPV].end());
      for(unsigned int ie=1; ie<mcIDs[iMCPV].size(); ie++)
      {
        if(mcIDs[iMCPV][ie] == mcIDs[iMCPV][ie-1])
          nClones++;
      }
      nTracksFromMCPV[iMCPV] = nTracksFromMCPV[iMCPV] - nClones;
      nPVTracks -= nClones;
    }

    // calculate rate of each type of tracks in reconstructed PV
    fPVTracksRate[0][iPV] = double(nGhostTracks)/double(nPVTracks);
    fPVTracksRate[1][iPV] = double(nTriggerTracks)/double(nPVTracks);
    fPVTracksRate[2][iPV] = double(nPileupTracks)/double(nPVTracks);
    fPVTracksRate[3][iPV] = double(nBGTracks)/double(nPVTracks);

    int iBestMCPV=-1;
    int nTracksBestMCPV = 1;
    for(unsigned int iMCPV=0; iMCPV<nTracksFromMCPV.size(); iMCPV++ )
    {
      if(nTracksFromMCPV[iMCPV] > nTracksBestMCPV )
      {
        nTracksBestMCPV = nTracksFromMCPV[iMCPV];
        iBestMCPV = iMCPV;
      }
    }

    if( (iBestMCPV > -1) && (iBestMCPV<int(fPrimVertices.size())) )
    {
      fNCorrectPVTracks[iPV] = nTracksFromMCPV[iBestMCPV];
    }
    else
      fNCorrectPVTracks[iPV] = 0;
    
    double purity = double(nTracksBestMCPV)/double(nPVTracks);

    fPVPurity[iPV] = purity;
//     if(purity < 0.7) continue;

    if(iBestMCPV < 0) continue;
    
    if(iBestMCPV < int(fPrimVertices.size()))
    {
      fPrimVertices[iBestMCPV].SetReconstructed();
    
      MCtoRPVId[iBestMCPV].ids.push_back(iPV);
      RtoMCPVId[iPV].ids.push_back(iBestMCPV);
    }
    else
    {
      RtoMCPVId[iPV].idsMI.push_back(iBestMCPV);
    }
    
//     for(unsigned int iMCPV=0; iMCPV<nTracksFromMCPV.size(); iMCPV++ )
//     {
//       if(nTracksFromMCPV[iMCPV] > 0 )
//       {
//         if( nTracksFromMCPV[iMCPV] >= 0.5 * fPrimVertices[iMCPV].NReconstructedDaughterTracks() )
//           if(iMCPV < int(fPrimVertices.size()))
//           {
//             fPrimVertices[iMCPV].SetReconstructed();
//           
//             MCtoRPVId[iMCPV].ids.push_back(iPV);
//             RtoMCPVId[iPV].ids.push_back(iMCPV);
//           }
//           else
//           {
//             RtoMCPVId[iPV].idsMI.push_back(iMCPV);
//           }
//       }
//     }
  }
}

void KFTopoPerformance::MatchTracks()
{
  recoData = AliHLTTPCCAPerformance::Instance().GetSubPerformance("Global Performance")->GetRecoData();
  GetMCParticles();
  FindReconstructableMCParticles();
  MatchParticles();
  CalculateEfficiency();
  
  FindReconstructableMCVertices();
  MatchPV();
  CalculatePVEfficiency();
} // void KFTopoPerformance::MatchTracks()

void KFTopoPerformance::CalculateEfficiency()
{
  KFPartEfficiencies partEff; // efficiencies for current event

  const int NRP = fTopoReconstructor->GetParticles().size();
  for ( int iP = 0; iP < NRP; ++iP ) {
//    const CbmKFParticle &part = fPF->GetParticles()[iP];
    const KFParticle &part = fTopoReconstructor->GetParticles()[iP];
    const int pdg = part.GetPDG();

    const bool isBG = RtoMCParticleId[iP].idsMI.size() != 0;
    const bool isGhost = !RtoMCParticleId[iP].IsMatched();

    for(int iPart=0; iPart<fParteff.nParticles; iPart++)
      if ( pdg == fParteff.partPDG[iPart] )
        partEff.IncReco(isGhost, isBG, fParteff.partName[iPart].Data());
    
    // Calculate the gost level for V0
    if(CAMath::Abs(pdg) == 310  /*||
       CAMath::Abs(pdg) == 3122 ||
       CAMath::Abs(pdg) == 421  ||
       CAMath::Abs(pdg) == 22 */)
    {
      partEff.IncReco(isGhost, 0, fParteff.partName[fParteff.nParticles - 1].Data());
    }
  }

  const int NMP = vMCParticles.size();
  for ( int iP = 0; iP < NMP; ++iP ) {
    const KFMCParticle &part = vMCParticles[iP];
    const int pdg = part.GetPDG();
    const int mId = part.GetMotherId();

    vector<bool> isReco;
    vector<int> nClones;

    vector<int> iParticle;
    iParticle.push_back(fParteff.GetParticleIndex(pdg));
    vector< vector<bool> > isReconstructable;
    vector<bool> isRecPart;
    for(int iEff = 0; iEff < 3; iEff++)
      isRecPart.push_back(part.IsReconstructable(iEff));

    isReconstructable.push_back(isRecPart);
    isReco.push_back( MCtoRParticleId[iP].ids.size() != 0 );
    nClones.push_back( MCtoRParticleId[iP].ids.size() - 1 );
    
    if(part.IsReconstructableV0(0) || part.IsReconstructableV0(1) || part.IsReconstructableV0(2) )
    {
      iParticle.push_back(fParteff.nParticles - 1);
      vector<bool> isRecV0;
      for(int iEff = 0; iEff < 3; iEff++)
        isRecV0.push_back(part.IsReconstructableV0(iEff));
      isReconstructable.push_back(isRecV0);
      isReco.push_back( (MCtoRParticleId[iP].ids.size() != 0) || (MCtoRParticleId[iP].idsMI.size() != 0) );
      
      int nClonesV0 = MCtoRParticleId[iP].ids.size() + MCtoRParticleId[iP].idsMI.size() - 1;
      nClones.push_back( nClonesV0 );
    }

    {
      for(int iPType=0; iPType<iParticle.size(); iPType++)
      {
        int iPart = iParticle[iPType];
        if(iPart<0) continue;
          
        partEff.Inc(isReco[iPType], nClones[iPType], isReconstructable[iPType][0], isReconstructable[iPType][1], isReconstructable[iPType][2], fParteff.partName[iPart].Data());
        if ( mId == -1 )
          partEff.Inc(isReco[iPType], nClones[iPType], isReconstructable[iPType][0], isReconstructable[iPType][1], isReconstructable[iPType][2], (fParteff.partName[iPart]+"_prim").Data());
        else
          partEff.Inc(isReco[iPType], nClones[iPType], isReconstructable[iPType][0], isReconstructable[iPType][1], isReconstructable[iPType][2], (fParteff.partName[iPart]+"_sec").Data());
        
        for(int iEff=0; iEff<3; iEff++)
        {
          if(!isReconstructable[iPType][iEff]) continue;
          
          int iMCTrack = part.GetMCTrackID();
          AliHLTTPCCAMCTrack &mcTrack = (*fMCTracks)[iMCTrack];
          
          TParticlePDG* particlePDG = TDatabasePDG::Instance()->GetParticle(mcTrack.PDG());
          Double_t massMC = (particlePDG) ? particlePDG->Mass() :0.13957;
          Double_t E = CAMath::Sqrt(mcTrack.P()*mcTrack.P() + massMC*massMC);
          Double_t Y = 0.5*log((E + mcTrack.Pz())/(E - mcTrack.Pz()));
          
          Double_t R = -1, L=-1;
          if(part.NDaughters() > 0)
          {
            int mcDaughterId = part.GetDaughterIds()[0];
            AliHLTTPCCAMCTrack &mcDaughter = (*fMCTracks)[mcDaughterId];
            R = CAMath::Sqrt(mcDaughter.X()*mcDaughter.X() + mcDaughter.Y()*mcDaughter.Y());
            L = CAMath::Sqrt(mcDaughter.X()*mcDaughter.X() + mcDaughter.Y()*mcDaughter.Y());
          }
          
          hPartEfficiency[iPart][iEff][0]->Fill( mcTrack.P(), isReco[iPType] );
          hPartEfficiency[iPart][iEff][1]->Fill( mcTrack.Pt(), isReco[iPType] );
          hPartEfficiency[iPart][iEff][2]->Fill( Y, isReco[iPType] );
          hPartEfficiency[iPart][iEff][3]->Fill( mcTrack.Z(), isReco[iPType] );
          hPartEfficiency[iPart][iEff][6]->Fill( L, isReco[iPType] );
          hPartEfficiency[iPart][iEff][7]->Fill( R, isReco[iPType] );
        }
      }
    }
  }

  fNEvents++;

  fParteff += partEff;

  partEff.CalcEff();
  fParteff.CalcEff();

    //   cout.precision(3);
//   if(fNEvents%100 == 0)
  {
    cout << " ---- KF Particle finder --- " << endl;
    // cout << "L1 STAT    : " << fNEvents << " EVENT "               << endl << endl;
    //partEff.PrintEff();
    // cout << endl;
    cout << "ACCUMULATED STAT    : " << fNEvents << " EVENTS "               << endl << endl;
    fParteff.PrintEff();

    cout<<endl;
      // cout<<"CA Track Finder: " << L1_CATIME/L1_fNEvents << " s/ev" << endl << endl;
  }
}

void KFTopoPerformance::CalculatePVEfficiency()
{
  KFPVEfficiencies pvEff; // efficiencies for current event
  KFPVEfficiencies pvEffMCReconstructable;
  int nTracks = 0;
  //calculate N reco tracks
  for( unsigned int iRP = 0; iRP < fTopoReconstructor->GetParticles().size(); iRP++ ) {
//    CbmKFParticle &rPart = fTopoReconstructor->GetParticles()[iRP];
    const KFParticle &rPart = fTopoReconstructor->GetParticles()[iRP];

    if (rPart.NDaughters() != 1) continue;
    
    nTracks++;
  }
    
  const int NRecoPV = fTopoReconstructor->NPrimaryVertices();
  for ( int iP = 0; iP < NRecoPV; ++iP ) {
      
    const bool isBG = RtoMCPVId[iP].idsMI.size() != 0;
    const bool isGhost = !RtoMCPVId[iP].IsMatched();

    pvEff.IncReco(isGhost, isBG, "PV");
    pvEffMCReconstructable.IncReco(isGhost, isBG, "PV");
  }

  const int NMCPV = fPrimVertices.size();
  for ( int iV = 0; iV < NMCPV; ++iV ) {
    const KFMCVertex &pvMC = fPrimVertices[iV];
    if ( pvMC.IsReconstructable() ) 
    {
      const bool isReco = pvMC.IsReconstructed();
      const int nClones = MCtoRPVId[iV].ids.size() - 1;

        
      pvEff.Inc(isReco, nClones, "PV");
      if ( pvMC.IsTriggerPV() )
      {
        pvEff.Inc(isReco, nClones, "PVtrigger");
        hPVefficiency[0][0]->Fill( pvMC.NDaughterTracks(), isReco );
        hPVefficiency[0][1]->Fill( NMCPV, isReco );
        hPVefficiency[0][2]->Fill( fMCTracks->Size(), isReco );
        hPVefficiency[0][3]->Fill( pvMC.NReconstructedDaughterTracks(), isReco );
        hPVefficiency[0][4]->Fill( NRecoPV, isReco );
        hPVefficiency[0][5]->Fill( nTracks, isReco );
      }
      else
      {
        pvEff.Inc(isReco, nClones, "PVpileup");
        hPVefficiency[1][0]->Fill( pvMC.NDaughterTracks(), isReco );
        hPVefficiency[1][1]->Fill( NMCPV, isReco );
        hPVefficiency[1][2]->Fill( fMCTracks->Size(), isReco );
        hPVefficiency[1][3]->Fill( pvMC.NReconstructedDaughterTracks(), isReco );
        hPVefficiency[1][4]->Fill( NRecoPV, isReco );
        hPVefficiency[1][5]->Fill( nTracks, isReco );
      }
    }
    if ( pvMC.IsMCReconstructable() ) 
    {
      const bool isReco = pvMC.IsReconstructed();
      const int nClones = MCtoRPVId[iV].ids.size() - 1;

        
      pvEffMCReconstructable.Inc(isReco, nClones, "PV");
      if ( pvMC.IsTriggerPV() )
      {
        pvEffMCReconstructable.Inc(isReco, nClones, "PVtrigger");
        hPVefficiency[2][0]->Fill( pvMC.NDaughterTracks(), isReco );
        hPVefficiency[2][1]->Fill( NMCPV, isReco );
        hPVefficiency[2][2]->Fill( fMCTracks->Size(), isReco );
        hPVefficiency[2][3]->Fill( pvMC.NReconstructedDaughterTracks(), isReco );
        hPVefficiency[2][4]->Fill( NRecoPV, isReco );
        hPVefficiency[2][5]->Fill( nTracks, isReco );
      }
      else
      {
        pvEffMCReconstructable.Inc(isReco, nClones, "PVpileup");
        hPVefficiency[3][0]->Fill( pvMC.NDaughterTracks(), isReco );
        hPVefficiency[3][1]->Fill( NMCPV, isReco );
        hPVefficiency[3][2]->Fill( fMCTracks->Size(), isReco );
        hPVefficiency[3][3]->Fill( pvMC.NReconstructedDaughterTracks(), isReco );
        hPVefficiency[3][4]->Fill( NRecoPV, isReco );
        hPVefficiency[3][5]->Fill( nTracks, isReco );
      }
    }
  }

  fPVeff += pvEff;
  
  pvEff.CalcEff();
  fPVeff.CalcEff();
  
  fPVeffMCReconstructable += pvEffMCReconstructable;
  pvEffMCReconstructable.CalcEff();
  fPVeffMCReconstructable.CalcEff();

    //   cout.precision(3);
  //if(fNEvents%100 == 0)
  {
    cout << " ---- KF PV finder --- " << endl;
    // cout << "L1 STAT    : " << fNEvents << " EVENT "               << endl << endl;
    //partEff.PrintEff();
    // cout << endl;
    cout << "ACCUMULATED STAT    : " << fNEvents << " EVENTS "               << endl << endl;
    cout << "PV with at least 2 reconstructed tracks is reconstructable:" << endl;
    fPVeff.PrintEff();
    cout << endl;
    cout << "PV with at least 2 MC tracks with 15 MC points is reconstructable:" << endl;
    fPVeffMCReconstructable.PrintEff();

    cout<<endl;
      // cout<<"CA Track Finder: " << L1_CATIME/L1_fNEvents << " s/ev" << endl << endl;
  }
}

void KFTopoPerformance::FillHistos()
{
  //fill histograms for found short-lived particles
  for(unsigned int iP=0; iP<fTopoReconstructor->GetParticles().size(); iP++)
  {

    int iParticle = fParteff.GetParticleIndex(fTopoReconstructor->GetParticles()[iP].GetPDG());
    if(iParticle < 0) continue;

    Double_t M, ErrM;
    Double_t dL, ErrdL; // decay length
    Double_t cT, ErrcT; // c*tau
    Double_t P, ErrP;
    Double_t Pt;
    Double_t Rapidity;
    Double_t Theta;
    Double_t Phi;
    Double_t X,Y,Z,R;
//    CbmKFParticle TempPart = fTopoReconstructor->GetParticles()[iP];
    KFParticle TempPart = fTopoReconstructor->GetParticles()[iP];
    TempPart.GetMass(M,ErrM);
    TempPart.GetMomentum(P,ErrP);
    Pt = TempPart.GetPt();
    Rapidity = TempPart.GetRapidity();
    TempPart.GetDecayLength(dL,ErrdL);
    TempPart.GetLifeTime(cT,ErrcT);
    Double_t chi2 = TempPart.GetChi2();
    Int_t ndf = TempPart.GetNDF();
    Double_t prob = TMath::Prob(chi2, ndf);//(TDHelper<float>::Chi2IProbability( ndf, chi2 ));
    Theta = TempPart.GetTheta();
    Phi = TempPart.GetPhi();
    X = TempPart.GetX();
    Y = TempPart.GetY();
    Z = TempPart.GetZ();
    R = CAMath::Sqrt(X*X+Y*Y);

    KFParticleSIMD tempSIMDPart(TempPart);
    float_v l,dl;
    KFParticleSIMD pv(fTopoReconstructor->GetPrimVertex());
    tempSIMDPart.GetDistanceToVertexLine(pv, l, dl);

    //for all particle-candidates
    hPartParam[iParticle][ 0]->Fill(M);
    hPartParam[iParticle][ 1]->Fill(P);
    hPartParam[iParticle][ 2]->Fill(Pt);
    hPartParam[iParticle][ 3]->Fill(Rapidity);
    hPartParam[iParticle][ 4]->Fill(dL);
    hPartParam[iParticle][ 5]->Fill(cT);
    hPartParam[iParticle][ 6]->Fill(chi2/ndf);
    hPartParam[iParticle][ 7]->Fill(prob);
    hPartParam[iParticle][ 8]->Fill(Theta);
    hPartParam[iParticle][ 9]->Fill(Phi);
    hPartParam[iParticle][10]->Fill(X);
    hPartParam[iParticle][11]->Fill(Y);
    hPartParam[iParticle][12]->Fill(Z);
    hPartParam[iParticle][13]->Fill(R);
    hPartParam[iParticle][14]->Fill(l[0]);
    hPartParam[iParticle][15]->Fill(l[0]/dl[0]);


    hPartParam2D[iParticle][0]->Fill(Rapidity,Pt,1);

    if(!RtoMCParticleId[iP].IsMatchedWithPdg()) //background
    {
      if(!RtoMCParticleId[iP].IsMatched())
      {
        // for ghost particles - combinatorial background
        hPartParamGhost[iParticle][ 0]->Fill(M);
        hPartParamGhost[iParticle][ 1]->Fill(P);
        hPartParamGhost[iParticle][ 2]->Fill(Pt);
        hPartParamGhost[iParticle][ 3]->Fill(Rapidity);
        hPartParamGhost[iParticle][ 4]->Fill(dL);
        hPartParamGhost[iParticle][ 5]->Fill(cT);
        hPartParamGhost[iParticle][ 6]->Fill(chi2/ndf);
        hPartParamGhost[iParticle][ 7]->Fill(prob);
        hPartParamGhost[iParticle][ 8]->Fill(Theta);
        hPartParamGhost[iParticle][ 9]->Fill(Phi);
        hPartParamGhost[iParticle][10]->Fill(X);
        hPartParamGhost[iParticle][11]->Fill(Y);
        hPartParamGhost[iParticle][12]->Fill(Z);
        hPartParamGhost[iParticle][13]->Fill(R);
        hPartParamGhost[iParticle][14]->Fill(l[0]);
        hPartParamGhost[iParticle][15]->Fill(l[0]/dl[0]);
    
        hPartParam2DGhost[iParticle][0]->Fill(Rapidity,Pt,1);
      }
      else
      {
        // for phisical background

        hPartParamBG[iParticle][ 0]->Fill(M);
        hPartParamBG[iParticle][ 1]->Fill(P);
        hPartParamBG[iParticle][ 2]->Fill(Pt);
        hPartParamBG[iParticle][ 3]->Fill(Rapidity);
        hPartParamBG[iParticle][ 4]->Fill(dL);
        hPartParamBG[iParticle][ 5]->Fill(cT);
        hPartParamBG[iParticle][ 6]->Fill(chi2/ndf);
        hPartParamBG[iParticle][ 7]->Fill(prob);
        hPartParamBG[iParticle][ 8]->Fill(Theta);
        hPartParamBG[iParticle][ 9]->Fill(Phi);
        hPartParamBG[iParticle][10]->Fill(X);
        hPartParamBG[iParticle][11]->Fill(Y);
        hPartParamBG[iParticle][12]->Fill(Z);
        hPartParamBG[iParticle][13]->Fill(R);
        hPartParamBG[iParticle][14]->Fill(l[0]);
        hPartParamBG[iParticle][15]->Fill(l[0]/dl[0]);

        hPartParam2DBG[iParticle][0]->Fill(Rapidity,Pt,1);
      }
      continue;
    }
    //for signal particles
    hPartParamSignal[iParticle][ 0]->Fill(M);
    hPartParamSignal[iParticle][ 1]->Fill(P);
    hPartParamSignal[iParticle][ 2]->Fill(Pt);
    hPartParamSignal[iParticle][ 3]->Fill(Rapidity);
    hPartParamSignal[iParticle][ 4]->Fill(dL);
    hPartParamSignal[iParticle][ 5]->Fill(cT);
    hPartParamSignal[iParticle][ 6]->Fill(chi2/ndf);
    hPartParamSignal[iParticle][ 7]->Fill(prob);
    hPartParamSignal[iParticle][ 8]->Fill(Theta);
    hPartParamSignal[iParticle][ 9]->Fill(Phi);
    hPartParamSignal[iParticle][10]->Fill(X);
    hPartParamSignal[iParticle][11]->Fill(Y);
    hPartParamSignal[iParticle][12]->Fill(Z);
    hPartParamSignal[iParticle][13]->Fill(R);
    hPartParamSignal[iParticle][14]->Fill(l[0]);
    hPartParamSignal[iParticle][15]->Fill(l[0]/dl[0]);

    hPartParam2DSignal[iParticle][0]->Fill(Rapidity,Pt,1);

    int iMCPart = RtoMCParticleId[iP].GetBestMatchWithPdg();
    KFMCParticle &mcPart = vMCParticles[iMCPart];
    // Fit quality of the mother particle
    {
      int iMCTrack = mcPart.GetMCTrackID();
      AliHLTTPCCAMCTrack &mcTrack = (*fMCTracks)[iMCTrack];
      int mcDaughterId = mcPart.GetDaughterIds()[0];
      AliHLTTPCCAMCTrack &mcDaughter = (*fMCTracks)[mcDaughterId];

      TParticlePDG* particlePDG = TDatabasePDG::Instance()->GetParticle(mcTrack.PDG());
      const float mcX =  mcDaughter.X();
      const float mcY =  mcDaughter.Y();
      const float mcZ =  mcDaughter.Z();
      const float mcPx = mcTrack.Par(3)*mcTrack.P();
      const float mcPy = mcTrack.Par(4)*mcTrack.P();
      const float mcPz = mcTrack.Par(5)*mcTrack.P();

      Double_t decayVtx[3] = { mcX, mcY, mcZ };
      Double_t recParam[8] = { 0 };
      Double_t errParam[8] = { 0 };

      for(int iPar=0; iPar<3; iPar++)
      {
        recParam[iPar] = TempPart.GetParameter(iPar);
        Double_t error = TempPart.GetCovariance(iPar,iPar);
        if(error < 0.) { error = 1.e20;}
        errParam[iPar] = TMath::Sqrt(error);
      }
      TempPart.TransportToPoint(decayVtx);
      for(int iPar=3; iPar<7; iPar++)
      {
        recParam[iPar] = TempPart.GetParameter(iPar);
        Double_t error = TempPart.GetCovariance(iPar,iPar);
        if(error < 0.) { error = 1.e20;}
        errParam[iPar] = TMath::Sqrt(error);
      }

      Double_t massMC = (particlePDG) ? particlePDG->Mass() :0.13957;

      Double_t Emc = sqrt(mcTrack.P()*mcTrack.P() + massMC*massMC);
      Double_t res[8] = {0}, 
               pull[8] = {0}, 
               mcParam[8] = { decayVtx[0], decayVtx[1], decayVtx[2],
                              mcPx, mcPy, mcPz, Emc, massMC };
      for(int iPar=0; iPar < 7; iPar++ )
      {
        res[iPar]  = recParam[iPar] - mcParam[iPar];
        if(fabs(errParam[iPar]) > 1.e-20) pull[iPar] = res[iPar]/errParam[iPar];
      }

      res[7] = M - mcParam[7];
      if(fabs(ErrM) > 1.e-20) pull[7] = res[7]/ErrM;

      for(int iPar=0; iPar < 8; iPar++ )
      {
        hFitQA[iParticle][iPar]->Fill(res[iPar]);
        hFitQA[iParticle][iPar+8]->Fill(pull[iPar]);
      }

// #ifdef MAIN_DRAW
//       if ( AliHLTTPCCADisplay::Instance().DrawType() == 3 ) {
//       if(mcTrack.PDG()==310)
//       {
//         AliHLTTPCCADisplay::Instance().ClearView();
//         AliHLTTPCCADisplay::Instance().SetTPCView();
//         AliHLTTPCCADisplay::Instance().DrawTPC();
// 
//         KFParticle rPart = fTopoReconstructor->GetParticles()[iP];
//         const unsigned int NRDaughters = rPart.NDaughters();
// 
//         float param[8] = {mcX, mcY, mcZ, mcPx, mcPy, mcPz, 0, 0};
// 
//         for (unsigned int iD=0 ; iD < NRDaughters; iD++ ) {
// 
//           KFParticle dPart = fTopoReconstructor->GetParticles()[rPart.DaughterIds()[iD]];
// 
//           int iDMC = RtoMCParticleId[dPart.DaughterIds()[0]].GetBestMatch();
//           AliHLTTPCCADisplay::Instance().SpecDrawMCTrackPointsGlobal( (*AliHLTTPCCAPerformance::Instance().GetSubPerformance("Global Performance")->fMCTracks)[iDMC], AliHLTTPCCAPerformance::Instance().GetSubPerformance("Global Performance")->fLocalMCPoints, kRed, 0.3 );
// 
//           float vtx[3] = {rPart.X(), rPart.Y(), rPart.Z()};
//           double vertex[3] = {rPart.X(), rPart.Y(), rPart.Z()};
//           for(int iP=0; iP<8; iP++)
//             param[iP] = dPart.Parameters()[iP];
//           double b[3];
//           dPart.GetFieldValue(vertex, b);
//           AliHLTTPCCADisplay::Instance().DrawParticleGlobal( param, dPart.Q(), 0, dPart.GetDStoPoint(vertex), b[2], kBlue, 2 );
// std::cout << "!!!!    " << rPart.DaughterIds()[iD] << " " << fTopoReconstructor->GetChiPrim()[rPart.DaughterIds()[iD]] << std::endl;
// 
//         }
// 
//         param[0] = mcX;
//         param[1] = mcY;
//         param[2] = mcZ;
//         param[3] = mcPx;
//         param[4] = mcPy;
//         param[5] = mcPz;
// 
//         AliHLTTPCCADisplay::Instance().DrawGBPoint( param, kBlack );
//         AliHLTTPCCADisplay::Instance().DrawGBPoint( rPart.X(), rPart.Y(), rPart.Z(), kGreen );
// 
//     KFParticle & vtx = fTopoReconstructor->GetPrimVertex();
// 
//         AliHLTTPCCADisplay::Instance().DrawGBPoint( vtx.X(), vtx.Y(), vtx.Z(), kRed );
// 
//         AliHLTTPCCADisplay::Instance().Ask();
//       }
//       }
// #endif
//      Double_t mcT = mcDaughter.GetStartT() - mcTrack.GetStartT();
      
    }
    // Fit quality of daughters
    for(int iD=0; iD<mcPart.NDaughters(); ++iD)
    {
      int mcDaughterId = mcPart.GetDaughterIds()[iD];
//      if(!MCtoRParticleId[mcDaughterId].IsMatchedWithPdg()) continue;
      if(!MCtoRParticleId[mcDaughterId].IsMatched()) continue;
      AliHLTTPCCAMCTrack &mcTrack = (*fMCTracks)[mcDaughterId];
//      int recDaughterId = MCtoRParticleId[mcDaughterId].GetBestMatchWithPdg();
      int recDaughterId = MCtoRParticleId[mcDaughterId].GetBestMatch();
//      CbmKFParticle Daughter = fPF->GetParticles()[recDaughterId];
      KFParticle Daughter = fTopoReconstructor->GetParticles()[recDaughterId];
      Daughter.GetMass(M,ErrM);

      TParticlePDG* particlePDG = TDatabasePDG::Instance()->GetParticle(mcTrack.PDG());
      const float mcX =  mcTrack.X();
      const float mcY =  mcTrack.Y();
      const float mcZ =  mcTrack.Z();
      const float mcPx = mcTrack.Px();
      const float mcPy = mcTrack.Py();
      const float mcPz = mcTrack.Pz();

      Double_t decayVtx[3] = {mcX, mcY, mcZ};
      Daughter.TransportToPoint(decayVtx);

      Double_t massMC = (particlePDG) ? particlePDG->Mass() :0.13957;

      Double_t Emc = sqrt(mcTrack.P()*mcTrack.P() + massMC*massMC);
      Double_t res[8] = {0}, 
               pull[8] = {0}, 
               mcParam[8] = { mcX, mcY, mcZ,
                              mcPx, mcPy, mcPz, Emc, massMC };
      for(int iPar=0; iPar < 7; iPar++ )
      {
        Double_t error = Daughter.GetCovariance(iPar,iPar);
        if(error < 0.) { error = 1.e20;}
        error = TMath::Sqrt(error);
        res[iPar]  = Daughter.GetParameter(iPar) - mcParam[iPar];
        if(fabs(error) > 1.e-20) pull[iPar] = res[iPar]/error;
      }
      res[7] = M - mcParam[7];
      if(fabs(ErrM) > 1.e-20) pull[7] = res[7]/ErrM;

      for(int iPar=0; iPar < 8; iPar++ )
      {
        hFitDaughtersQA[iParticle][iPar]->Fill(res[iPar]);
        hFitDaughtersQA[iParticle][iPar+8]->Fill(pull[iPar]);
      }
    }
  }
  
  //fill histograms with ChiPrim for every particle
  for(unsigned int iP=0; iP<fTopoReconstructor->GetParticles().size(); iP++)
  {
    const KFParticle &rPart = fTopoReconstructor->GetParticles()[iP];
    const unsigned int NRDaughters = rPart.NDaughters();
    if (NRDaughters > 1) break;
    if( RtoMCParticleId[iP].GetBestMatch()<0 ) continue;
    KFMCParticle &mPart = vMCParticles[ RtoMCParticleId[iP].GetBestMatch() ];

    if(mPart.GetMotherId() < 0)
    {
      hTrackParameters[KFPartEfficiencies::nParticles]->Fill(fTopoReconstructor->GetChiPrim()[iP] );
      continue;
    }

    KFMCParticle &mMotherPart = vMCParticles[mPart.GetMotherId()];

    int iParticle = fParteff.GetParticleIndex(mMotherPart.GetPDG());
    float chiPrim = fTopoReconstructor->GetChiPrim()[iP];
    if(iParticle > -1 && iParticle<KFPartEfficiencies::nParticles)
      hTrackParameters[iParticle]->Fill(chiPrim );
  }

  //fill histograms of the primary vertex quality
  for(int iPV = 0; iPV<fTopoReconstructor->NPrimaryVertices(); iPV++)
  {
    KFParticle & vtx = fTopoReconstructor->GetPrimVertex(iPV);
    vector<short int> &tracks = fTopoReconstructor->GetPVTrackIndexArray(iPV);

    Double_t probPV = TMath::Prob(vtx.Chi2(), vtx.NDF());//(TDHelper<float>::Chi2IProbability( ndf, chi2 ));
    vector<Double_t> dzPV;
    if(RtoMCPVId[iPV].IsMatched())
    {
      int iCurrMCPV = RtoMCPVId[iPV].GetBestMatch();
      for(int iPV2 = iPV+1; iPV2 < fTopoReconstructor->NPrimaryVertices(); iPV2++)
      {
        if(!RtoMCPVId[iPV2].IsMatched()) continue;
        int iCurrMCPV2 = RtoMCPVId[iPV2].GetBestMatch();
        if(iCurrMCPV != iCurrMCPV2) continue;
        KFParticle & vtx2 = fTopoReconstructor->GetPrimVertex(iPV2);
        
        dzPV.push_back(CAMath::Abs(vtx.Z() - vtx2.Z())); 
      }
    }
    
    hPVParam[ 0]->Fill(vtx.X());
    hPVParam[ 1]->Fill(vtx.Y());
    hPVParam[ 2]->Fill(vtx.Z());
    hPVParam[ 3]->Fill(tracks.size());    
    hPVParam[ 4]->Fill(vtx.Chi2());
    hPVParam[ 5]->Fill(vtx.NDF());
    hPVParam[ 6]->Fill(vtx.Chi2()/vtx.NDF());      
    hPVParam[ 7]->Fill(probPV);
    hPVParam[ 8]->Fill(fPVPurity[iPV]);
    hPVParam[ 9]->Fill(fPVTracksRate[0][iPV]);
    hPVParam[10]->Fill(fPVTracksRate[1][iPV]);
    hPVParam[11]->Fill(fPVTracksRate[2][iPV]);
    hPVParam[12]->Fill(fPVTracksRate[3][iPV]);
    for(unsigned int iZ=0; iZ<dzPV.size(); iZ++)
      hPVParam[13]->Fill(dzPV[iZ]);

    
    if(!RtoMCPVId[iPV].IsMatchedWithPdg())
    {
      if(!RtoMCPVId[iPV].IsMatched())
      {
        hPVParamGhost[ 0]->Fill(vtx.X());
        hPVParamGhost[ 1]->Fill(vtx.Y());
        hPVParamGhost[ 2]->Fill(vtx.Z());
        hPVParamGhost[ 3]->Fill(tracks.size());
        hPVParamGhost[ 4]->Fill(vtx.Chi2());
        hPVParamGhost[ 5]->Fill(vtx.NDF());
        hPVParamGhost[ 6]->Fill(vtx.Chi2()/vtx.NDF());
        hPVParamGhost[ 7]->Fill(probPV);
        hPVParamGhost[ 8]->Fill(fPVPurity[iPV]);
        hPVParamGhost[ 9]->Fill(fPVTracksRate[0][iPV]);
        hPVParamGhost[10]->Fill(fPVTracksRate[1][iPV]);
        hPVParamGhost[11]->Fill(fPVTracksRate[2][iPV]);
        hPVParamGhost[12]->Fill(fPVTracksRate[3][iPV]);
        for(unsigned int iZ=0; iZ<dzPV.size(); iZ++)
          hPVParamGhost[13]->Fill(dzPV[iZ]);
      }
      else
      {
        hPVParamBG[ 0]->Fill(vtx.X());
        hPVParamBG[ 1]->Fill(vtx.Y());
        hPVParamBG[ 2]->Fill(vtx.Z());
        hPVParamBG[ 3]->Fill(tracks.size());
        hPVParamBG[ 4]->Fill(vtx.Chi2());
        hPVParamBG[ 5]->Fill(vtx.NDF());
        hPVParamBG[ 6]->Fill(vtx.Chi2()/vtx.NDF());
        hPVParamBG[ 7]->Fill(probPV);
        hPVParamBG[ 8]->Fill(fPVPurity[iPV]);
        hPVParamBG[ 9]->Fill(fPVTracksRate[0][iPV]);
        hPVParamBG[10]->Fill(fPVTracksRate[1][iPV]);
        hPVParamBG[11]->Fill(fPVTracksRate[2][iPV]);
        hPVParamBG[12]->Fill(fPVTracksRate[3][iPV]);   
        for(unsigned int iZ=0; iZ<dzPV.size(); iZ++)
          hPVParamBG[13]->Fill(dzPV[iZ]);
      }
      continue;
    }
    
    int iMCPV = RtoMCPVId[iPV].GetBestMatch();
    KFMCVertex &mcPV = fPrimVertices[iMCPV]; // primary vertex positions (currently only one vertex is implemented)
    
    int iPVType = 0;
    if(mcPV.IsTriggerPV())
    {
      hPVParamSignal[ 0]->Fill(vtx.X());
      hPVParamSignal[ 1]->Fill(vtx.Y());
      hPVParamSignal[ 2]->Fill(vtx.Z());
      hPVParamSignal[ 3]->Fill(tracks.size());
      hPVParamSignal[ 4]->Fill(vtx.Chi2());
      hPVParamSignal[ 5]->Fill(vtx.NDF());
      hPVParamSignal[ 6]->Fill(vtx.Chi2()/vtx.NDF());
      hPVParamSignal[ 7]->Fill(probPV);
      hPVParamSignal[ 8]->Fill(fPVPurity[iPV]);
      hPVParamSignal[ 9]->Fill(fPVTracksRate[0][iPV]);
      hPVParamSignal[10]->Fill(fPVTracksRate[1][iPV]);
      hPVParamSignal[11]->Fill(fPVTracksRate[2][iPV]);
      hPVParamSignal[12]->Fill(fPVTracksRate[3][iPV]);
      for(unsigned int iZ=0; iZ<dzPV.size(); iZ++)
        hPVParamSignal[13]->Fill(dzPV[iZ]);
    }
    else
    {
      hPVParamPileup[ 0]->Fill(vtx.X());
      hPVParamPileup[ 1]->Fill(vtx.Y());
      hPVParamPileup[ 2]->Fill(vtx.Z());
      hPVParamPileup[ 3]->Fill(tracks.size());
      hPVParamPileup[ 4]->Fill(vtx.Chi2());
      hPVParamPileup[ 5]->Fill(vtx.NDF());
      hPVParamPileup[ 6]->Fill(vtx.Chi2()/vtx.NDF());
      hPVParamPileup[ 7]->Fill(probPV);
      hPVParamPileup[ 8]->Fill(fPVPurity[iPV]);
      hPVParamPileup[ 9]->Fill(fPVTracksRate[0][iPV]);
      hPVParamPileup[10]->Fill(fPVTracksRate[1][iPV]);
      hPVParamPileup[11]->Fill(fPVTracksRate[2][iPV]);
      hPVParamPileup[12]->Fill(fPVTracksRate[3][iPV]);
      for(unsigned int iZ=0; iZ<dzPV.size(); iZ++)
        hPVParamPileup[13]->Fill(dzPV[iZ]);
      iPVType = 1;
    }
    //Find MC parameters of the primary vertex 
    float mcPVx[3]={mcPV.X(), mcPV.Y(), mcPV.Z()};

    float errPV[3] = {vtx.CovarianceMatrix()[0], vtx.CovarianceMatrix()[2], vtx.CovarianceMatrix()[5]};
    for(int iErr=0; iErr<3; iErr++)
      if(CAMath::Abs(errPV[iErr]) < 1.e-8f) errPV[iErr] = 1.e8;
        
    float dRPVr[3] = {vtx.X()-mcPVx[0],
                      vtx.Y()-mcPVx[1],
                      vtx.Z()-mcPVx[2]};
    float dRPVp[3] = {dRPVr[0]/sqrt(errPV[0]),
                      dRPVr[1]/sqrt(errPV[1]),
                      dRPVr[2]/sqrt(errPV[2])};

    for(unsigned int iHPV=0; iHPV<3; ++iHPV)
      hPVFitQa[iPVType][iHPV]->Fill(dRPVr[iHPV]);
    for(unsigned int iHPV=3; iHPV<6; ++iHPV)
      hPVFitQa[iPVType][iHPV]->Fill(dRPVp[iHPV-3]);
    
    hPVFitQa[iPVType][6]->Fill( double(mcPV.NReconstructedDaughterTracks() - fNCorrectPVTracks[iPV])/double(mcPV.NReconstructedDaughterTracks()) );
  }
  
  //fill histograms with quality of input tracks from PV
  for(unsigned int iP=0; iP<fTopoReconstructor->GetParticles().size(); iP++)
  {
    KFParticle TempPart = fTopoReconstructor->GetParticles()[iP];
    int nDaughters = TempPart.NDaughters();
    if(nDaughters > 1) break; //use only tracks, not short lived particles
    
    if(!RtoMCParticleId[iP].IsMatchedWithPdg())  continue; //ghost
    
    int iMCPart = RtoMCParticleId[iP].GetBestMatchWithPdg();
    KFMCParticle &mcPart = vMCParticles[iMCPart];

    int iMCTrack = mcPart.GetMCTrackID();
    AliHLTTPCCAMCTrack &mcTrack = (*fMCTracks)[iMCTrack];
    
    if( mcTrack.MotherId() > -1 ) continue; // select only PV tracks
    
    const float mcX =  mcTrack.X();
    const float mcY =  mcTrack.Y();
    const float mcZ =  mcTrack.Z();
    const float mcPx = mcTrack.Px();
    const float mcPy = mcTrack.Py();
    const float mcPz = mcTrack.Pz();

    Double_t decayVtx[3] = {mcX, mcY, mcZ};
    TempPart.TransportToPoint(decayVtx);


    Double_t res[6] = {0}, 
             pull[6] = {0}, 
             mcParam[6] = { mcX, mcY, mcZ,
                            mcPx, mcPy, mcPz };
    for(int iPar=0; iPar < 6; iPar++ )
    {
      Double_t error = TempPart.GetCovariance(iPar,iPar);
      if(error < 0.) { error = 1.e20;}
      error = TMath::Sqrt(error);
      res[iPar]  = TempPart.GetParameter(iPar) - mcParam[iPar];
      if(fabs(error) > 1.e-20) pull[iPar] = res[iPar]/error;
      
      hFitPVTracksQA[iPar]->Fill(res[iPar]);
      hFitPVTracksQA[iPar+6]->Fill(pull[iPar]);
    }
  }
  
} // void KFTopoPerformance::FillHistos()

void KFTopoPerformance::AddV0Histos()
{
  int iV0 = fParteff.nParticles - 1;
  int iK0 = fParteff.GetParticleIndex(310);
  
  for(int iH=0; iH<nFitQA; iH++)
  {
    hFitDaughtersQA[iV0][iH]->Add(hFitDaughtersQA[iK0][iH]);
    hFitQA[iV0][iH]->Add(hFitQA[iK0][iH]);
  }

  for(int iH=0; iH<nHistoPartParam; iH++)
  {
    hPartParam[iV0][iH]->Add(hPartParam[iK0][iH]);
    hPartParamGhost[iV0][iH]->Add(hPartParamGhost[iK0][iH]);
    hPartParamSignal[iV0][iH]->Add(hPartParamSignal[iK0][iH]);
    hPartParamSignal[iV0][iH]->Add(hPartParamBG[iK0][iH]);
  }
}

#endif //DO_TPCCATRACKER_EFF_PERFORMANCE
