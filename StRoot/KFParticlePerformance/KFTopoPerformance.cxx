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

#ifdef KFPWITHTRACKER
#ifdef MAIN_DRAW
#include "AliHLTTPCCADisplay.h"
#endif //DRAW
#include "AliHLTTPCCATracker.h"
#include "AliHLTTPCCAPerformance.h"
#endif

#include "KFParticleTopoReconstructor.h"
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

KFTopoPerformance::KFTopoPerformance():KFParticlePerformanceBase(),fTopoReconstructor(0),fPrimVertices(0), fMCTrackToMCPVMatch(0), 
  fPVPurity(0), fNCorrectPVTracks(0), fTrackMatch(0), vMCTracks(0), vMCParticles(0), MCtoRParticleId(0), RtoMCParticleId(0), 
  MCtoRPVId(0), RtoMCPVId(0), fPrintEffFrequency(1)
{
}

KFTopoPerformance::~KFTopoPerformance()
{
}

#ifdef KFPWITHTRACKER
void KFTopoPerformance::SetNewEvent(
                            const AliHLTTPCCAGBTracker * const tracker,
                            AliHLTResizableArray<AliHLTTPCCAHitLabel> *hitLabels,
                            AliHLTResizableArray<AliHLTTPCCAMCTrack> *mcTracks,
                            AliHLTResizableArray<AliHLTTPCCALocalMCPoint> *localMCPoints)
{  
  vMCTracks.resize(mcTracks->Size());
  for(int iTr=0; iTr<vMCTracks.size(); iTr++)
  {
    for(int iP=0; iP<3; iP++)
      vMCTracks[iTr].SetPar(iP, (*mcTracks)[iTr].Par(iP));
    for(int iP=3; iP<6; iP++)
      vMCTracks[iTr].SetPar(iP, (*mcTracks)[iTr].Par(iP) * (*mcTracks)[iTr].P());
    vMCTracks[iTr].SetPar(6, (*mcTracks)[iTr].Par(6));
    
    vMCTracks[iTr].SetPDG( (*mcTracks)[iTr].PDG() );
    vMCTracks[iTr].SetMotherId( (*mcTracks)[iTr].MotherId() );
    vMCTracks[iTr].SetNMCPoints( (*mcTracks)[iTr].NMCPoints() );
  } 
} // void KFTopoPerformance::SetNewEvent
#endif

void KFTopoPerformance::SetTopoReconstructor( const KFParticleTopoReconstructor * const TopoReconstructor)
{  
  fTopoReconstructor = TopoReconstructor;
} // void KFTopoPerformance::SetTopoReconstructor

void KFTopoPerformance::CheckMCTracks()
{  
  fMCTrackToMCPVMatch.clear();
  fPrimVertices.clear();

    // find prim vertex
  if (vMCTracks.size() <= 0) return;
  
  fMCTrackToMCPVMatch.resize(vMCTracks.size(),-1);
  
  vector<int> pvIndex;
  for(unsigned int iTr=0; iTr<vMCTracks.size(); iTr++)
  {
    KFMCTrack &mctr = vMCTracks[iTr];
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
} // void KFTopoPerformance::CheckMCTracks()

void KFTopoPerformance::GetMCParticles()
{
  // convert MC tracks into KF MC Particles

  vMCParticles.clear();
  vMCParticles.reserve(vMCTracks.size());
  // all MC tracks are copied into KF MC Particles
  for(unsigned int iMC=0; iMC < vMCTracks.size(); iMC++)
  {
    KFMCTrack &mtra = vMCTracks[iMC];
    KFMCParticle part;
    part.SetMCTrackID( iMC );
    part.SetMotherId ( mtra.MotherId() );
    part.SetPDG      ( mtra.PDG() );
    vMCParticles.push_back( part );
  }
  // find relations between mother and daughter MC particles
  const int nMCParticles = vMCParticles.size();
  for (int iP = 0; iP < nMCParticles; iP++ ) {
    KFMCParticle &part = vMCParticles[iP];
    int motherId = part.GetMotherId();
    if(motherId < 0) continue;
    if(motherId >= nMCParticles)
    {
      std::cout << "ERROR!!!!!  KF Particle Performance: MC track mother Id is out of range." << std::endl;
      exit(1);
    }
    KFMCParticle &motherPart = vMCParticles[motherId];
    motherPart.AddDaughter(iP);
  }

  for(unsigned int iMC=0; iMC < vMCParticles.size(); iMC++)
  {
    KFMCParticle &part = vMCParticles[iMC];
    part.SetMCTrackID( iMC );
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
  if ( vMCTracks[part.GetMCTrackID()].IsOutOfDetector() ) return;
  
    // tracks
  if ( fabs(part.GetPDG()) ==        211 ||
       fabs(part.GetPDG()) ==       2212 ||
       fabs(part.GetPDG()) ==        321 ||
       fabs(part.GetPDG()) ==         11 ||
       fabs(part.GetPDG()) ==         13 ||
       fabs(part.GetPDG()) == 1000010020 ||
       fabs(part.GetPDG()) == 1000010030 ||
       fabs(part.GetPDG()) == 1000020030 ||
       fabs(part.GetPDG()) == 1000020040 ||
       ( (part.GetPDG() == 22) && (vMCTracks[part.GetMCTrackID()].IsReconstructed()) ) )
  {
    part.SetAsReconstructable(0);

    int iMCTrack = part.GetMCTrackID();
    KFMCTrack &mcTrack = vMCTracks[iMCTrack];
      
    if(mcTrack.NMCPoints() >= 15)
      part.SetAsReconstructable(1);   
//     if(mc.IsReconstructable())
//       part.SetAsReconstructable2();

    if(mcTrack.IsReconstructed())
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
    if(nD == 0) return; //TODO optimize for all species
    
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
    const KFParticle &rPart = fTopoReconstructor->GetParticles()[iRP];

    if (rPart.NDaughters() != 1) continue;
    
    const int rTrackId = rPart.DaughterIds()[0];
    const int mcTrackId = fTrackMatch[rTrackId];
    
    if(mcTrackId < 0) continue;

    KFMCParticle &mPart = vMCParticles[mcTrackId];
    if( mPart.GetPDG() == rPart.GetPDG() ) {
      MCtoRParticleId[mcTrackId].ids.push_back(iRP);
      RtoMCParticleId[iRP].ids.push_back(mcTrackId);
    }
    else {
      MCtoRParticleId[mcTrackId].idsMI.push_back(iRP);
      RtoMCParticleId[iRP].idsMI.push_back(mcTrackId);
    }
  }
  
  // match created mother particles
  for( unsigned int iRP = 0; iRP < fTopoReconstructor->GetParticles().size(); iRP++ ) {
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

    vector<int> &tracks = fTopoReconstructor->GetPVTrackIndexArray(iPV);
    
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
      KFMCTrack &mcTrack = vMCTracks[iMCTrack];
      
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
  }
}

void KFTopoPerformance::MatchTracks()
{
#ifdef KFPWITHTRACKER
  for(int iTr=0; iTr<vMCTracks.size(); iTr++)
  {
    if( (AliHLTTPCCAPerformance::Instance().GetSubPerformance("Global Performance")->GetMCData())[iTr].IsReconstructed() )
      vMCTracks[iTr].SetReconstructed();
  } 
  fTrackMatch.resize(AliHLTTPCCAPerformance::Instance().GetSubPerformance("Global Performance")->GetRecoData().size());
  for(int iTr=0; iTr<fTrackMatch.size(); iTr++)
  {
    const AliHLTTPCCAPerformanceRecoTrackData& matchInfo = (AliHLTTPCCAPerformance::Instance().GetSubPerformance("Global Performance")->GetRecoData())[iTr];
    if( matchInfo.IsGhost(PParameters::MinTrackPurity) || matchInfo.GetMCTrackId()<0  )
      fTrackMatch[iTr] = -1;
    else
      fTrackMatch[iTr] = matchInfo.GetMCTrackId();
  }
#endif

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
    if(abs(pdg) == 310  /*||
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
      for(unsigned int iPType=0; iPType<iParticle.size(); iPType++)
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
          KFMCTrack &mcTrack = vMCTracks[iMCTrack];
          
          TParticlePDG* particlePDG = TDatabasePDG::Instance()->GetParticle(mcTrack.PDG());
          Double_t massMC = (particlePDG) ? particlePDG->Mass() :0.13957;
          Double_t E = sqrt(mcTrack.P()*mcTrack.P() + massMC*massMC);
          Double_t Y = 0.5*log((E + mcTrack.Pz())/(E - mcTrack.Pz()));
          
          Double_t R = -1, L=-1;
          if(part.NDaughters() > 0)
          {
            int mcDaughterId = part.GetDaughterIds()[0];
            KFMCTrack &mcDaughter = vMCTracks[mcDaughterId];
            R = sqrt(mcDaughter.X()*mcDaughter.X() + mcDaughter.Y()*mcDaughter.Y());
            L = sqrt(mcDaughter.X()*mcDaughter.X() + mcDaughter.Y()*mcDaughter.Y());
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
  if(fNEvents%fPrintEffFrequency == 0)
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
        hPVefficiency[0][2]->Fill( vMCTracks.size(), isReco );
        hPVefficiency[0][3]->Fill( pvMC.NReconstructedDaughterTracks(), isReco );
        hPVefficiency[0][4]->Fill( NRecoPV, isReco );
        hPVefficiency[0][5]->Fill( nTracks, isReco );
      }
      else
      {
        pvEff.Inc(isReco, nClones, "PVpileup");
        hPVefficiency[1][0]->Fill( pvMC.NDaughterTracks(), isReco );
        hPVefficiency[1][1]->Fill( NMCPV, isReco );
        hPVefficiency[1][2]->Fill( vMCTracks.size(), isReco );
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
        hPVefficiency[2][2]->Fill( vMCTracks.size(), isReco );
        hPVefficiency[2][3]->Fill( pvMC.NReconstructedDaughterTracks(), isReco );
        hPVefficiency[2][4]->Fill( NRecoPV, isReco );
        hPVefficiency[2][5]->Fill( nTracks, isReco );
      }
      else
      {
        pvEffMCReconstructable.Inc(isReco, nClones, "PVpileup");
        hPVefficiency[3][0]->Fill( pvMC.NDaughterTracks(), isReco );
        hPVefficiency[3][1]->Fill( NMCPV, isReco );
        hPVefficiency[3][2]->Fill( vMCTracks.size(), isReco );
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
  if(fNEvents%fPrintEffFrequency == 0)
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
  vector<int> multiplicities(KFPartEfficiencies::nParticles, 0);
  vector<int> multiplicitiesGhost(KFPartEfficiencies::nParticles, 0);
  vector<int> multiplicitiesBG(KFPartEfficiencies::nParticles, 0);
  vector<int> multiplicitiesSignal(KFPartEfficiencies::nParticles, 0);
  
  //fill histograms for found short-lived particles
  for(unsigned int iP=0; iP<fTopoReconstructor->GetParticles().size(); iP++)
  {

    int iParticle = fParteff.GetParticleIndex(fTopoReconstructor->GetParticles()[iP].GetPDG());
    if(iParticle < 0) continue;
    
    float M, ErrM;
    float dL, ErrdL; // decay length
    float cT, ErrcT; // c*tau
    float P, ErrP;
    float Pt;
    float Rapidity;
    float Theta;
    float Phi;
    float X,Y,Z,R;
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
#ifdef CBM
    if(Z>=1. && iParticle>=46 && iParticle<=56) continue;
#endif
    R = sqrt(X*X+Y*Y);

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

    multiplicities[iParticle]++;

    hPartParam2D[iParticle][0]->Fill(Rapidity,Pt,1);
    
    bool drawZR = (iParticle<5) || (iParticle==41);
    if(drawZR)
      hPartParam2D[iParticle][1]->Fill(Z,R,1);

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
    
        multiplicitiesGhost[iParticle]++;
        
        hPartParam2DGhost[iParticle][0]->Fill(Rapidity,Pt,1);
        if(drawZR)
          hPartParam2DGhost[iParticle][1]->Fill(Z,R,1);
      }
      else
      {
        // for physical background

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

        multiplicitiesBG[iParticle]++;
        
        hPartParam2DBG[iParticle][0]->Fill(Rapidity,Pt,1);
        if(drawZR)
          hPartParam2DBG[iParticle][1]->Fill(Z,R,1);
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

    multiplicitiesSignal[iParticle]++;
    
    hPartParam2DSignal[iParticle][0]->Fill(Rapidity,Pt,1);
    if(drawZR)
      hPartParam2DSignal[iParticle][1]->Fill(Z,R,1);

    int iMCPart = RtoMCParticleId[iP].GetBestMatchWithPdg();
    KFMCParticle &mcPart = vMCParticles[iMCPart];
    // Fit quality of the mother particle
    {
      int iMCTrack = mcPart.GetMCTrackID();
      KFMCTrack &mcTrack = vMCTracks[iMCTrack];
      int mcDaughterId = -1;
      if(iParticle > 74 && iParticle <93)
        mcDaughterId = iMCTrack;
      else if(mcTrack.PDG() == 22 && TempPart.NDaughters() == 1)
        mcDaughterId = iMCTrack;
      else
        mcDaughterId = mcPart.GetDaughterIds()[0];
      KFMCTrack &mcDaughter = vMCTracks[mcDaughterId];

      const float mcX =  mcDaughter.X();
      const float mcY =  mcDaughter.Y();
      const float mcZ =  mcDaughter.Z();
      const float mcPx = mcTrack.Par(3);
      const float mcPy = mcTrack.Par(4);
      const float mcPz = mcTrack.Par(5);

      float decayVtx[3] = { mcTrack.X(), mcTrack.Y(), mcTrack.Z() };
      float recParam[8] = { 0 };
      float errParam[8] = { 0 };

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

      int jParticlePDG = fParteff.GetParticleIndex(mcTrack.PDG());      
      Double_t massMC = (jParticlePDG>=0) ? fParteff.partMass[jParticlePDG] :0.13957;
      
      Double_t Emc = sqrt(mcTrack.P()*mcTrack.P() + massMC*massMC);
      Double_t res[8] = {0}, 
               pull[8] = {0}, 
               mcParam[8] = { mcX, mcY, mcZ,
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
    }
    // Fit quality of daughters
    int daughterIndex[2] = {-1, -1};
    
    for(int iD=0; iD<mcPart.NDaughters(); ++iD)
    {
      int mcDaughterId = mcPart.GetDaughterIds()[iD];
//      if(!MCtoRParticleId[mcDaughterId].IsMatchedWithPdg()) continue;
      if(!MCtoRParticleId[mcDaughterId].IsMatched()) continue;
      KFMCTrack &mcTrack = vMCTracks[mcDaughterId];
//      int recDaughterId = MCtoRParticleId[mcDaughterId].GetBestMatchWithPdg();
      int recDaughterId = MCtoRParticleId[mcDaughterId].GetBestMatch();
      KFParticle Daughter = fTopoReconstructor->GetParticles()[recDaughterId];
      Daughter.GetMass(M,ErrM);

      const float mcX =  mcTrack.X();
      const float mcY =  mcTrack.Y();
      const float mcZ =  mcTrack.Z();
      const float mcPx = mcTrack.Px();
      const float mcPy = mcTrack.Py();
      const float mcPz = mcTrack.Pz();

      float_v decayVtx[3] = {mcX, mcY, mcZ};
      //Daughter.TransportToPoint(decayVtx);
      KFParticleSIMD DaughterSIMD(Daughter);
      
//       float decayVtxScalar[3] = {mcX, mcY, mcZ};
//       float dsdrscalar[6] = {0.f};
//       const float dSscalar = Daughter.GetDStoPoint(decayVtxScalar, dsdrscalar);
//       
//       float_v dsdrvector[6] = {0.f};
//       float_v dSvector = DaughterSIMD.GetDStoPoint(decayVtx, dsdrvector);
//       
//       //std::cout << "Scalar ds " << dSscalar  << " vector " << dSvector[0] << std::endl;
//       for(int iVVV=0; iVVV<6; iVVV++)
//         std::cout << "i " << iVVV << "  " << dsdrvector[iVVV][0] << " " <<  dsdrscalar[iVVV]<< " ";
//       std::cout << std::endl;
//       dSvector = dSscalar;
//       DaughterSIMD.TransportToDS(dSvector, dsdrvector);
      DaughterSIMD.TransportToPoint(decayVtx);
      
      int jParticlePDG = fParteff.GetParticleIndex(mcTrack.PDG());      
      Double_t massMC = (jParticlePDG>=0) ? fParteff.partMass[jParticlePDG] :0.13957;
      Double_t Emc = sqrt(mcTrack.P()*mcTrack.P() + massMC*massMC);
      
      Double_t res[8] = {0}, 
               pull[8] = {0}, 
               mcParam[8] = { mcX, mcY, mcZ,
                              mcPx, mcPy, mcPz, Emc, massMC };
      for(int iPar=0; iPar < 7; iPar++ )
      {
        Double_t error = DaughterSIMD.GetCovariance(iPar,iPar)[0];
        if(error < 0.) { error = 1.e20;}
        error = TMath::Sqrt(error);
        Double_t recoPar = DaughterSIMD.GetParameter(iPar)[0];
        res[iPar]  = recoPar - mcParam[iPar];
        if(fabs(error) > 1.e-20) pull[iPar] = res[iPar]/error;
      }
      res[7] = M - mcParam[7];
      if(fabs(ErrM) > 1.e-20) pull[7] = res[7]/ErrM;

      for(int iPar=0; iPar < 8; iPar++ )
      {
        hFitDaughtersQA[iParticle][iPar]->Fill(res[iPar]);
        hFitDaughtersQA[iParticle][iPar+8]->Fill(pull[iPar]);
      }
      
      //fill Histos for GetDStoParticle
      if(iD == 0)
        daughterIndex[0] = recDaughterId;
      if(iD == 1 && daughterIndex[0] > -1)
      {
        daughterIndex[1] = recDaughterId;
        KFParticle d1 = fTopoReconstructor->GetParticles()[daughterIndex[0]];
        KFParticle d2 = fTopoReconstructor->GetParticles()[daughterIndex[1]];
        
        KFParticleSIMD daughters[2] = {d2, d1};
        
        float_v dS[2] = {0.f, 0.f};
        float_v dsdr[4][6];
        for(int i1=0; i1<4; i1++)
          for(int i2=0; i2<6; i2++)
            dsdr[i1][i2] = 0.f;
          
        daughters[0].GetDStoParticle(daughters[1], dS, dsdr);
        float_v pD[2][8], cD[2][36], corrPD[2][36], corrCD[2][36];
        
        for(int iDR=0; iDR<2; iDR++)
        {
          for(int iPD = 0; iPD<8; iPD++)
          {
            pD[iDR][iPD] = 0;
            corrPD[iDR][iPD] = 0;
          }
          for(int iCD=0; iCD<36; iCD++)
          {
            cD[iDR][iCD] = 0;
            corrCD[iDR][iCD] = 0;
          }
        }
        
        float_v F[4][36];
        {
          for(int i1=0; i1<4; i1++)
            for(int i2=0; i2<36; i2++)
                F[i1][i2] = 0;
        }
        daughters[0].Transport(dS[0], dsdr[0], pD[0], cD[0], dsdr[1], F[0], F[1]);
        daughters[1].Transport(dS[1], dsdr[3], pD[1], cD[1], dsdr[2], F[3], F[2]);
        
        daughters[0].MultQSQt( F[1], daughters[1].CovarianceMatrix(), corrCD[0], 6);
        daughters[0].MultQSQt( F[2], daughters[0].CovarianceMatrix(), corrCD[1], 6);
        for(int iDR=0; iDR<2; iDR++)
          for(int iC=0; iC<6; iC++)
            cD[iDR][iC] += corrCD[iDR][iC];
        
//         for(int iDR=0; iDR<2; iDR++)
//         {
//           float_v vtxMC[3] = {decayVtx[0][0], decayVtx[1][0], decayVtx[2][0]};
//           daughters[iDR].TransportToPoint(vtxMC);
// //           daughters[iDR].TransportToPoint(vtx);
//           pD[iDR][0] = daughters[iDR].X();
//           pD[iDR][1] = daughters[iDR].Y();
//           pD[iDR][2] = daughters[iDR].Z();
//           cD[iDR][0] = daughters[iDR].GetCovariance(0,0);
//           cD[iDR][1] = daughters[iDR].GetCovariance(1,1);
//           cD[iDR][2] = daughters[iDR].GetCovariance(2,2);
//         }

        for(int iDR=0; iDR<2; iDR++)
        {
          cD[iDR][1] = cD[iDR][2];
          cD[iDR][2] = cD[iDR][5];
          for(int iPar=0; iPar<3; iPar++)
          {
            res[iPar] = pD[iDR][iPar][0] - decayVtx[iPar][0];
            
            Double_t error = cD[iDR][iPar][0];
            if(error < 0.) { error = 1.e20;}
            error = sqrt(error);
            
            pull[iPar] = res[iPar] / error;
            
            hDSToParticleQA[iParticle][iPar]->Fill(res[iPar]);
            hDSToParticleQA[iParticle][iPar+3]->Fill(pull[iPar]);
          }
        }
        
        Double_t dXds = pD[0][0][0] - pD[1][0][0];
        Double_t dYds = pD[0][1][0] - pD[1][1][0];
        Double_t dZds = pD[0][2][0] - pD[1][2][0];
        
        Double_t dRds = sqrt(dXds*dXds + dYds*dYds + dZds*dZds);
        hDSToParticleQA[iParticle][6]->Fill(dRds);
      }
    }
  }
  
  //fill histograms with ChiPrim for every particle
//   if(fTopoReconstructor->GetTracks())
//   {
//     for(int iTV=0; iTV<2; iTV++)
//     {
//       const kfvector_float& chiPrim = fTopoReconstructor->GetChiPrim()[iTV];
//       const KFPTrackVector& tracks  = fTopoReconstructor->GetTracks()[iTV];
//     
//       for(int iTr = 0; iTr<tracks.Size(); iTr++)
//       {
//         int iP = tracks.Id()[iTr];
//         if(iP < 0) continue;
// 
//         int iMC = RtoMCParticleId[iP].GetBestMatch();
//         if(iMC < 0) 
//         {
//           hTrackParameters[KFPartEfficiencies::nParticles]->Fill(chiPrim[iTr] );
//           continue;
//         }
//         KFMCParticle &mPart = vMCParticles[ iMC ];
//         if(mPart.GetMotherId() < 0)
//         {
//           hTrackParameters[KFPartEfficiencies::nParticles]->Fill(chiPrim[iTr] );
//           continue;
//         }
//         KFMCParticle &mMotherPart = vMCParticles[mPart.GetMotherId()];
//         int iParticle = fParteff.GetParticleIndex(mMotherPart.GetPDG());
//         if(iParticle > -1 && iParticle<KFPartEfficiencies::nParticles)
//           hTrackParameters[iParticle]->Fill(chiPrim[iTr] );
//       }
//     }
//   }

  //fill histograms with ChiPrim for every particle
  for(unsigned int iP=0; iP<fTopoReconstructor->GetParticles().size(); iP++)
  {
    KFParticle TempPart = fTopoReconstructor->GetParticles()[iP];
    KFParticle & vtx = fTopoReconstructor->GetPrimVertex(0);
    float chi2 = TempPart.GetDeviationFromVertex(vtx);
    int ndf = 2;
    
    hTrackParameters[KFPartEfficiencies::nParticles]->Fill(chi2);
    hTrackParameters[KFPartEfficiencies::nParticles+4]->Fill(TMath::Prob(chi2, ndf));
    
    if(!RtoMCParticleId[iP].IsMatched()) 
    {
      hTrackParameters[KFPartEfficiencies::nParticles+3]->Fill(chi2);
      hTrackParameters[KFPartEfficiencies::nParticles+7]->Fill(TMath::Prob(chi2, ndf));
      continue;
    }
    
    int iMCPart = RtoMCParticleId[iP].GetBestMatch();
    KFMCParticle &mcPart = vMCParticles[iMCPart];
    if(mcPart.GetMotherId() < 0)
    {
      hTrackParameters[KFPartEfficiencies::nParticles+1]->Fill(chi2 );
      hTrackParameters[KFPartEfficiencies::nParticles+5]->Fill(TMath::Prob(chi2, ndf));
    }
    else
    {
      hTrackParameters[KFPartEfficiencies::nParticles+2]->Fill(chi2 );
      hTrackParameters[KFPartEfficiencies::nParticles+6]->Fill(TMath::Prob(chi2, ndf));
    }    
    int iParticle = fParteff.GetParticleIndex(fTopoReconstructor->GetParticles()[iP].GetPDG());
    if(iParticle > -1 && iParticle<KFPartEfficiencies::nParticles)
      hTrackParameters[iParticle]->Fill(chi2 );
  }
  
  
  //fill histograms of the primary vertex quality
  for(int iPV = 0; iPV<fTopoReconstructor->NPrimaryVertices(); iPV++)
  {
    KFParticle & vtx = fTopoReconstructor->GetPrimVertex(iPV);
    vector<int> &tracks = fTopoReconstructor->GetPVTrackIndexArray(iPV);

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
        
        dzPV.push_back(fabs(vtx.Z() - vtx2.Z())); 
      }
    }
    
    hPVParam[ 0]->Fill(vtx.X());
    hPVParam[ 1]->Fill(vtx.Y());
    hPVParam[ 2]->Fill(vtx.Z());
    hPVParam[ 3]->Fill(sqrt(vtx.X()*vtx.X() + vtx.Y()*vtx.Y()));
    hPVParam[ 4]->Fill(tracks.size());    
    hPVParam[ 5]->Fill(vtx.Chi2());
    hPVParam[ 6]->Fill(vtx.NDF());
    hPVParam[ 7]->Fill(vtx.Chi2()/vtx.NDF());      
    hPVParam[ 8]->Fill(probPV);
    hPVParam[ 9]->Fill(fPVPurity[iPV]);
    hPVParam[10]->Fill(fPVTracksRate[0][iPV]);
    hPVParam[11]->Fill(fPVTracksRate[1][iPV]);
    hPVParam[12]->Fill(fPVTracksRate[2][iPV]);
    hPVParam[13]->Fill(fPVTracksRate[3][iPV]);
    for(unsigned int iZ=0; iZ<dzPV.size(); iZ++)
      hPVParam[14]->Fill(dzPV[iZ]);

    hPVParam2D[0]->Fill(vtx.X(),vtx.Y());

    
    if(!RtoMCPVId[iPV].IsMatchedWithPdg())
    {
      if(!RtoMCPVId[iPV].IsMatched())
      {
        hPVParamGhost[ 0]->Fill(vtx.X());
        hPVParamGhost[ 1]->Fill(vtx.Y());
        hPVParamGhost[ 2]->Fill(vtx.Z());
        hPVParamGhost[ 3]->Fill(sqrt(vtx.X()*vtx.X() + vtx.Y()*vtx.Y()));
        hPVParamGhost[ 4]->Fill(tracks.size());    
        hPVParamGhost[ 5]->Fill(vtx.Chi2());
        hPVParamGhost[ 6]->Fill(vtx.NDF());
        hPVParamGhost[ 7]->Fill(vtx.Chi2()/vtx.NDF());      
        hPVParamGhost[ 8]->Fill(probPV);
        hPVParamGhost[ 9]->Fill(fPVPurity[iPV]);
        hPVParamGhost[10]->Fill(fPVTracksRate[0][iPV]);
        hPVParamGhost[11]->Fill(fPVTracksRate[1][iPV]);
        hPVParamGhost[12]->Fill(fPVTracksRate[2][iPV]);
        hPVParamGhost[13]->Fill(fPVTracksRate[3][iPV]);
        for(unsigned int iZ=0; iZ<dzPV.size(); iZ++)
          hPVParamGhost[14]->Fill(dzPV[iZ]);
      }
      else
      {
        hPVParamBG[ 0]->Fill(vtx.X());
        hPVParamBG[ 1]->Fill(vtx.Y());
        hPVParamBG[ 2]->Fill(vtx.Z());
        hPVParamBG[ 3]->Fill(sqrt(vtx.X()*vtx.X() + vtx.Y()*vtx.Y()));
        hPVParamBG[ 4]->Fill(tracks.size());    
        hPVParamBG[ 5]->Fill(vtx.Chi2());
        hPVParamBG[ 6]->Fill(vtx.NDF());
        hPVParamBG[ 7]->Fill(vtx.Chi2()/vtx.NDF());      
        hPVParamBG[ 8]->Fill(probPV);
        hPVParamBG[ 9]->Fill(fPVPurity[iPV]);
        hPVParamBG[10]->Fill(fPVTracksRate[0][iPV]);
        hPVParamBG[11]->Fill(fPVTracksRate[1][iPV]);
        hPVParamBG[12]->Fill(fPVTracksRate[2][iPV]);
        hPVParamBG[13]->Fill(fPVTracksRate[3][iPV]);
        for(unsigned int iZ=0; iZ<dzPV.size(); iZ++)
          hPVParamBG[14]->Fill(dzPV[iZ]);
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
      hPVParamSignal[ 3]->Fill(sqrt(vtx.X()*vtx.X() + vtx.Y()*vtx.Y()));
      hPVParamSignal[ 4]->Fill(tracks.size());    
      hPVParamSignal[ 5]->Fill(vtx.Chi2());
      hPVParamSignal[ 6]->Fill(vtx.NDF());
      hPVParamSignal[ 7]->Fill(vtx.Chi2()/vtx.NDF());      
      hPVParamSignal[ 8]->Fill(probPV);
      hPVParamSignal[ 9]->Fill(fPVPurity[iPV]);
      hPVParamSignal[10]->Fill(fPVTracksRate[0][iPV]);
      hPVParamSignal[11]->Fill(fPVTracksRate[1][iPV]);
      hPVParamSignal[12]->Fill(fPVTracksRate[2][iPV]);
      hPVParamSignal[13]->Fill(fPVTracksRate[3][iPV]);
      for(unsigned int iZ=0; iZ<dzPV.size(); iZ++)
        hPVParamSignal[14]->Fill(dzPV[iZ]);
    }
    else
    {
      hPVParamPileup[ 0]->Fill(vtx.X());
      hPVParamPileup[ 1]->Fill(vtx.Y());
      hPVParamPileup[ 2]->Fill(vtx.Z());
      hPVParamPileup[ 3]->Fill(sqrt(vtx.X()*vtx.X() + vtx.Y()*vtx.Y()));
      hPVParamPileup[ 4]->Fill(tracks.size());    
      hPVParamPileup[ 5]->Fill(vtx.Chi2());
      hPVParamPileup[ 6]->Fill(vtx.NDF());
      hPVParamPileup[ 7]->Fill(vtx.Chi2()/vtx.NDF());      
      hPVParamPileup[ 8]->Fill(probPV);
      hPVParamPileup[ 9]->Fill(fPVPurity[iPV]);
      hPVParamPileup[10]->Fill(fPVTracksRate[0][iPV]);
      hPVParamPileup[11]->Fill(fPVTracksRate[1][iPV]);
      hPVParamPileup[12]->Fill(fPVTracksRate[2][iPV]);
      hPVParamPileup[13]->Fill(fPVTracksRate[3][iPV]);
      for(unsigned int iZ=0; iZ<dzPV.size(); iZ++)
        hPVParamPileup[14]->Fill(dzPV[iZ]);
      iPVType = 1;
    }
    //Find MC parameters of the primary vertex 
    float mcPVx[3]={mcPV.X(), mcPV.Y(), mcPV.Z()};

    float errPV[3] = {vtx.CovarianceMatrix()[0], vtx.CovarianceMatrix()[2], vtx.CovarianceMatrix()[5]};
    for(int iErr=0; iErr<3; iErr++)
      if(fabs(errPV[iErr]) < 1.e-8f) errPV[iErr] = 1.e8;
        
    float dRPVr[3] = {vtx.X()-mcPVx[0],
                      vtx.Y()-mcPVx[1],
                      vtx.Z()-mcPVx[2]};
    float dRPVp[3] = {static_cast<float>(dRPVr[0]/sqrt(errPV[0])),
                      static_cast<float>(dRPVr[1]/sqrt(errPV[1])),
                      static_cast<float>(dRPVr[2]/sqrt(errPV[2]))};

    for(unsigned int iHPV=0; iHPV<3; ++iHPV)
      hPVFitQa[iPVType][iHPV]->Fill(dRPVr[iHPV]);
    for(unsigned int iHPV=3; iHPV<6; ++iHPV)
      hPVFitQa[iPVType][iHPV]->Fill(dRPVp[iHPV-3]);
    
    for(unsigned int iHPV=0; iHPV<3; ++iHPV)
      hPVFitQa2D[iPVType][1][iHPV]->Fill(fNCorrectPVTracks[iPV],dRPVr[iHPV]);
    for(unsigned int iHPV=3; iHPV<6; ++iHPV)
      hPVFitQa2D[iPVType][1][iHPV]->Fill(fNCorrectPVTracks[iPV],dRPVp[iHPV-3]);

    for(unsigned int iHPV=0; iHPV<3; ++iHPV)
      hPVFitQa2D[iPVType][0][iHPV]->Fill(mcPV.NReconstructedDaughterTracks(),dRPVr[iHPV]);
    for(unsigned int iHPV=3; iHPV<6; ++iHPV)
      hPVFitQa2D[iPVType][0][iHPV]->Fill(mcPV.NReconstructedDaughterTracks(),dRPVp[iHPV-3]);
    
    hPVFitQa[iPVType][6]->Fill( double(mcPV.NReconstructedDaughterTracks() - fNCorrectPVTracks[iPV])/double(mcPV.NReconstructedDaughterTracks()) );
  }
  
  //fill histograms with quality of input tracks from PV
  for(unsigned int iP=0; iP<fTopoReconstructor->GetParticles().size(); iP++)
  {
    KFParticle TempPart = fTopoReconstructor->GetParticles()[iP];
    int nDaughters = TempPart.NDaughters();
    if(nDaughters > 1) continue; //use only tracks, not short lived particles
    
    if(!RtoMCParticleId[iP].IsMatchedWithPdg())  continue; //ghost
    
    int iMCPart = RtoMCParticleId[iP].GetBestMatchWithPdg();
    KFMCParticle &mcPart = vMCParticles[iMCPart];

    int iMCTrack = mcPart.GetMCTrackID();
    KFMCTrack &mcTrack = vMCTracks[iMCTrack];
    
    if( mcTrack.MotherId() > -1 ) continue; // select only PV tracks
    
    const float mcX =  mcTrack.X();
    const float mcY =  mcTrack.Y();
    const float mcZ =  mcTrack.Z();
    const float mcPx = mcTrack.Px();
    const float mcPy = mcTrack.Py();
    const float mcPz = mcTrack.Pz();

    float decayVtx[3] = {mcX, mcY, mcZ};
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
  
  for(int iP=0; iP < KFPartEfficiencies::nParticles; iP++) {
    hPartParam[iP][16]->Fill(multiplicities[iP]);
    hPartParamGhost[iP][16]->Fill(multiplicitiesGhost[iP]);
    hPartParamBG[iP][16]->Fill(multiplicitiesBG[iP]);
    hPartParamSignal[iP][16]->Fill(multiplicitiesSignal[iP]);
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
