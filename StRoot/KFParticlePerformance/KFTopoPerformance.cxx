/*
 * This file is part of KFParticle package
 * Copyright (C) 2007-2019 FIAS Frankfurt Institute for Advanced Studies
 *               2007-2019 Goethe University of Frankfurt
 *               2007-2019 Ivan Kisel <I.Kisel@compeng.uni-frankfurt.de>
 *               2007-2019 Maksym Zyzak
 *
 * KFParticle is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * KFParticle is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

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
#include "KFPHistogram/KFPHistogram.h"
#include "TParticlePDG.h"
#include "TDatabasePDG.h"

#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TProfile.h"
#include "TProfile2D.h"

#include <map>
#include <algorithm>
using std::sort;
using std::vector;

KFTopoPerformance::KFTopoPerformance():KFParticlePerformanceBase(),fTopoReconstructor(0),fPrimVertices(0), fMCTrackToMCPVMatch(0), 
  fPVPurity(0), fNCorrectPVTracks(0), fTrackMatch(0), vMCTracks(0), vMCParticles(0), fNeutralIndex(0), MCtoRParticleId(0), RtoMCParticleId(0), 
  MCtoRPVId(0), RtoMCPVId(0), fPrintEffFrequency(1), fCentralityBin(-1), fCentralityWeight(0.f)
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
  /** Sets a pointer to the external KFParticleTopoReconstructor object. */
  fTopoReconstructor = TopoReconstructor;
} // void KFTopoPerformance::SetTopoReconstructor

void KFTopoPerformance::CheckMCTracks()
{
  /** Cleans Monte Carlo information on primary vertices, and refill it with the current event. */
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
  /** Fills information on relations between Monte Carlo particles. */

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
  
  fNeutralIndex.clear();
  fNeutralIndex.resize(nMCParticles, -1);
  
  for(unsigned int iMC=0; iMC < vMCParticles.size(); iMC++)
  {
    KFMCParticle &part = vMCParticles[iMC];
    part.SetMCTrackID( iMC );
  }
  
  const int NmmPDG = 7;
  vector<int> mmMotherPDG(NmmPDG); //PDG for particles found by the missing mass method
  vector<int> mmChargedDaughterPDG(NmmPDG);
  vector<int> mmNeutralDaughterPDG(NmmPDG);
  vector<int> newMotherPDG(NmmPDG);
  vector<int> newNeutralPDG(NmmPDG);
  
  mmMotherPDG[ 0] = 3112; mmChargedDaughterPDG[ 0] =  211; mmNeutralDaughterPDG[ 0] = 2112;
  mmMotherPDG[ 1] = 3222; mmChargedDaughterPDG[ 1] =  211; mmNeutralDaughterPDG[ 1] = 2112;
  mmMotherPDG[ 2] = 3312; mmChargedDaughterPDG[ 2] =  211; mmNeutralDaughterPDG[ 2] = 3122;
  mmMotherPDG[ 3] = 3334; mmChargedDaughterPDG[ 3] =  211; mmNeutralDaughterPDG[ 3] = 3322;
  mmMotherPDG[ 4] =  321; mmChargedDaughterPDG[ 4] =  211; mmNeutralDaughterPDG[ 4] =  111;
  mmMotherPDG[ 5] = 3334; mmChargedDaughterPDG[ 5] =  321; mmNeutralDaughterPDG[ 5] = 3122;
  mmMotherPDG[ 6] = 3222; mmChargedDaughterPDG[ 6] = 2212; mmNeutralDaughterPDG[ 6] =  111;
  
  newMotherPDG[ 0] = 7003112; newNeutralPDG[ 0] = 7002112;
  newMotherPDG[ 1] = 7003222; newNeutralPDG[ 1] = 8002112;
  newMotherPDG[ 2] = 7003312; newNeutralPDG[ 2] = 7003122;
  newMotherPDG[ 3] = 7003334; newNeutralPDG[ 3] = 7003322;
  newMotherPDG[ 4] = 9000321; newNeutralPDG[ 4] = 9000111;
  newMotherPDG[ 5] = 8003334; newNeutralPDG[ 5] = 8003122;
  newMotherPDG[ 6] = 8003222; newNeutralPDG[ 6] = 8000111;
    
  //add neutrinos, if they are not saved
  for(int iMC=0; iMC<nMCParticles; iMC++)
  {
    if( abs(vMCParticles[iMC].GetPDG()) == 211 || abs(vMCParticles[iMC].GetPDG()) == 321 )
    {
      int muonIndex = -1;
      for(int iD=0; iD<vMCParticles[iMC].NDaughters(); iD++)
        if( abs(vMCParticles[vMCParticles[iMC].GetDaughterIds()[iD]].GetPDG()) == 13 )
          muonIndex = vMCParticles[iMC].GetDaughterIds()[iD];

      if(muonIndex > -1)
      {
        int newPDG = 0;
        if(vMCParticles[iMC].GetPDG() >0)
          newPDG = vMCParticles[iMC].GetPDG() + 7000000;
        else
          newPDG = vMCParticles[iMC].GetPDG() - 7000000;
        KFMCParticle motherPart = vMCParticles[iMC];
        KFMCTrack motherTrack = vMCTracks[motherPart.GetMCTrackID()];
        motherTrack.SetPDG(newPDG);
        motherTrack.SetNotReconstructed();
        int newMotherIndex = vMCTracks.size();
        motherPart.SetPDG(newPDG);
        motherPart.SetMCTrackID(newMotherIndex);

        const KFMCParticle& daughterPart = vMCParticles[muonIndex];
        const KFMCTrack& daughterTrack = vMCTracks[daughterPart.GetMCTrackID()];

        int neutrinoPDG = 7000014;
        if(vMCParticles[iMC].GetPDG() == -211) neutrinoPDG = -7000014;
        if(vMCParticles[iMC].GetPDG() ==  321) neutrinoPDG =  8000014;
        if(vMCParticles[iMC].GetPDG() == -321) neutrinoPDG = -8000014;

        int neutrinoIndex = vMCTracks.size()+1;
        vMCParticles[iMC].AddDaughter(neutrinoIndex);
        
        fNeutralIndex[iMC] = neutrinoIndex;

        KFMCParticle neutrinoPart;
        KFMCTrack neutrinoTrack;
        neutrinoTrack.SetX(daughterTrack.X());
        neutrinoTrack.SetY(daughterTrack.Y());
        neutrinoTrack.SetZ(daughterTrack.Z());
        neutrinoTrack.SetPx(motherTrack.Px() - daughterTrack.Px());
        neutrinoTrack.SetPy(motherTrack.Py() - daughterTrack.Py());
        neutrinoTrack.SetPz(motherTrack.Pz() - daughterTrack.Pz());
        neutrinoTrack.SetQP(0);
        neutrinoTrack.SetMotherId(newMotherIndex);
        neutrinoTrack.SetPDG(neutrinoPDG);

        motherPart.CleanDaughters();
        motherPart.AddDaughter(muonIndex);
        motherPart.AddDaughter(neutrinoIndex);
        motherPart.SetInitialParticleId(iMC);
        vMCTracks.push_back(motherTrack);
        vMCParticles.push_back(motherPart);
        fNeutralIndex.push_back(-1);
        
        neutrinoPart.SetMCTrackID(neutrinoIndex);
        neutrinoPart.SetMotherId(newMotherIndex);
        neutrinoPart.SetPDG(neutrinoPDG);
        neutrinoPart.AddDaughter(iMC);
        neutrinoPart.AddDaughter(muonIndex);
        vMCTracks.push_back(neutrinoTrack);
        vMCParticles.push_back(neutrinoPart);
        fNeutralIndex.push_back(-1);
        
        vMCParticles[iMC].SetAsReconstructable(4);
        vMCParticles[muonIndex].SetAsReconstructable(4);
      }
    }
    // add sigmas, omegas, xis ...
    if( vMCParticles[iMC].NDaughters() >= 2 && 
        (abs(vMCParticles[iMC].GetPDG()) == 3112 || 
         abs(vMCParticles[iMC].GetPDG()) == 3222 || 
         abs(vMCParticles[iMC].GetPDG()) == 3312 || 
         abs(vMCParticles[iMC].GetPDG()) == 3334 || 
         abs(vMCParticles[iMC].GetPDG()) ==  321) )
    {
      int neutralDaughterId = -1, chargedDaughterId = -1;

      int newPDG = 0;
      int neutralPDG = 0;

      for(int iPDG=0; iPDG<NmmPDG; iPDG++)
      {
        if(abs(vMCParticles[iMC].GetPDG()) == mmMotherPDG[iPDG])
        {
          bool isDaughter[2] = {0,0};

          vector<float> xDaughter;
          vector<float> yDaughter;
          vector<float> zDaughter;
          vector< vector<int> > nDaughtersAtPoint;
          
          for(int iMCDaughter=0; iMCDaughter<vMCParticles[iMC].NDaughters(); iMCDaughter++)
          {
            bool isNewDecayPoint = 1;
            
            for(unsigned int iPoint=0; iPoint<xDaughter.size(); iPoint++)
            {
              float dx = fabs(vMCTracks[vMCParticles[iMC].GetDaughterIds()[iMCDaughter]].X() - xDaughter[iPoint]);
              float dy = fabs(vMCTracks[vMCParticles[iMC].GetDaughterIds()[iMCDaughter]].Y() - yDaughter[iPoint]);
              float dz = fabs(vMCTracks[vMCParticles[iMC].GetDaughterIds()[iMCDaughter]].Z() - zDaughter[iPoint]);
              
              bool isSamePoint = (dx < 1.e-5 && dy < 1.e-5 && dz < 1.e-5);
              if(isSamePoint)
                nDaughtersAtPoint[iPoint].push_back(iMCDaughter);
              
              isNewDecayPoint &= !isSamePoint;
            }
            
            if(isNewDecayPoint)
            {
              xDaughter.push_back(vMCTracks[vMCParticles[iMC].GetDaughterIds()[iMCDaughter]].X());
              yDaughter.push_back(vMCTracks[vMCParticles[iMC].GetDaughterIds()[iMCDaughter]].Y());
              zDaughter.push_back(vMCTracks[vMCParticles[iMC].GetDaughterIds()[iMCDaughter]].Z());
              vector<int> newPointIndex;
              newPointIndex.push_back(iMCDaughter);
              nDaughtersAtPoint.push_back(newPointIndex);
            }
          }
          
          for(unsigned int iPoint = 0; iPoint<nDaughtersAtPoint.size(); iPoint++)
          {
            if(nDaughtersAtPoint[iPoint].size() == 2)
            {
              for(unsigned int iDaughter=0; iDaughter<nDaughtersAtPoint[iPoint].size(); iDaughter++)
              {
                int iMCDaughter = nDaughtersAtPoint[iPoint][iDaughter];
                if(abs(vMCParticles[vMCParticles[iMC].GetDaughterIds()[iMCDaughter]].GetPDG()) == mmChargedDaughterPDG[iPDG])
                {
                  isDaughter[0] = 1;
                  chargedDaughterId = vMCParticles[iMC].GetDaughterIds()[iMCDaughter];
                }
                if(abs(vMCParticles[vMCParticles[iMC].GetDaughterIds()[iMCDaughter]].GetPDG()) == mmNeutralDaughterPDG[iPDG])
                {
                  isDaughter[1] = 1;
                  neutralDaughterId = vMCParticles[iMC].GetDaughterIds()[iMCDaughter];
                }
              }
              
              if(isDaughter[0] && isDaughter[1])
              {
                int signPDG = vMCParticles[iMC].GetPDG()/abs(vMCParticles[iMC].GetPDG());
                newPDG     = signPDG * newMotherPDG[iPDG];
                neutralPDG = signPDG * newNeutralPDG[iPDG];
              }
            }
          }
        }
      }

      if(newPDG != 0)
      {
        KFMCParticle motherPart = vMCParticles[iMC];
        KFMCTrack motherTrack = vMCTracks[motherPart.GetMCTrackID()];
        motherTrack.SetPDG(newPDG);
        motherTrack.SetNotReconstructed();
        int newMotherIndex = vMCTracks.size();
        motherPart.SetPDG(newPDG);
        motherPart.SetMCTrackID(newMotherIndex);

        int neutrinoIndex = vMCTracks.size()+1;
        fNeutralIndex[iMC] = neutrinoIndex;
        
        KFMCTrack neutralTrack = vMCTracks[neutralDaughterId];
        neutralTrack.SetMotherId(newMotherIndex);
        neutralTrack.SetPDG(neutralPDG);

        motherPart.CleanDaughters();
        motherPart.AddDaughter(chargedDaughterId);
        motherPart.AddDaughter(neutrinoIndex);
        motherPart.SetInitialParticleId(iMC);
        vMCTracks.push_back(motherTrack);
        vMCParticles.push_back(motherPart);
        fNeutralIndex.push_back(-1);
        
        KFMCParticle neutralPart;
        neutralPart.SetMCTrackID(neutrinoIndex);
        neutralPart.SetMotherId(newMotherIndex);
        neutralPart.SetPDG(neutralPDG);
        neutralPart.AddDaughter(iMC);
        neutralPart.AddDaughter(chargedDaughterId);
        vMCTracks.push_back(neutralTrack);
        vMCParticles.push_back(neutralPart);
        fNeutralIndex.push_back(-1);
        
        vMCParticles[iMC].SetAsReconstructable(4);
        vMCParticles[chargedDaughterId].SetAsReconstructable(4);
      }
    }
  }
  
  //clean Lambda c daughters
  for(unsigned int iMC=0; iMC < vMCParticles.size(); iMC++)
  {
    KFMCParticle &part = vMCParticles[iMC];

//     if(abs(part.GetPDG()) == 4122)
    {
      //add daughters into one pool
      vector<int> newDaughters;
      for(unsigned int iD=0; iD<part.GetDaughterIds().size(); iD++)
      {
        KFMCParticle &d = vMCParticles[part.GetDaughterIds()[iD]];
        if( abs(d.GetPDG())==3224 || 
            abs(d.GetPDG())==3114 || 
            abs(d.GetPDG())==113  ||
            abs(d.GetPDG())==313  ||
            abs(d.GetPDG())==323  ||
            abs(d.GetPDG())==2224 ||
            abs(d.GetPDG())==2214 ||
            abs(d.GetPDG())==2114 ||
            abs(d.GetPDG())==1114
          )
        {
          for(unsigned int iDaughter=0; iDaughter<d.GetDaughterIds().size(); iDaughter++)
          {
            newDaughters.push_back(d.GetDaughterIds()[iDaughter]);
            vMCParticles[d.GetDaughterIds()[iDaughter]].SetMotherId(iMC);
          }
        }
        else
//         if(d.GetDaughterIds().size() == 0 || abs(d.GetPDG())==310 || abs(d.GetPDG())==3122)
          newDaughters.push_back(part.GetDaughterIds()[iD]);
      }
      part.CleanDaughters();
      for(unsigned int iDaughter=0; iDaughter<newDaughters.size(); iDaughter++)
        part.AddDaughter(newDaughters[iDaughter]);
      
      //change PDG to separate channels
      int indexPDG = fParteff.GetParticleIndex(part.GetPDG());
      for(int iPDG=indexPDG; iPDG<indexPDG+10; iPDG++)
      {
        const int nDaughters = part.GetDaughterIds().size();
        
        if(int(fParteff.partDaughterPdg[iPDG].size()) != nDaughters)
          continue;
        
        vector<bool> isDaughterFound(nDaughters);
        vector<bool> isDaughterUsed(nDaughters);
        for(int iDMC=0; iDMC<nDaughters; iDMC++)
        {
          isDaughterFound[iDMC] = 0;
          isDaughterUsed[iDMC] = 0;
        }
        
        bool isCorrectPDG = 1;
        for(int iDMC=0; iDMC<nDaughters; iDMC++)
          for(int iD=0; iD<nDaughters; iD++)
          {
            if(isDaughterUsed[iD]) continue;
            if(vMCParticles[part.GetDaughterIds()[iD]].GetPDG() == fParteff.partDaughterPdg[iPDG][iDMC]) 
            {
              isDaughterUsed[iD] = 1;
              isDaughterFound[iDMC] = 1;
              break;
            }
          }

        for(int iDMC=0; iDMC<nDaughters; iDMC++)
          isCorrectPDG &= isDaughterFound[iDMC];
        
        if(isCorrectPDG)
        {
          part.SetPDG(fParteff.partPDG[iPDG]);
          break;
        }
      }
    }
  }
}

void KFTopoPerformance::FindReconstructableMCParticles()
{
  /** Check each Monte Carlo particle if it can be reconstructed. */
  const unsigned int nMCParticles = vMCParticles.size();

  for ( unsigned int iP = 0; iP < nMCParticles; iP++ ) {
    KFMCParticle &part = vMCParticles[iP];
    CheckMCParticleIsReconstructable(part);
  }
}

void KFTopoPerformance::CheckMCParticleIsReconstructable(KFMCParticle &part)
{
  /** Checks if the given Monte Carlo particle can be reconstructed. */
  if ( part.IsReconstructable(0) ) return;
  if ( vMCTracks[part.GetMCTrackID()].IsOutOfDetector() ) return;
  
  if( abs(part.GetPDG()) ==        211 ||
      abs(part.GetPDG()) ==       2212 ||
      abs(part.GetPDG()) ==        321 ||
      abs(part.GetPDG()) ==         13 ||
      abs(part.GetPDG()) ==       3112 ||
      abs(part.GetPDG()) ==       3222 ||
      abs(part.GetPDG()) ==       3312 ||
      abs(part.GetPDG()) ==       3334 )
  {
    int iMCTrack = part.GetMCTrackID();
    KFMCTrack &mcTrack = vMCTracks[iMCTrack];

    // reconstructable in 4pi is defined in GetMCParticles, when decay is found
    if(mcTrack.IsReconstructed())
      part.SetAsReconstructable(3);
  }
    // tracks
  if ( abs(part.GetPDG()) ==        211 ||
       abs(part.GetPDG()) ==       2212 ||
       abs(part.GetPDG()) ==        321 ||
       abs(part.GetPDG()) ==         11 ||
       abs(part.GetPDG()) ==         13 ||
       abs(part.GetPDG()) == 1000010020 ||
       abs(part.GetPDG()) == 1000010030 ||
       abs(part.GetPDG()) == 1000020030 ||
       abs(part.GetPDG()) == 1000020040 ||
       ( (part.GetPDG() == 22) && (vMCTracks[part.GetMCTrackID()].IsReconstructed()) ) )
  {
    int iMCTrack = part.GetMCTrackID();
    KFMCTrack &mcTrack = vMCTracks[iMCTrack];

    part.SetAsReconstructable(0);

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
      bool isPositiveDaughter[5] = {0,0,0,0,0};
      bool isNegativeDaughter[5] = {0,0,0,0,0};
      
      int nRecoDaughters[5] = {0,0,0,0,0};
      
      for(int iD=0; iD < part.NDaughters(); iD++)
      {
        KFMCParticle &daughter = vMCParticles[part.GetDaughterIds()[iD]];
        CheckMCParticleIsReconstructable(daughter);
        
        TParticlePDG* particlePDG = TDatabasePDG::Instance()->GetParticle(daughter.GetPDG());
        Double_t charge = (particlePDG) ? particlePDG->Charge()/3 : 0;
        
        for(int iEff=0; iEff<5; iEff++)
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
    bool reco4 = 1;
    bool reco5 = 1;
      
    for ( unsigned int iD = 0; iD < nD && (reco1 || reco2 || reco3 || reco4 || reco5); iD++ ) {
      KFMCParticle &dp = vMCParticles[dIds[iD]];
      CheckMCParticleIsReconstructable(dp);
      reco1 &= dp.IsReconstructable(0);
      reco2 &= dp.IsReconstructable(1);
      reco3 &= dp.IsReconstructable(2);
      reco4 &= dp.IsReconstructable(3);
      reco5 &= dp.IsReconstructable(4);
    }
    
    if (reco1) part.SetAsReconstructable(0);
    if (reco2) part.SetAsReconstructable(1);
    if (reco3) part.SetAsReconstructable(2);
    int iParticle = fParteff.GetParticleIndex(part.GetPDG());
    if (reco4 && iParticle>=KFPartEfficiencies::fFirstMissingMassParticleIndex &&
                 iParticle<=KFPartEfficiencies::fLastMissingMassParticleIndex ) part.SetAsReconstructable(3);
    if (reco5 && iParticle>=KFPartEfficiencies::fFirstMissingMassParticleIndex &&
                 iParticle<=KFPartEfficiencies::fLastMissingMassParticleIndex ) part.SetAsReconstructable(4);
  }
}


void KFTopoPerformance::FindReconstructableMCVertices()
{
  /** Checks which Monte Carlo primary vertices can be reconstructed. */
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
  /** Matches Monte Carlo and reconstructed particles. */
  MCtoRParticleId.clear();
  RtoMCParticleId.clear();
  MCtoRParticleId.resize(vMCParticles.size());
  RtoMCParticleId.resize(fTopoReconstructor->GetParticles().size() );

  // match tracks ( particles which are direct copies of tracks )
  for( unsigned int iRP = 0; iRP < fTopoReconstructor->GetParticles().size(); iRP++ ) 
  {
    const KFParticle &rPart = fTopoReconstructor->GetParticles()[iRP];

    if (rPart.NDaughters() != 1) continue;

    const int rTrackId = rPart.DaughterIds()[0];
    const int mcTrackId = fTrackMatch[rTrackId];

    if(mcTrackId < 0) continue;

    KFMCParticle &mPart = vMCParticles[mcTrackId];
    if( mPart.GetPDG() == rPart.GetPDG() ) 
    {
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

    bool isMissingMass = ((abs(rPart.GetPDG()) == 7000211)||(abs(rPart.GetPDG()) == 7000321)||(abs(rPart.GetPDG()) == 7003112) || (abs(rPart.GetPDG()) == 7003222)|| (abs(rPart.GetPDG()) == 7003312)|| (abs(rPart.GetPDG()) == 7003334)|| (abs(rPart.GetPDG()) == 9000321)|| (abs(rPart.GetPDG()) == 8003334)||(abs(rPart.GetPDG()) == 8003222));
    
    //missing mass method
    if ( (abs(rPart.GetPDG()) == 7000014 ) || (abs(rPart.GetPDG()) == 8000014 ) || (abs(rPart.GetPDG()) == 7002112 ) ||
         (abs(rPart.GetPDG()) == 8002112 ) || (abs(rPart.GetPDG()) == 7003122 ) || (abs(rPart.GetPDG()) == 7003322 ) ||
         (abs(rPart.GetPDG()) == 9000111 ) || (abs(rPart.GetPDG()) == 8003122 ) || (abs(rPart.GetPDG()) == 8000111 ) )
    {
      //During the reconstruction 1st daughter - mother particle, 2nd daughter - charged daughter

      int mcNeutralDaughterId = -1;
      const int recoMotherId = rPart.DaughterIds()[0];
      if ( !RtoMCParticleId[recoMotherId].IsMatched() ) continue;

      const int mcMotherId = RtoMCParticleId[recoMotherId].GetBestMatch();
      
      const int recoChargedDaughterId = rPart.DaughterIds()[1];
      if ( !RtoMCParticleId[recoChargedDaughterId].IsMatched() ) continue;

      const int mcChargedDaughterId = RtoMCParticleId[recoChargedDaughterId].GetBestMatch();
      const KFMCParticle& chargedDaughter = vMCParticles[mcChargedDaughterId];

      if(chargedDaughter.GetMotherId() != mcMotherId) continue;
      const KFMCParticle& mother = vMCParticles[mcMotherId];

      if(fNeutralIndex[mcMotherId] > -1)
        mcNeutralDaughterId = fNeutralIndex[mcMotherId];

      if(mcNeutralDaughterId > -1)
      {
        KFMCParticle &neutralDaughter = vMCParticles[mcNeutralDaughterId];
        
        int iParticle = fParteff.GetParticleIndex(rPart.GetPDG());
        
        bool allCorrectDaughters = mother.GetPDG()          == fParteff.partDaughterPdg[iParticle][0] &&
                                   chargedDaughter.GetPDG() == fParteff.partDaughterPdg[iParticle][1];

        if( neutralDaughter.GetPDG()     == rPart.GetPDG()     &&
            neutralDaughter.NDaughters() == rPart.NDaughters() &&
            allCorrectDaughters) {
          MCtoRParticleId[mcNeutralDaughterId].ids.push_back(iRP);
          RtoMCParticleId[iRP].ids.push_back(mcNeutralDaughterId);
        }
        else {
          MCtoRParticleId[mcNeutralDaughterId].idsMI.push_back(iRP);
          RtoMCParticleId[iRP].idsMI.push_back(mcNeutralDaughterId);
        }
      }
    }
    
    
    
    //normal decays
    else
    {
      unsigned int iD = 0;
      vector<int> mcDaughterIds;
      int mmId = -2; // MC id for rPart
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

        if(isMissingMass)
        {
          const KFMCParticle &neutralDaughter = vMCParticles[mdId];
          if(mmId != vMCParticles[neutralDaughter.GetMotherId()].InitialParticleId()) break;
          mmId = neutralDaughter.GetMotherId();
        }
        
        if( !(isMissingMass) && (vMCParticles[mdId].GetMotherId() != mmId) ) break;
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
}

void KFTopoPerformance::MatchPV()
{
  /** Matches Monte Carlo and reconstructed primary vertices. */
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
  /** Runs reading of Monte Carlo particles and vertices, their matching, calculation of efficiency. */
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
  /** Calculates reconstruction efficiency of short-lived particles. */
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
        partEff.IncReco(isGhost, isBG, fParteff.partName[iPart].data());
    
    // Calculate the gost level for V0
    if(abs(pdg) == 310  /*||
       CAMath::Abs(pdg) == 3122 ||
       CAMath::Abs(pdg) == 421  ||
       CAMath::Abs(pdg) == 22 */)
    {
      partEff.IncReco(isGhost, 0, fParteff.partName[fParteff.nParticles - 1].data());
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

    const std::map<int,bool>& decays = fTopoReconstructor->GetKFParticleFinder()->GetReconstructionList();
    if(!(decays.empty()) && (iParticle[0] < fParteff.fFirstStableParticleIndex || iParticle[0] > fParteff.fLastStableParticleIndex))
      if(decays.find(pdg) == decays.end()) continue;
        
    if( fParteff.GetParticleIndex(pdg)>=KFPartEfficiencies::fFirstMissingMassParticleIndex &&
        fParteff.GetParticleIndex(pdg)<=KFPartEfficiencies::fLastMissingMassParticleIndex )
    {
      isRecPart.push_back(part.IsReconstructable(4));
      isRecPart.push_back(part.IsReconstructable(1));
      isRecPart.push_back(part.IsReconstructable(3));
    }
    else
      for(int iEff = 0; iEff < 3; iEff++)
        isRecPart.push_back(part.IsReconstructable(iEff));
    
    isReconstructable.push_back(isRecPart);
    isReco.push_back( MCtoRParticleId[iP].ids.size() != 0 );
    nClones.push_back( MCtoRParticleId[iP].ids.size() - 1 );
    
    if(decays.empty() && (part.IsReconstructableV0(0) || part.IsReconstructableV0(1) || part.IsReconstructableV0(2)) )
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
          
        partEff.Inc(isReco[iPType], nClones[iPType], isReconstructable[iPType][0], isReconstructable[iPType][1], isReconstructable[iPType][2], fParteff.partName[iPart].data());
        if ( mId == -1 )
          partEff.Inc(isReco[iPType], nClones[iPType], isReconstructable[iPType][0], isReconstructable[iPType][1], isReconstructable[iPType][2], (fParteff.partName[iPart]+"_prim").data());
        else
          partEff.Inc(isReco[iPType], nClones[iPType], isReconstructable[iPType][0], isReconstructable[iPType][1], isReconstructable[iPType][2], (fParteff.partName[iPart]+"_sec").data());
        
        for(int iEff=0; iEff<3; iEff++)
        {
          if(!isReconstructable[iPType][iEff]) continue;
          
          int iMCTrack = part.GetMCTrackID();
          KFMCTrack &mcTrack = vMCTracks[iMCTrack];
          
          Double_t massMC = fParteff.partMass[iPart];
          Double_t E = sqrt(mcTrack.P()*mcTrack.P() + massMC*massMC);
          Double_t Y = 0.5*log((E + mcTrack.Pz())/(E - mcTrack.Pz()));
          Double_t Z = mcTrack.Z();
          Double_t R = -1, L=-1;
          Double_t Mt_mc = sqrt(mcTrack.Pt()*mcTrack.Pt()+massMC*massMC)-massMC;
          Double_t cT = -1.e10;
          Double_t decayLength = -1.e10;
          
          if(part.NDaughters() > 0)
          {
            int mcDaughterId = part.GetDaughterIds()[0];
            KFMCTrack &mcDaughter = vMCTracks[mcDaughterId];
            R = sqrt(mcDaughter.X()*mcDaughter.X() + mcDaughter.Y()*mcDaughter.Y());
            L = sqrt(mcDaughter.X()*mcDaughter.X() + mcDaughter.Y()*mcDaughter.Y());
            Z = mcDaughter.Z();
            
            if(mcTrack.MotherId() < 0)
            {
              KFParticle motherKFParticle;
              float decayPoint[3] = { mcDaughter.X(), mcDaughter.Y(), mcDaughter.Z() };
              for(int iP=0; iP<6; iP++)
                motherKFParticle.Parameter(iP) = mcTrack.Par()[iP];
              
              float dsdr[6];
              double s = motherKFParticle.GetDStoPoint(decayPoint, dsdr);
              int jParticlePDG = fParteff.GetParticleIndex(mcTrack.PDG());      
              Double_t massMC = (jParticlePDG>=0) ? fParteff.partMass[jParticlePDG] :0.13957;
              
              cT = s*massMC;
              decayLength = s*mcTrack.P();
            }
          }

          if(fStoreMCHistograms)
          {
            hPartEfficiency[iPart][iEff][0]->Fill( mcTrack.P(), isReco[iPType] );
            hPartEfficiency[iPart][iEff][1]->Fill( mcTrack.Pt(), isReco[iPType] );
            hPartEfficiency[iPart][iEff][2]->Fill( Y, isReco[iPType] );
            hPartEfficiency[iPart][iEff][3]->Fill( Z, isReco[iPType] );
            if(cT > -1.e10)          hPartEfficiency[iPart][iEff][4]->Fill( cT, isReco[iPType] );
            if(decayLength > -1.e10) hPartEfficiency[iPart][iEff][5]->Fill( decayLength, isReco[iPType] );
            hPartEfficiency[iPart][iEff][3]->Fill( Z, isReco[iPType] );
            hPartEfficiency[iPart][iEff][6]->Fill( L, isReco[iPType] );
            hPartEfficiency[iPart][iEff][7]->Fill( R, isReco[iPType] );
            hPartEfficiency[iPart][iEff][8]->Fill( Mt_mc, isReco[iPType] );
            
            hPartEfficiency2D[iPart][iEff][0]->Fill( Y, mcTrack.Pt(), isReco[iPType] );
            hPartEfficiency2D[iPart][iEff][1]->Fill( Y, Mt_mc, isReco[iPType] );
          }
        }
      }
    }
  }

  fNEvents++;

  fParteff += partEff;

  partEff.CalcEff();
  fParteff.CalcEff();

  if(fNEvents%fPrintEffFrequency == 0)
  {
    std::cout << " ---- KF Particle finder --- " << std::endl;
    std::cout << "ACCUMULATED STAT    : " << fNEvents << " EVENTS "               << std::endl << std::endl;
    fParteff.PrintEff();
    std::cout<<std::endl;
  }
}

void KFTopoPerformance::CalculatePVEfficiency()
{
  /** Calculates reconstruction efficiency of primary vertices. */
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

  if(fNEvents%fPrintEffFrequency == 0)
  {
    std::cout << " ---- KF PV finder --- " << std::endl;
    std::cout << "ACCUMULATED STAT    : " << fNEvents << " EVENTS "               << std::endl << std::endl;
    std::cout << "PV with at least 2 reconstructed tracks is reconstructable:" << std::endl;
    fPVeff.PrintEff();
    std::cout << std::endl;
    std::cout << "PV with at least 2 MC tracks with 15 MC points is reconstructable:" << std::endl;
    fPVeffMCReconstructable.PrintEff();

    std::cout<<std::endl;
  }
}

void KFTopoPerformance::FillParticleParameters(KFParticle& TempPart,
                                               int iParticle,
                                               int iP,
                                               int iPV,
                                               TH1F* histoParameters[nParametersSet][KFPartEfficiencies::nParticles][nHistoPartParam],
                                               TH2F* histoParameters2D[nParametersSet][KFPartEfficiencies::nParticles][nHistoPartParam2D],
                                               TH3F* histoParameters3D[1][KFPartEfficiencies::nParticles][nHistoPartParam3D],
                                               TH1F* histoFit[KFPartEfficiencies::nParticles][nFitQA],
                                               TH1F* histoFitDaughtersQA[KFPartEfficiencies::nParticles][nFitQA],
                                               TH1F* histoDSToParticleQA[KFPartEfficiencies::nParticles][nDSToParticleQA],
                                               vector<int>* multiplicities)
{
  /** Fills provided histograms with the parameters of the given particle. */
  
  const std::map<int,bool>& decays = fTopoReconstructor->GetKFParticleFinder()->GetReconstructionList();
  if(!(decays.empty()) && (iParticle < fParteff.fFirstStableParticleIndex || iParticle > fParteff.fLastStableParticleIndex))
    if(decays.find(TempPart.GetPDG()) == decays.end()) return;
    
  float M, M_t, ErrM;
  float dL, ErrdL; // decay length
  float cT, ErrcT; // c*tau
  float P, ErrP;
  float Pt;
  float Rapidity;
  float Theta;
  float Phi;
  float X,Y,Z,R;
  float QtAlpha[2];
    
  TempPart.GetMass(M,ErrM);
  TempPart.GetMomentum(P,ErrP);
  Pt = TempPart.GetPt();
  Rapidity = TempPart.GetRapidity();
  
  KFParticle TempPartTopo = TempPart;
  TempPartTopo.SetProductionVertex(fTopoReconstructor->GetPrimVertex(0));
  TempPartTopo.GetDecayLength(dL,ErrdL);
  TempPartTopo.GetLifeTime(cT,ErrcT);
  
  float chi2 = TempPart.GetChi2();
  Int_t ndf = TempPart.GetNDF();
  float prob = TMath::Prob(chi2, ndf);//(TDHelper<float>::Chi2IProbability( ndf, chi2 ));
  Theta = TempPart.GetTheta();
  Phi = TempPart.GetPhi();
  X = TempPart.GetX();
  Y = TempPart.GetY();
  Z = TempPart.GetZ();
#ifdef CBM
  if(Z>=1. && iParticle>=54 && iParticle<=64) return;
#endif
  R = sqrt(X*X+Y*Y);
  M_t = sqrt(Pt*Pt+fParteff.GetMass(iParticle)*fParteff.GetMass(iParticle))-fParteff.GetMass(iParticle);
  
  KFParticleSIMD tempSIMDPart(TempPart);
  float_v l,dl;
  KFParticleSIMD pv(fTopoReconstructor->GetPrimVertex(iPV));
  tempSIMDPart.GetDistanceToVertexLine(pv, l, dl);
#ifdef __ROOT__
  if( (l[0] > 0.2f || Pt < 0.f) && (abs( TempPart.GetPDG() ) ==   4122 ||
                                    abs( TempPart.GetPDG() ) == 104122 ||
                                    abs( TempPart.GetPDG() ) == 204122 ||
                                    abs( TempPart.GetPDG() ) == 304122 ||
                                    abs( TempPart.GetPDG() ) == 404122 || 
                                    abs( TempPart.GetPDG() ) == 504122 )  ) return;
  if( (l[0] > 0.2f || Pt < 0.f) && (abs( TempPart.GetPDG() ) == 421 ||
                                    abs( TempPart.GetPDG() ) == 420 ||
                                    abs( TempPart.GetPDG() ) == 425 ||
                                    abs( TempPart.GetPDG() ) == 426 ||
                                    abs( TempPart.GetPDG() ) == 427 ||
                                    abs( TempPart.GetPDG() ) == 429)  ) return;
  if( (l[0] > 0.4f || Pt < 0.f) && (abs( TempPart.GetPDG() ) ==    411 ||
                                    abs( TempPart.GetPDG() ) == 100411 ||
                                    abs( TempPart.GetPDG() ) == 200411 ||
                                    abs( TempPart.GetPDG() ) == 300411)  ) return;
  if( (l[0] > 0.2f || Pt < 0.f) && (abs( TempPart.GetPDG() ) ==    431 ||
                                    abs( TempPart.GetPDG() ) == 100431 ||
                                    abs( TempPart.GetPDG() ) == 200431 ||
                                    abs( TempPart.GetPDG() ) == 300431 ||
                                    abs( TempPart.GetPDG() ) == 400431)  ) return;
  
//   if(Pt < 2. && (abs( TempPart.GetPDG() ) ==    443 ||
//                  abs( TempPart.GetPDG() ) == 100443 ||
//                  abs( TempPart.GetPDG() ) == 200443 ||
//                  abs( TempPart.GetPDG() ) == 300443 ||
//                  abs( TempPart.GetPDG() ) == 400443 ||
//                  abs( TempPart.GetPDG() ) == 500443) ) return;
  
  if(Pt < 0.5f && (abs( TempPart.GetPDG() ) == 3000 ||
                   abs( TempPart.GetPDG() ) == 3001) ) return;
#endif
  float parameters[17] = {M, P, Pt, Rapidity, dL, cT, chi2/ndf, prob, Theta, Phi, X, Y, Z, R, l[0], l[0]/dl[0], M_t };

  //for all particle-candidates
  for(int iParam=0; iParam<17; iParam++)
    histoParameters[0][iParticle][iParam]->Fill(parameters[iParam]);

  if(multiplicities)
    multiplicities[0][iParticle]++;

  histoParameters2D[0][iParticle][0]->Fill(Rapidity,Pt,1);
  histoParameters2D[0][iParticle][3]->Fill(Rapidity,M_t,1);
  
  const bool drawZR = IsCollectZRHistogram(iParticle);
  if(histoParameters2D[0][iParticle][1] && drawZR)
  {
    histoParameters2D[0][iParticle][1]->Fill(Z,R,1);
  }

  if(TempPart.NDaughters() == 2 && IsCollectArmenteros(iParticle))
  {
    int index1 = TempPart.DaughterIds()[0];
    int index2 = TempPart.DaughterIds()[1];
    if(index1 >= int(fTopoReconstructor->GetParticles().size()) || 
        index2 >= int(fTopoReconstructor->GetParticles().size()) || 
        index1 < 0 || index2 < 0 ) 
      return;
      
    KFParticle posDaughter, negDaughter;
    if(int(fTopoReconstructor->GetParticles()[index1].Q()) > 0)
    {
      posDaughter = fTopoReconstructor->GetParticles()[index1];
      negDaughter = fTopoReconstructor->GetParticles()[index2];
    }
    else
    {
      negDaughter = fTopoReconstructor->GetParticles()[index1];
      posDaughter = fTopoReconstructor->GetParticles()[index2];
    }
    float vertex[3] = {TempPart.GetX(), TempPart.GetY(), TempPart.GetZ()};
    posDaughter.TransportToPoint(vertex);
    negDaughter.TransportToPoint(vertex);
    KFParticle::GetArmenterosPodolanski(posDaughter, negDaughter, QtAlpha );
    
    histoParameters2D[0][iParticle][2]->Fill(QtAlpha[1],QtAlpha[0],1);
  }
    
  //Fill 3D histograms for multi differential analysis
  if( histoParameters3D && IsCollect3DHistogram(iParticle))
  {
    histoParameters3D[0][iParticle][0]->Fill(Rapidity,Pt,M,1);
    histoParameters3D[0][iParticle][1]->Fill(Rapidity,M_t,M,1);
    if(fCentralityBin>=0)
    {
      histoParameters3D[0][iParticle][2]->Fill(fCentralityBin, Pt, M, fCentralityWeight);
      histoParameters3D[0][iParticle][3]->Fill(fCentralityBin, Rapidity, M, fCentralityWeight);
      histoParameters3D[0][iParticle][4]->Fill(fCentralityBin, M_t, M, fCentralityWeight);
    }
    histoParameters3D[0][iParticle][5]->Fill(cT, Pt, M, 1);
  }
  
  //Fill histograms for the side bands analysis
  if(histoDSToParticleQA && IsCollect3DHistogram(iParticle))
  {
    if(fabs(fParteff.GetMass(iParticle)-M) < 3.f*fParteff.GetMassSigma(iParticle))//SignalReco
    {
      for(int iParam=0; iParam<17; iParam++)
        histoParameters[4][iParticle][iParam]->Fill(parameters[iParam]);
      
      if(multiplicities)
        multiplicities[4][iParticle]++;

      histoParameters2D[4][iParticle][0]->Fill(Rapidity,Pt,1);
      histoParameters2D[4][iParticle][3]->Fill(Rapidity,M_t,1);
      
      if(drawZR)
      {
        if(histoParameters2D[4][iParticle][1])
          histoParameters2D[4][iParticle][1]->Fill(Z,R,1);
        if(histoParameters2D[4][iParticle][2])
          histoParameters2D[4][iParticle][2]->Fill(QtAlpha[1],QtAlpha[0],1);
      }
    }
    
    if( fabs(fParteff.GetMass(iParticle)-M) > 3.f*fParteff.GetMassSigma(iParticle) &&
        fabs(fParteff.GetMass(iParticle)-M) <= 6.f*fParteff.GetMassSigma(iParticle) )//BGReco
    {
      for(int iParam=0; iParam<17; iParam++)
        histoParameters[5][iParticle][iParam]->Fill(parameters[iParam]);
      
      if(multiplicities)
        multiplicities[5][iParticle]++;

      histoParameters2D[5][iParticle][0]->Fill(Rapidity,Pt,1);
      histoParameters2D[5][iParticle][3]->Fill(Rapidity,M_t,1);
      
      if(drawZR)
      {
        if(histoParameters2D[5][iParticle][1])
          histoParameters2D[5][iParticle][1]->Fill(Z,R,1);
        if(histoParameters2D[5][iParticle][2])
          histoParameters2D[5][iParticle][2]->Fill(QtAlpha[1],QtAlpha[0],1);
      }
    }
  }
  
  if(!fStoreMCHistograms) return;

  int iSet = 1;
  if(!RtoMCParticleId[iP].IsMatchedWithPdg()) //background
  {
    if(!RtoMCParticleId[iP].IsMatched()) iSet = 3; // for ghost particles - combinatorial background
    else iSet = 2; // for physical background
  }
  
  //Check if PV association is correct
  if(!histoDSToParticleQA && iSet == 1)
  { 
    int iMCPart = RtoMCParticleId[iP].GetBestMatchWithPdg();
    KFMCParticle &mcPart = vMCParticles[iMCPart];
    int iMCTrack = mcPart.GetMCTrackID();
    KFMCTrack &mcTrack = vMCTracks[iMCTrack];
    int motherId = mcTrack.MotherId();
    bool isSecondaryParticle = motherId >= 0;
    
    if(iPV >=0)
    {
      if(isSecondaryParticle)
        iSet = 4;
      else 
      {
        int iMCPV = -1;
        if(RtoMCPVId[iPV].IsMatchedWithPdg())
          iMCPV = RtoMCPVId[iPV].GetBestMatch();
        
        int iMCPVFromParticle = fMCTrackToMCPVMatch[iMCTrack];
        if(iMCPV != iMCPVFromParticle)
          iSet = 4;
      }
    }
    else
    {
      if(!isSecondaryParticle)
        iSet = 4;
    }
  }
  
  //for signal particles
  for(int iParam=0; iParam<17; iParam++)
    histoParameters[iSet][iParticle][iParam]->Fill(parameters[iParam]);
    
  if(multiplicities)
    multiplicities[iSet][iParticle]++;
  
  histoParameters2D[iSet][iParticle][0]->Fill(Rapidity,Pt,1);
  if(drawZR)
  {
    if(histoParameters2D[iSet][iParticle][1])
      histoParameters2D[iSet][iParticle][1]->Fill(Z,R,1);
    if(histoParameters2D[iSet][iParticle][2])
      histoParameters2D[iSet][iParticle][2]->Fill(QtAlpha[1],QtAlpha[0],1);
  }
  histoParameters2D[iSet][iParticle][3]->Fill(Rapidity,M_t,1);
  
  if(iSet != 1) return;
  
  int iMCPart = RtoMCParticleId[iP].GetBestMatchWithPdg();
  KFMCParticle &mcPart = vMCParticles[iMCPart];
  // Fit quality of the mother particle
  if(histoFit)
  {
    int iMCTrack = mcPart.GetMCTrackID();
    KFMCTrack &mcTrack = vMCTracks[iMCTrack];
    int mcDaughterId = -1;
    if(iParticle >= fParteff.fFirstStableParticleIndex && iParticle <= fParteff.fLastStableParticleIndex)
      mcDaughterId = iMCTrack;
    else if(mcTrack.PDG() == 22 && TempPart.NDaughters() == 1)
      mcDaughterId = iMCTrack;
    else if(iParticle >= fParteff.fFirstMissingMassParticleIndex && iParticle <= fParteff.fLastMissingMassParticleIndex)
      mcDaughterId = mcPart.GetDaughterIds()[1]; //the charged daughter
    else
      mcDaughterId = mcPart.GetDaughterIds()[0];
    
    KFMCTrack &mcDaughter = vMCTracks[mcDaughterId];
    
    float mcX =  mcTrack.X();
    float mcY =  mcTrack.Y();
    float mcZ =  mcTrack.Z();
    if(histoDSToParticleQA || hPartParamPrimary == histoParameters)
    {
      mcX =  mcDaughter.X();
      mcY =  mcDaughter.Y();
      mcZ =  mcDaughter.Z();
    }
    const float mcPx = mcTrack.Par(3);
    const float mcPy = mcTrack.Par(4);
    const float mcPz = mcTrack.Par(5);

    float decayVtx[3] = { mcTrack.X(), mcTrack.Y(), mcTrack.Z() };
    float recParam[8] = { 0.f, 0.f, 0.f, 0.f, 0.f, 0.f, 0.f, 0.f };
    float errParam[8] = { 0.f, 0.f, 0.f, 0.f, 0.f, 0.f, 0.f, 0.f };

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
      histoFit[iParticle][iPar]->Fill(res[iPar]);
      histoFit[iParticle][iPar+8]->Fill(pull[iPar]);
    }
  }
  // Fit quality of daughters
  int daughterIndex[2] = {-1, -1};
  
  if(histoFitDaughtersQA)
  {
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

      KFParticleSIMD DaughterSIMD(Daughter);
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
        histoFitDaughtersQA[iParticle][iPar]->Fill(res[iPar]);
        histoFitDaughtersQA[iParticle][iPar+8]->Fill(pull[iPar]);
      }
      
      //fill Histos for GetDStoParticle
      if(iD == 0)
        daughterIndex[0] = recDaughterId;
      if(iD == 1 && daughterIndex[0] > -1 && histoDSToParticleQA)
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
            
            histoDSToParticleQA[iParticle][iPar]->Fill(res[iPar]);
            histoDSToParticleQA[iParticle][iPar+3]->Fill(pull[iPar]);
          }
        }
        
        Double_t dXds = pD[0][0][0] - pD[1][0][0];
        Double_t dYds = pD[0][1][0] - pD[1][1][0];
        Double_t dZds = pD[0][2][0] - pD[1][2][0];
        
        Double_t dRds = sqrt(dXds*dXds + dYds*dYds + dZds*dZds);
        histoDSToParticleQA[iParticle][6]->Fill(dRds);
      }
    }
  }
}

void KFTopoPerformance::FillHistos()
{
  /** Fills histograms with parameter  distributions and fit quality for all particle and primary vertex candidates. */
  vector<int> multiplicities[6];
  for(int iV=0; iV<6; iV++)
    multiplicities[iV].resize(KFPartEfficiencies::nParticles, 0);
  
  //fill histograms for found short-lived particles
  for(unsigned int iP=0; iP<fTopoReconstructor->GetParticles().size(); iP++)
  {
    int iParticle = fParteff.GetParticleIndex(fTopoReconstructor->GetParticles()[iP].GetPDG());
    if(iParticle < 0) continue;
    KFParticle TempPart = fTopoReconstructor->GetParticles()[iP];
    
    FillParticleParameters(TempPart,iParticle, iP, 0, hPartParam, hPartParam2D, hPartParam3D,
                           hFitQA, hFitDaughtersQA, hDSToParticleQA, multiplicities);
  }
  
  if(fStoreMCHistograms)
  {
    for(int iSet=0; iSet<KFParticleFinder::GetNSecondarySets(); iSet++)
    {
      const std::vector<KFParticle>& SecondaryCandidates = fTopoReconstructor->GetKFParticleFinder()->GetSecondaryCandidates()[iSet];
      for(unsigned int iP=0; iP<SecondaryCandidates.size(); iP++)
      {
        KFParticle TempPart = SecondaryCandidates[iP];
        int iParticle = fParteff.GetParticleIndex(TempPart.GetPDG());
        if(iParticle < 0) continue;
        
        const int id = TempPart.Id();
        FillParticleParameters(TempPart, iParticle, id, 0, hPartParamSecondaryMass, hPartParam2DSecondaryMass, 0);
        
        TempPart = fTopoReconstructor->GetParticles()[id];
        FillParticleParameters(TempPart, iParticle, id, 0, hPartParamSecondary, hPartParam2DSecondary, 0);
      }
    }
    
    for(int iSet=0; iSet<KFParticleFinder::GetNPrimarySets(); iSet++)
    {
      for(int iPV=0; iPV<fTopoReconstructor->NPrimaryVertices(); iPV++)
      {
        const std::vector<KFParticle>& PrimaryCandidates = fTopoReconstructor->GetKFParticleFinder()->GetPrimaryCandidates()[iSet][iPV];
        for(unsigned int iP=0; iP<PrimaryCandidates.size(); iP++)
        {
          KFParticle TempPart =  PrimaryCandidates[iP];
          int iParticle = fParteff.GetParticleIndex(TempPart.GetPDG());
          if(iParticle < 0) continue;
          
          const int id = TempPart.Id();
          FillParticleParameters(TempPart,iParticle, id, iPV, hPartParamPrimaryMass, hPartParam2DPrimaryMass, 0, hFitQAMassConstraint);
          
          TempPart = fTopoReconstructor->GetParticles()[id];
          FillParticleParameters(TempPart,iParticle, id, iPV, hPartParamPrimary, hPartParam2DPrimary, 0, hFitQANoConstraint);
        }
        
        const std::vector<KFParticle>& PrimaryCandidatesTopo = fTopoReconstructor->GetKFParticleFinder()->GetPrimaryTopoCandidates()[iSet][iPV];
        for(unsigned int iP=0; iP<PrimaryCandidatesTopo.size(); iP++)
        {
          KFParticle TempPart =  PrimaryCandidatesTopo[iP];
          int iParticle = fParteff.GetParticleIndex(TempPart.GetPDG());
          if(iParticle < 0) continue;
          
          FillParticleParameters(TempPart,iParticle, TempPart.Id(), iPV, hPartParamPrimaryTopo, hPartParam2DPrimaryTopo, 0, hFitQATopoConstraint);
        }
        
        const std::vector<KFParticle>& PrimaryCandidatesTopoMass = fTopoReconstructor->GetKFParticleFinder()->GetPrimaryTopoMassCandidates()[iSet][iPV];
        for(unsigned int iP=0; iP<PrimaryCandidatesTopoMass.size(); iP++)
        {
          KFParticle TempPart =  PrimaryCandidatesTopoMass[iP];
          int iParticle = fParteff.GetParticleIndex(TempPart.GetPDG());
          if(iParticle < 0) continue;
          
          FillParticleParameters(TempPart,iParticle, TempPart.Id(), iPV, hPartParamPrimaryTopoMass, hPartParam2DPrimaryTopoMass, 0, hFitQATopoMassConstraint);
        }
      }
    }
  }
  //fill histograms with ChiPrim for every particle
  for(unsigned int iP=0; iP<fTopoReconstructor->GetParticles().size(); iP++)
  {
    KFParticle TempPart = fTopoReconstructor->GetParticles()[iP];
    KFParticle vtx = fTopoReconstructor->GetPrimVertex(0);
    
    if(RtoMCParticleId[iP].IsMatched())
    {
      int iMCPV = vMCParticles[RtoMCParticleId[iP].GetBestMatch()].GetMotherId();
      if(iMCPV<0.)
      {
        iMCPV = -iMCPV - 1;
        if(MCtoRPVId[iMCPV].IsMatched())
        { 
          vtx = fTopoReconstructor->GetPrimVertex(MCtoRPVId[iMCPV].GetBestMatch());
        }  
      }
    }
//     else
//       KFParticle & vtx = fTopoReconstructor->GetPrimVertex(0);
    
    
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
  
  if(fStoreMCHistograms)
  {
    for(int iV=0; iV<6; iV++)
      for(int iP=0; iP < KFPartEfficiencies::nParticles; iP++)
        if(hPartParam[iV][iP][17])
          hPartParam[iV][iP][17]->Fill(multiplicities[iV][iP]);
    FillMCHistos();
  }
  else
    for(int iP=0; iP < KFPartEfficiencies::nParticles; iP++)
      if(hPartParam[0][iP][17])
        hPartParam[0][iP][17]->Fill(multiplicities[0][iP]);  
} // void KFTopoPerformance::FillHistos()

void KFTopoPerformance::FillMCHistos()
{
  /** Fills histograms of Monte Carlo particles. */
  for(unsigned int iMCTrack=0; iMCTrack<vMCTracks.size(); iMCTrack++)
  {
    int iPDG = fParteff.GetParticleIndex(vMCTracks[iMCTrack].PDG());
    if(iPDG < 0) continue;
    
    if(vMCTracks[iMCTrack].MotherId()>=0) continue;
    KFMCParticle &part = vMCParticles[iMCTrack];
       
    float M = fParteff.partMass[iPDG];
    float P = vMCTracks[iMCTrack].P();
    float Pt = vMCTracks[iMCTrack].Pt();
    float E = sqrt(M*M+P*P);
    float Rapidity = 0.5*log((E+vMCTracks[iMCTrack].Pz())/(E-vMCTracks[iMCTrack].Pz()));
    float M_t = sqrt(Pt*Pt+M*M)-M;
    
    float X;
    float Y;
    float Z;
    float R;
    
    if (part.NDaughters()>0)
    {
      X = vMCTracks[part.GetDaughterIds()[0]].X();
      Y = vMCTracks[part.GetDaughterIds()[0]].Y();
      Z = vMCTracks[part.GetDaughterIds()[0]].Z();
    }
    else
    {
      X = vMCTracks[iMCTrack].X();
      Y = vMCTracks[iMCTrack].Y();
      Z = vMCTracks[iMCTrack].Z();
    }
    R = sqrt(X*X+Y*Y);
    
    
    float parameters[17] = {M, P, Pt, Rapidity, 0, 0, 0, 0, 0, 0, X, Y, Z, R, 0, 0, M_t};
    //for all particle-candidates
    for(int iParam=0; iParam<17; iParam++)
      if(hPartParam[6][iPDG][iParam]) hPartParam[6][iPDG][iParam]->Fill(parameters[iParam]);

    if(hPartParam2D[6][iPDG][0]) hPartParam2D[6][iPDG][0]->Fill(Rapidity,Pt,1);
    if(hPartParam2D[6][iPDG][3]) hPartParam2D[6][iPDG][3]->Fill(Rapidity,M_t,1);
    
    if(IsCollectZRHistogram(iPDG))
      if(hPartParam2D[6][iPDG][1]) hPartParam2D[6][iPDG][1]->Fill(Z,R,1);
    
    if( part.IsReconstructable(2) && IsCollectArmenteros(iPDG))
    {
      int index1 = part.GetDaughterIds()[0];
      int index2 = part.GetDaughterIds()[1];
      KFMCTrack positive, negative;
      if(vMCTracks[index1].Par(6) > 0)
      {
        positive = vMCTracks[index1];
        negative = vMCTracks[index2];
      }
      else
      {
        negative = vMCTracks[index1];
        positive = vMCTracks[index2];
      }

      float alpha = 0., qt = 0.;
      float spx = positive.Px() + negative.Px();
      float spy = positive.Py() + negative.Py();
      float spz = positive.Pz() + negative.Pz();
      float sp  = sqrt(spx*spx + spy*spy + spz*spz);
      float pn, pln, plp;
      pn = sqrt(negative.Px()*negative.Px() + negative.Py()*negative.Py() + negative.Pz()*negative.Pz());
      pln  = (negative.Px()*spx+negative.Py()*spy+negative.Pz()*spz)/sp;
      plp  = (positive.Px()*spx+positive.Py()*spy+positive.Pz()*spz)/sp;
      float ptm  = (1.-((pln/pn)*(pln/pn)));
      qt= (ptm>=0.)?  pn*sqrt(ptm) :0;
      alpha = (plp-pln)/(plp+pln);
      
      if(hPartParam2D[6][iPDG][2]) hPartParam2D[6][iPDG][2]->Fill(alpha,qt,1);
    }  
  }
}

void KFTopoPerformance::AddV0Histos()
{
  /** Copies histograms of K0s candidates to V0 folder. */
  int iV0 = fParteff.nParticles - 1;
  int iK0 = fParteff.GetParticleIndex(310);
  
  for(int iH=0; iH<nFitQA; iH++)
  {
    hFitDaughtersQA[iV0][iH]->Add(hFitDaughtersQA[iK0][iH]);
    hFitQA[iV0][iH]->Add(hFitQA[iK0][iH]);
  }

  for(int iV=0; iV<4; iV++)
    for(int iH=0; iH<nHistoPartParam; iH++)
      hPartParam[iV][iV0][iH]->Add(hPartParam[iV][iK0][iH]);
}

void KFTopoPerformance::FillHistos(const KFPHistogram* histograms)
{
  /** Fill histograms with the histograms from the provided KFPHistogram object. */
  for(int iParticle=0; iParticle<KFPartEfficiencies::nParticles; iParticle++)
  {
    const int& nHistograms = histograms->GetHistogramSet(0).GetNHisto1D();
    for(int iHistogram=0; iHistogram<nHistograms; iHistogram++)
    {
      const KFPHistogram1D& histogram = histograms->GetHistogram(iParticle,iHistogram);
      for(int iBin=0; iBin<histogram.Size(); iBin++)
        hPartParam[0][iParticle][iHistogram]->SetBinContent( iBin, histogram.GetHistogram()[iBin] );
    }
  }
}

#endif //DO_TPCCATRACKER_EFF_PERFORMANCE
