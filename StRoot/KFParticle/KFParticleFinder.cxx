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

#include "KFParticleFinder.h"

using std::map;
using std::vector;

#include "KFParticleMath.h"
#include "KFParticleDatabase.h"
#include "KFPEmcCluster.h"

KFParticleFinder::KFParticleFinder():
  fNPV(-1),fNThreads(1),fDistanceCut(1.f),fLCut(-5.f),fCutCharmPt(0.2f),fCutCharmChiPrim(85.f),fCutLVMPt(0.0f),fCutLVMP(0.0f),fCutJPsiPt(1.0f),
  fD0(0), fD0bar(0), fD04(0), fD04bar(0), fD0KK(0), fD0pipi(0), fDPlus(0), fDMinus(0), 
  fDPlus3Pi(0), fDMinus3Pi(0), fDsPlusK2Pi(0), fDsMinusK2Pi(0), fLcPlusP2Pi(0), fLcMinusP2Pi(0),
  fLPi(0), fLPiPIndex(0), fDPi(0), fDPiBar(0), fTPi(0), fTPiBar(0), fHe3Pi(0), fHe3PiBar(0), fHe3PPi(0), fHe4Pi(0), fHe4PiBar(0), fHe4PPi(0),
  fHe4L(0), fHe5L(0),  fLLn(0), fH5LL(0), fPipi(0), fPpi(0), fPpiBar(0), fPPpi(0), fPPpiBar(0),
  fSecCandidates(), fPrimCandidates(), fPrimCandidatesTopo(),fPrimCandidatesTopoMass(),
  fEmcClusters(0), fMixedEventAnalysis(0), fDecayReconstructionList()
{
  /** The default constructor. Initialises all cuts to the default values. **/
  //Cuts
  //track + track
  //chi2_prim         chi2_geo          ldl
  fCuts2D[0] = 3.f; fCuts2D[1] = 3.f; fCuts2D[2] = 5.f; 
  //cuts to select primary and secondary particles
  //mass              chi2_topo          ldl
#ifdef PANDA_STT
  fSecCuts[0] = 3.f; fSecCuts[1] = -3.f; fSecCuts[2] = 10.f;
#else  
  fSecCuts[0] = 3.f; fSecCuts[1] = 5.f; fSecCuts[2] = 10.f;
#endif
  
#ifdef __ROOT__
  fCutCharmChiPrim = 8;
#endif
  
  //track + particle
  //                ldl          chi2_topo                        chi2_geo
  fCutsTrackV0[0][0] =  5;     fCutsTrackV0[0][1] = 5;        fCutsTrackV0[0][2] = 6;  //Xi, Omega
  fCutsTrackV0[1][0] =  5;     fCutsTrackV0[1][1] = 5;        fCutsTrackV0[1][2] = 6;  //Charm, H0, Sigma+
  fCutsTrackV0[2][0] = -100.;  fCutsTrackV0[2][1] = 10000.;   fCutsTrackV0[2][2] = 3;  //resonances
  
  //charm
  //chi2               l/dl                  chi2_topo
  fCutsCharm[0] = 3.f; fCutsCharm[1] = 10.f;  fCutsCharm[2] = 3.f; //D0 -> pi+ K-
  
  //cuts on particles reconstructed from short-lived particles
  //ldl,                      chi2_topo                 chi2_geo
  //H0 -> Lambda Lambda, Xi0 -> Lambda pi0
  fCutsPartPart[0][0] =  10;  fCutsPartPart[0][1] = 3;  fCutsPartPart[0][2] = 3;
  //Sigma0 -> Lambda Gamma, pi0 -> Gamma Gamma, K* -> K pi0, Sigma*0 -> Lambda pi0, Xi* -> Xi pi0
  fCutsPartPart[1][0] = -10;  fCutsPartPart[1][1] = 3;  fCutsPartPart[1][2] = 3;  
}

//________________________________________________________________________________
void KFParticleFinder::Init(int nPV) 
{
  /** Initialises the new event: all vectors with temporary candidates are cleaned, the number of 
   ** primary vertices is set to "nPV", vectors with primary candidates are resized correspondingly.
   ** \param[in] nPV - number of primary vertices in the event which will be processed
   **/
  
  fNPV = nPV;
//   Particles.reserve(vRTracks.size() + nPart);

  fD0.clear();
  fD0bar.clear();
  fD04.clear();
  fD04bar.clear();
  fD0KK.clear();
  fD0pipi.clear();
  fDPlus.clear();
  fDMinus.clear();
  fDPlus3Pi.clear();
  fDMinus3Pi.clear();
  fDsPlusK2Pi.clear();
  fDsMinusK2Pi.clear();
  fLcPlusP2Pi.clear();
  fLcMinusP2Pi.clear();
  fLPi.clear();
  fLPiPIndex.clear();
  fDPi.clear();
  fDPiBar.clear();
  fTPi.clear();
  fTPiBar.clear();
  fHe3Pi.clear();
  fHe3PiBar.clear();
  fHe3PPi.clear();
  fHe4Pi.clear();
  fHe4PiBar.clear();
  fHe4PPi.clear();
  fHe4L.clear();
  fHe5L.clear();
  fLLn.clear();
  fH5LL.clear();
  fPipi.clear();
  fPpi.clear();
  fPpiBar.clear();
  fPPpi.clear();
  fPPpiBar.clear();
  
  for(int iCandidates=0; iCandidates<fNSecCandidatesSets; iCandidates++)
    fSecCandidates[iCandidates].clear();
  
  for(int iCandidates=0; iCandidates<fNPrimCandidatesSets; iCandidates++)
  {
    fPrimCandidates[iCandidates].clear();
    fPrimCandidates[iCandidates].resize(fNPV);
  }
  
  for(int iCandidates=0; iCandidates<fNPrimCandidatesTopoSets; iCandidates++)
  {
    fPrimCandidatesTopo[iCandidates].clear();
    fPrimCandidatesTopo[iCandidates].resize(fNPV);
    
    fPrimCandidatesTopoMass[iCandidates].clear();
    fPrimCandidatesTopoMass[iCandidates].resize(fNPV);
  }
  
}
//________________________________________________________________________________

void KFParticleFinder::FindParticles(KFPTrackVector* vRTracks, kfvector_float* ChiToPrimVtx, std::vector<KFParticle>& Particles,
                                     std::vector<KFParticleSIMD, KFPSimdAllocator<KFParticleSIMD> >& PrimVtx, int nPV)
{
  /** The main interface which runs reconstruction of short-lived particles:\n
   ** 1) a new event is initialised; \n
   ** 2) long-lived particles formed from tracks are stored to the output array "Particles"; \n
   ** 3) 2-daughter channels are reconstructed (KFParticleFinder::Find2DaughterDecay()); \n
   ** 4) the 2-daughter same-signed background is collected for resonances (KFParticleFinder::ConstructPrimaryBG()); \n
   ** 5) found primary candidates of \f$K_s^0\f$, \f$\Lambda\f$, \f$\overline{\Lambda}\f$ and \f$\gamma\f$ are transported
   ** to the point of the closest approach with the corresponding primary vertex (KFParticleFinder::ExtrapolateToPV()); \n
   ** 6) reconstruction with the missing mass method (KFParticleFinder::NeutralDaughterDecay()); \n
   ** 7) all other decays are reconstructed one after another. \n
   ** If analysis is run in the mixed event mode only steps 1) and 2) are performed.
   ** \param[in] vRTracks - pointer to the array with vectors of tracks:\n
   ** 0) secondary positive at the first hit position; \n
   ** 1) secondary negative at the first hit position; \n
   ** 2) primary positive at the first hit position; \n
   ** 3) primary negative at the first hit position; \n
   ** 4) secondary positive at the last hit position; \n
   ** 5) secondary negative at the last hit position; \n
   ** 6) primary positive at the last hit position; \n
   ** 7) primary negative at the last hit position. \n
   ** \param[in] ChiToPrimVtx - arrays with vectors of the \f$\chi^2_{prim}\f$ deviations for track vectors 1) and 2).
   ** \param[out] Particles - output vector with particles.
   ** \param[in] PrimVtx - vector with primary vertices.
   ** \param[in] nPV - number of the input primary vertices.
   **/
  Init(nPV);
//   const int nPartPrim = vRTracks[2].NPions() * vRTracks[3].NKaons() + 
//                         vRTracks[3].NPions() * vRTracks[2].NKaons() + 
//                         vRTracks[2].NKaons() * vRTracks[3].NKaons() + 
//                         vRTracks[2].NKaons() * vRTracks[3].NProtons() + 
//                         vRTracks[3].NKaons() * vRTracks[2].NProtons() + 
//                         vRTracks[2].NElectrons() * vRTracks[3].NElectrons() + 
//                         vRTracks[2].NMuons() * vRTracks[3].NMuons();

//   const int nPart = vRTracks[0].NPions() * vRTracks[1].NPions() +
//                     vRTracks[0].NPions() * vRTracks[1].NProtons() +
//                     vRTracks[1].NPions() * vRTracks[0].NProtons() + nPartPrim;
//   int nEmcClusters = 0;
//   if(fEmcClusters)
//     nEmcClusters = fEmcClusters->Size();
  vector<KFParticle> vGammaPrimEmc;

//   int nPartEstimation = nPart+vRTracks[0].Size()+vRTracks[1].Size()+vRTracks[2].Size()+vRTracks[3].Size() + nEmcClusters;
//   if(nPartEstimation < 100000)
//     Particles.reserve(nPartEstimation);

  //* Finds particles (K0s and Lambda) from a given set of tracks
  {
    KFPTrack kfTrack;
    for(int iV=0; iV<4; iV++)
    {
      for(int iTr=0; iTr < vRTracks[iV].Size(); iTr++)
      {
        vRTracks[iV].GetTrack(kfTrack, iTr);
        int pdg = vRTracks[iV].PDG()[iTr];
        if( pdg == 19 ) pdg =  13;
        if( pdg ==-19 ) pdg = -13;
        KFParticle tmp(kfTrack, pdg);
        tmp.SetPDG(pdg);
        tmp.SetId(Particles.size());
        vRTracks[iV].SetId(Particles.size(),iTr);
        if(vRTracks[iV+4].Size() > 0)
          vRTracks[iV+4].SetId(Particles.size(),iTr);
        tmp.AddDaughterId( kfTrack.Id() );
#ifdef NonhomogeneousField
        for(int iF=0; iF<10; iF++)
          tmp.SetFieldCoeff( vRTracks[iV].FieldCoefficient(iF)[iTr], iF);
#endif
        Particles.push_back(tmp);
      }
    }

    if(fEmcClusters)
    {
      KFParticleSIMD tmpGammaSIMD;
      KFParticle tmpGamma;
      
      for(int iEmc=0; iEmc < fEmcClusters->Size(); iEmc += SimdLen)
      {
        const int NClustersVec = (iEmc + SimdLen < fEmcClusters->Size()) ? SimdLen : (fEmcClusters->Size() - iEmc);
        tmpGammaSIMD.Load(*fEmcClusters, iEmc, PrimVtx[0]);
        for(int iV=0; iV<NClustersVec; iV++)
        {
          tmpGammaSIMD.GetKFParticle(tmpGamma, iV);
          tmpGamma.SetPDG(22); //gamma pdg
          tmpGamma.SetId(Particles.size());
          tmpGamma.CleanDaughtersId();
          tmpGamma.AddDaughterId(fEmcClusters->Id()[iEmc+iV]);
          Particles.push_back(tmpGamma);
          vGammaPrimEmc.push_back(tmpGamma);
        }
      }
    }
  }

#if 0
  Find2DaughterDecayOneSign(vRTracks[0], Particles, PrimVtx);
#else

  // ConstructResonances2D(vRTracks, Particles, PrimVtx);

  Find2DaughterDecay(vRTracks, ChiToPrimVtx,
                     Particles, PrimVtx, fCuts2D,
                     fSecCuts, fPrimCandidates, fSecCandidates);

  if(!fMixedEventAnalysis)
  {
    //Construct two-particle background from positive primary tracks for subtraction from the resonance spectra
    // ConstructPrimaryBG(vRTracks, Particles, PrimVtx, fCuts2D, fSecCuts, fPrimCandidates, fSecCandidates);
    
    for(int iPV=0; iPV<fNPV; iPV++ )
    {
      ExtrapolateToPV(fPrimCandidates[0][iPV],PrimVtx[iPV]);
      ExtrapolateToPV(fPrimCandidates[1][iPV],PrimVtx[iPV]);
      ExtrapolateToPV(fPrimCandidates[2][iPV],PrimVtx[iPV]);
      ExtrapolateToPV(fPrimCandidates[3][iPV],PrimVtx[iPV]);
    }
    
    NeutralDaughterDecay(vRTracks, Particles, PrimVtx);
    
    //Xi- -> Lambda pi-, Omega- -> Lambda K-
    FindTrackV0Decay(fSecCandidates[1], 3122, vRTracks[1], -1, vRTracks[1].FirstPion(), vRTracks[1].LastKaon(),
                     Particles, PrimVtx, -1, &(ChiToPrimVtx[1]), &fPrimCandidates[5]);
    //Xi+ -> Lambda pi+, Omega+ -> Lambda K+
    FindTrackV0Decay(fSecCandidates[2], -3122, vRTracks[0], 1, vRTracks[0].FirstPion(), vRTracks[0].LastKaon(),
                     Particles, PrimVtx, -1, &(ChiToPrimVtx[0]), &fPrimCandidates[6]);

    for(int iPV=0; iPV<fNPV; iPV++ )
    {
      ExtrapolateToPV(fPrimCandidates[5][iPV],PrimVtx[iPV]);
      ExtrapolateToPV(fPrimCandidates[6][iPV],PrimVtx[iPV]);
      
      ExtrapolateToPV(fPrimCandidates[7][iPV],PrimVtx[iPV]);
      ExtrapolateToPV(fPrimCandidates[8][iPV],PrimVtx[iPV]);
    }

    //K*+ -> K0 pi+
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[0][iPV], 310, vRTracks[2], 1, vRTracks[2].FirstPion(), vRTracks[2].LastPion(),
                      Particles, PrimVtx, iPV, 0);
    //K*- -> K0 pi-
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[0][iPV], 310, vRTracks[3], -1, vRTracks[3].FirstPion(), vRTracks[3].LastPion(),
                      Particles, PrimVtx, iPV, 0);
    //Sigma*+ -> Lambda pi+
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[1][iPV], 3122, vRTracks[2], 1, vRTracks[2].FirstPion(), vRTracks[2].LastPion(),
                      Particles, PrimVtx, iPV, 0);
    //Sigma*- -> Lambda pi-, Xi*- -> Lambda K-
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[1][iPV], 3122, vRTracks[3], -1, vRTracks[3].FirstPion(), vRTracks[3].LastKaon(),
                      Particles, PrimVtx, iPV, 0);
    //Sigma*+_bar -> Lambda_bar pi-
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[2][iPV], -3122, vRTracks[3], -1, vRTracks[3].FirstPion(), vRTracks[3].LastPion(),
                        Particles, PrimVtx, iPV, 0);
    //Sigma*-_bar -> Lambda_bar pi+, Xi*+ -> Lambda_bar + K+
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[2][iPV], -3122, vRTracks[2], 1, vRTracks[2].FirstPion(), vRTracks[2].LastKaon(),
                        Particles, PrimVtx, iPV, 0);
    //Xi*0 -> Xi- pi+
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[5][iPV], 3312, vRTracks[2], 1, vRTracks[2].FirstPion(), vRTracks[2].LastPion(),
                        Particles, PrimVtx, iPV, 0, &fPrimCandidates[9]);
    //Xi*0_bar -> Xi+ pi-
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[6][iPV], -3312, vRTracks[3], -1, vRTracks[3].FirstPion(), vRTracks[3].LastPion(),
                        Particles, PrimVtx, iPV, 0, &fPrimCandidates[10]);
    //Omega*- -> Xi- pi+ K-
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[9][iPV], 3324, vRTracks[3], -1, vRTracks[3].FirstKaon(), vRTracks[3].LastKaon(),
                        Particles, PrimVtx, iPV, 0);
    //Omega*+ -> Xi+ pi- K+
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[10][iPV], -3324, vRTracks[2], 1, vRTracks[2].FirstKaon(), vRTracks[2].LastKaon(),
                        Particles, PrimVtx, iPV, 0);
      
    // K+ -> pi+ pi+ pi-
    FindTrackV0Decay(fPipi, 100310, vRTracks[0],  1, vRTracks[0].FirstPion(), vRTracks[0].LastPion(), Particles, PrimVtx, -1, 0, &fPrimCandidates[11]);

    // K- -> pi+ pi- pi-
    FindTrackV0Decay(fPipi, 100310, vRTracks[1], -1, vRTracks[1].FirstPion(), vRTracks[1].LastPion(), Particles, PrimVtx, -1, 0, &fPrimCandidates[12]);    
      
    MatchKaons(vRTracks, PrimVtx, Particles);

    //Hypernuclei
    //H3L -> d p pi-, H4L -> d d pi-, H5L -> t d pi-
    FindTrackV0Decay(fDPi     , 3003, vRTracks[0],  1, vRTracks[0].FirstProton(), vRTracks[0].LastBe7(), Particles, PrimVtx, -1, 0);
    //H3L_bar -> d- p- pi+, H4L_bar -> d- d- pi+, H5L_bar -> t- d- pi+
    FindTrackV0Decay(fDPiBar  ,-3003, vRTracks[1], -1, vRTracks[1].FirstProton(), vRTracks[1].LastBe7(), Particles, PrimVtx, -1, 0);  
    //H4L -> t p pi-, H6L -> t t pi-
    FindTrackV0Decay(fTPi     , 3103, vRTracks[0],  1, vRTracks[0].FirstProton(), vRTracks[0].LastBe7(), Particles, PrimVtx, -1, 0);
    //H4L_bar -> t- p- pi+, H6L_bar -> t- t- pi+
    FindTrackV0Decay(fTPiBar  ,-3103, vRTracks[1], -1, vRTracks[1].FirstProton(), vRTracks[1].LastBe7(), Particles, PrimVtx, -1, 0);    
    //He4L -> He3 p pi- 
    FindTrackV0Decay(fHe3Pi   , 3004, vRTracks[0],  1, vRTracks[0].FirstProton(), vRTracks[0].LastBe7(), Particles, PrimVtx, -1, 0);
    //He5L -> He3 d pi-, He6L -> He3 t pi-, Li6L-> He3 He3 pi-, Li7L-> He4 He3 pi-
//     FindTrackV0Decay(fHe3Pi   , 3004, vRTracks[0],  1, vRTracks[0].FirstDeuteron(), vRTracks[0].LastBe7(), Particles, PrimVtx, -1, 0);
    //He4L_bar -> He3- p- pi+, He5L_bar -> He3- d- pi+, He6L_bar -> He3- t- pi+, Li6L_bar -> He3- He3- pi+, Li7L_bar -> He4- He3- pi+
    FindTrackV0Decay(fHe3PiBar,-3004, vRTracks[1], -1, vRTracks[1].FirstProton(), vRTracks[1].LastBe7(), Particles, PrimVtx, -1, 0);
    //He5L -> He4 p pi-
    FindTrackV0Decay(fHe4Pi   , 3005, vRTracks[0],  1, vRTracks[0].FirstProton(), vRTracks[0].LastBe7(), Particles, PrimVtx, -1, 0);
    //He6L -> He4 d pi-, He7L -> He4 t pi-, Li8L-> He4 He4 pi-
//     FindTrackV0Decay(fHe4Pi   , 3005, vRTracks[0],  1, vRTracks[0].FirstDeuteron(), vRTracks[0].LastBe7(), Particles, PrimVtx, -1, 0, 0);
    //He5L_bar -> He4- p- pi+, He6L_bar -> He4- d- pi+, He7L_bar -> He4- t- pi+, Li8L_bar -> He4- He4- pi+
    FindTrackV0Decay(fHe4PiBar,-3005, vRTracks[1], -1, vRTracks[1].FirstProton(), vRTracks[1].LastBe7(), Particles, PrimVtx, -1, 0);
    //LLn -> H3L pi-
    std::vector<KFParticle> h3L;
    float massH3L, massH3LSigma;
    KFParticleDatabase::Instance()->GetMotherMass(3004, massH3L, massH3LSigma);
    for(unsigned int iHe3Pi=0; iHe3Pi<fHe3Pi.size(); iHe3Pi++) {
      KFParticle& he3pi = fHe3Pi[iHe3Pi];
      float m, dm;
      he3pi.GetMass(m,dm);
      if( (fabs(m - massH3L)/massH3LSigma) > 3.f ) continue;
      he3pi.SetNonlinearMassConstraint(massH3L);
      h3L.push_back(he3pi);
    }
    FindTrackV0Decay(h3L      , 3004, vRTracks[1], -1, vRTracks[1].FirstPion(),   vRTracks[1].LastPion(),   Particles, PrimVtx, -1, 0 );

    //LLnn -> H4L pi-
    std::vector<KFParticle> h4L;
    float massH4L = 3.9225f;
    float massH4LSigma = 0.002f;
    for(unsigned int iHe4Pi=0; iHe4Pi<fHe4Pi.size(); iHe4Pi++) {
      KFParticle& he4pi = fHe4Pi[iHe4Pi];
      float m, dm;
      he4pi.GetMass(m,dm);
      if( (fabs(m - massH4L)/massH4LSigma) > 3.f ) continue;
      he4pi.SetNonlinearMassConstraint(massH4L);
      h4L.push_back(he4pi);
    }
    FindTrackV0Decay(h4L      , 3005, vRTracks[1], -1, vRTracks[1].FirstPion(),   vRTracks[1].LastPion(),   Particles, PrimVtx, -1, 0 );

    //H4LL -> He4L pi-
    FindLL(3008, -211, fHe4L, vRTracks[1], vRTracks[1].FirstPion(), vRTracks[1].LastPion(), PrimVtx[0], Particles);
    FindLL(3008, -211, fHe4L, vRTracks[3], vRTracks[3].FirstPion(), vRTracks[3].LastPion(), PrimVtx[0], Particles);
    // FindTrackV0Decay(fHe4L    , 3006, vRTracks[1], -1, vRTracks[1].FirstPion(),   vRTracks[1].LastPion(),   Particles, PrimVtx, -1, 0 );
    //H5LL -> He5L pi-
    FindLL(3010, -211, fHe5L, vRTracks[1], vRTracks[1].FirstPion(), vRTracks[1].LastPion(), PrimVtx[0], Particles);
    FindLL(3010, -211, fHe5L, vRTracks[3], vRTracks[3].FirstPion(), vRTracks[3].LastPion(), PrimVtx[0], Particles);
    // FindTrackV0Decay(fHe5L    , 3007, vRTracks[1], -1, vRTracks[1].FirstPion(),   vRTracks[1].LastPion(),   Particles, PrimVtx, -1, 0 );
    //H4LL -> H3L p pi-
    FindTrackV0Decay(fLLn     , 3203, vRTracks[0],  1, vRTracks[0].FirstProton(), vRTracks[0].LastProton(), Particles, PrimVtx, -1, 0 );
    //He6LL -> He5L p pi-
    FindTrackV0Decay(fH5LL    , 3010, vRTracks[0],  1, vRTracks[0].FirstProton(), vRTracks[0].LastProton(), Particles, PrimVtx, -1, 0 );
    //H2L -> p p pi- 
    FindTrackV0Decay(fPpi     , 8122, vRTracks[0],  1, vRTracks[0].FirstProton(), vRTracks[0].LastProton(), Particles, PrimVtx, -1, 0 );
    //H2L_bar -> p- p- pi+ 
    FindTrackV0Decay(fPpiBar  ,-8122, vRTracks[1], -1, vRTracks[1].FirstProton(), vRTracks[1].LastProton(), Particles, PrimVtx, -1, 0 );
    //He3L -> p p p pi- 
    FindTrackV0Decay(fPPpi    , 3028, vRTracks[0],  1, vRTracks[0].FirstProton(), vRTracks[0].LastProton(), Particles, PrimVtx, -1, 0 );
    //He3L_bar -> p- p- p- pi+ 
    FindTrackV0Decay(fPPpiBar ,-3028, vRTracks[1], -1, vRTracks[1].FirstProton(), vRTracks[1].LastProton(), Particles, PrimVtx, -1, 0 );
    //Li5L -> He3 p p pi-
    FindTrackV0Decay(fHe3PPi  , 3006, vRTracks[0],  1, vRTracks[0].FirstProton(), vRTracks[0].LastProton(), Particles, PrimVtx, -1, 0 );
    //Li6L -> He4 p p pi-
    FindTrackV0Decay(fHe4PPi  , 3007, vRTracks[0],  1, vRTracks[0].FirstProton(), vRTracks[0].LastProton(), Particles, PrimVtx, -1, 0 );

    //Hyper resonances with Lambda
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[1][iPV], 3122, vRTracks[2], 1, vRTracks[2].FirstProton(), vRTracks[2].LastHe4(),
                      Particles, PrimVtx, iPV, 0);
    //Hyper resonances with Lambda_bar
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[2][iPV], -3122, vRTracks[3], -1, vRTracks[3].FirstProton(), vRTracks[3].LastHe4(),
                        Particles, PrimVtx, iPV, 0);
    // Charm
    //LambdaC -> pi+ K- p, Ds+ -> pi+ K- K+, D+ -> pi+ K- pi+
    FindTrackV0Decay(fD0, 421, vRTracks[0], 1, vRTracks[0].FirstPion(), vRTracks[0].LastProton(),
                    Particles, PrimVtx, -1, &(ChiToPrimVtx[0]));
    //LambdaC_bar -> pi- K+ p-, Ds- -> pi- K+ K-, D- -> pi- K+ pi-
    FindTrackV0Decay(fD0bar, -421, vRTracks[1], -1, vRTracks[1].FirstPion(), vRTracks[1].LastProton(),
                    Particles, PrimVtx, -1, &(ChiToPrimVtx[1]));    
    //D0->pi+ K- pi+ pi-
    FindTrackV0Decay(fDPlus, 411, vRTracks[1], -1, vRTracks[1].FirstPion(), vRTracks[1].LastPion(),
                    Particles, PrimVtx, -1, &(ChiToPrimVtx[1]));
    //D0_bar->pi- K+ pi- pi+
    FindTrackV0Decay(fDMinus, -411, vRTracks[0], 1, vRTracks[0].FirstPion(), vRTracks[0].LastPion(),
                    Particles, PrimVtx, -1, &(ChiToPrimVtx[0]));
    //B+ -> D0_bar pi+, B+ -> D0_bar K+
    FindTrackV0Decay(fD0bar, -421, vRTracks[0], 1, vRTracks[0].FirstPion(), vRTracks[0].LastKaon(),
                     Particles, PrimVtx, -1, &(ChiToPrimVtx[0]));    
    //B- -> D0 pi-, B- -> D0 K-
    FindTrackV0Decay(fD0, 421, vRTracks[1], -1, vRTracks[1].FirstPion(), vRTracks[1].LastKaon(),
                     Particles, PrimVtx, -1, &(ChiToPrimVtx[1]));
    //B0 -> D- pi+, B0 -> D- K+
    FindTrackV0Decay(fDMinus, -419, vRTracks[0], 1, vRTracks[0].FirstPion(), vRTracks[0].LastKaon(),
                     Particles, PrimVtx, -1, &(ChiToPrimVtx[0]));    
    //B0_bar -> D+ pi-, B0_bar -> D+ K-
    FindTrackV0Decay(fDPlus, 419, vRTracks[1], -1, vRTracks[1].FirstPion(), vRTracks[1].LastKaon(),
                     Particles, PrimVtx, -1, &(ChiToPrimVtx[1]));
    //D0 -> pi+ K-
    SelectParticles(Particles,fD0,PrimVtx,fCutsCharm[2],fCutsCharm[1],
                    KFParticleDatabase::Instance()->GetD0Mass(), KFParticleDatabase::Instance()->GetD0MassSigma(), fSecCuts[0]);
    //D0_bar -> pi+ K-
    SelectParticles(Particles,fD0bar,PrimVtx,fCutsCharm[2],fCutsCharm[1],
                    KFParticleDatabase::Instance()->GetD0Mass(), KFParticleDatabase::Instance()->GetD0MassSigma(), fSecCuts[0]);    
    //D*+->D0 pi+
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fD0, 421, vRTracks[2], 1, vRTracks[2].FirstPion(), vRTracks[2].LastPion(),
                        Particles, PrimVtx, iPV, 0);
    //D*- -> D0_bar pi-
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fD0, -421, vRTracks[3], -1, vRTracks[3].FirstPion(), vRTracks[3].LastPion(),
                        Particles, PrimVtx, iPV, 0);
    //D0 -> pi+ K- pi+ pi-
    SelectParticles(Particles,fD04,PrimVtx,fCutsCharm[2],fCutsCharm[1],
                    KFParticleDatabase::Instance()->GetD0Mass(), KFParticleDatabase::Instance()->GetD0MassSigma(), fSecCuts[0]);
    //D0_bar -> pi- K+ pi- pi+
    SelectParticles(Particles,fD04bar,PrimVtx,fCutsCharm[2],fCutsCharm[1],
                    KFParticleDatabase::Instance()->GetD0Mass(), KFParticleDatabase::Instance()->GetD0MassSigma(), fSecCuts[0]);
    //D*+->D0 pi+
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fD04, 429, vRTracks[2], 1, vRTracks[2].FirstPion(), vRTracks[2].LastPion(),
                        Particles, PrimVtx, iPV, 0);
    //D0*- -> D0_bar pi-
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fD04bar, -429, vRTracks[3], -1, vRTracks[3].FirstPion(), vRTracks[3].LastPion(),
                        Particles, PrimVtx, iPV, 0);
    //D+
    SelectParticles(Particles,fDPlus,PrimVtx,fCutsCharm[2],fCutsCharm[1],
                    KFParticleDatabase::Instance()->GetDPlusMass(), KFParticleDatabase::Instance()->GetDPlusMassSigma(), fSecCuts[0]);
    //D-
    SelectParticles(Particles,fDMinus,PrimVtx,fCutsCharm[2],fCutsCharm[1],
                    KFParticleDatabase::Instance()->GetDPlusMass(), KFParticleDatabase::Instance()->GetDPlusMassSigma(), fSecCuts[0]);
    //D*0->D+ pi-
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fDPlus, 411, vRTracks[3], -1, vRTracks[3].FirstPion(), vRTracks[3].LastPion(),
                        Particles, PrimVtx, iPV, 0);
    //D*0_bar->D- pi+
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fDMinus, -411, vRTracks[2], 1, vRTracks[2].FirstPion(), vRTracks[2].LastPion(),
                        Particles, PrimVtx, iPV, 0);
    
    float cutsD0[3] = {fCutsCharm[1], fCutsCharm[2], fCutsCharm[0]};
//     float cutsD0[3] = {-5, 1e10, 1e10}; 
    //D0 -> K0 pi+ pi-
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fD0pipi, fPrimCandidates[0][iPV], Particles, PrimVtx, cutsD0, -1, 425, 0, 1);
    //D0 -> K0 K+ K-
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fD0KK, fPrimCandidates[0][iPV], Particles, PrimVtx, cutsD0, -1, 427, 0, 1);
    
    
    //LambdaC -> p pi+ pi-, Ds+ -> K+ pi+ pi-, D+ -> pi+ pi+ pi-
    FindTrackV0Decay(fD0pipi, 420, vRTracks[0], 1, vRTracks[0].FirstPion(), vRTracks[0].LastProton(),
                     Particles, PrimVtx, -1, &(ChiToPrimVtx[0]));
    //LambdaC_bar -> p_bar pi+ pi-, Ds- -> K- pi+ pi-, D- -> pi+ pi- pi-
    FindTrackV0Decay(fD0pipi, 420, vRTracks[1],-1, vRTracks[1].FirstPion(), vRTracks[1].LastProton(),
                     Particles, PrimVtx, -1, &(ChiToPrimVtx[1]));
    
    //D+ -> K0 pi+ pi+ pi-
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fDPlus3Pi, fPrimCandidates[0][iPV], Particles, PrimVtx, cutsD0, -1, 200411, 0, 1);
    //D- -> K0 pi+ pi- pi-
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fDMinus3Pi, fPrimCandidates[0][iPV], Particles, PrimVtx, cutsD0, -1, -200411, 0, 1);
    //Lc+ -> Lambda pi+ pi+ pi-
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fDPlus3Pi, fPrimCandidates[1][iPV], Particles, PrimVtx, cutsD0, -1, 404122, 0, 1);
    //Lc- -> Lambda_bar pi+ pi- pi-
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fDMinus3Pi, fPrimCandidates[2][iPV], Particles, PrimVtx, cutsD0, -1, -404122, 0, 1);
    //Xic0 -> Xi- pi+ pi+ pi-
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fDPlus3Pi, fPrimCandidates[5][iPV], Particles, PrimVtx, cutsD0, -1, 4132, 0, 1);
    //Xic0_bar -> Xi+ pi+ pi- pi-
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fDMinus3Pi, fPrimCandidates[6][iPV], Particles, PrimVtx, cutsD0, -1, -4132, 0, 1);
    
    //Ds+ -> K0 K+ pi+ pi-
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fDsPlusK2Pi, fPrimCandidates[0][iPV], Particles, PrimVtx, cutsD0, -1, 300431, 0, 1);
    //Ds- -> K0 K- pi+ pi-
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fDsMinusK2Pi, fPrimCandidates[0][iPV], Particles, PrimVtx, cutsD0, -1, -300431, 0, 1);
    
    //Lc+ -> p K0 pi+ pi-
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fLcPlusP2Pi, fPrimCandidates[0][iPV], Particles, PrimVtx, cutsD0, -1, 204122, 0, 1);
    //Lc- -> p- K0 pi+ pi-
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fLcMinusP2Pi, fPrimCandidates[0][iPV], Particles, PrimVtx, cutsD0, -1, -204122, 0, 1);
    
    //D0 -> pi+ pi-
    SelectParticles(Particles,fD0pipi,PrimVtx,fCutsCharm[2],fCutsCharm[1],
                    KFParticleDatabase::Instance()->GetD0Mass(), KFParticleDatabase::Instance()->GetD0MassSigma(), fSecCuts[0]);
    //D0 -> K+ K-
    SelectParticles(Particles,fD0KK,PrimVtx,fCutsCharm[2],fCutsCharm[1],
                    KFParticleDatabase::Instance()->GetD0Mass(), KFParticleDatabase::Instance()->GetD0MassSigma(), fSecCuts[0]);
    //D+ -> pi+ pi+ pi-
    SelectParticles(Particles,fDPlus3Pi,PrimVtx,fCutsCharm[2],fCutsCharm[1],
                    KFParticleDatabase::Instance()->GetDPlusMass(), KFParticleDatabase::Instance()->GetDPlusMassSigma(), fSecCuts[0]);
    //D- -> pi+ pi- pi-
    SelectParticles(Particles,fDMinus3Pi,PrimVtx,fCutsCharm[2],fCutsCharm[1],
                    KFParticleDatabase::Instance()->GetDPlusMass(), KFParticleDatabase::Instance()->GetDPlusMassSigma(), fSecCuts[0]);
    //Ds+ -> K+ pi+ pi-, 
    SelectParticles(Particles,fDsPlusK2Pi,PrimVtx,fCutsCharm[2],fCutsCharm[1],
                    KFParticleDatabase::Instance()->GetD0Mass(), KFParticleDatabase::Instance()->GetD0MassSigma(), fSecCuts[0]);
    //Ds- -> K- pi+ pi-
    SelectParticles(Particles,fDsMinusK2Pi,PrimVtx,fCutsCharm[2],fCutsCharm[1],
                    KFParticleDatabase::Instance()->GetD0Mass(), KFParticleDatabase::Instance()->GetD0MassSigma(), fSecCuts[0]);
    //LambdaC -> p pi+ pi-
    SelectParticles(Particles,fLcPlusP2Pi,PrimVtx,fCutsCharm[2],fCutsCharm[1],
                    KFParticleDatabase::Instance()->GetD0Mass(), KFParticleDatabase::Instance()->GetD0MassSigma(), fSecCuts[0]);
    //LambdaC_bar -> p_bar pi+ pi-
    SelectParticles(Particles,fLcMinusP2Pi,PrimVtx,fCutsCharm[2],fCutsCharm[1],
                    KFParticleDatabase::Instance()->GetD0Mass(), KFParticleDatabase::Instance()->GetD0MassSigma(), fSecCuts[0]);
    
    
    //D+ -> K0 pi+
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[0][iPV], 310, vRTracks[0],  1, vRTracks[0].FirstPion(), vRTracks[0].LastPion(),
                       Particles, PrimVtx, -1, &(ChiToPrimVtx[0]) );
    //D- -> K0 pi-
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[0][iPV], 310, vRTracks[1], -1, vRTracks[1].FirstPion(), vRTracks[1].LastPion(),
                       Particles, PrimVtx, -1, &(ChiToPrimVtx[1]) );
    //Ds+ -> K0 K+
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[0][iPV], 310, vRTracks[0],  1, vRTracks[0].FirstKaon(), vRTracks[0].LastKaon(),
                       Particles, PrimVtx, -1, &(ChiToPrimVtx[0]) );
    //Ds- -> K0 K-
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[0][iPV], 310, vRTracks[1], -1, vRTracks[1].FirstKaon(), vRTracks[1].LastKaon(),
                       Particles, PrimVtx, -1, &(ChiToPrimVtx[1]) );      
    //Lambdac+ -> Lambda pi+
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[1][iPV], 3122, vRTracks[0], 1, vRTracks[0].FirstPion(), vRTracks[0].LastPion(),
                       Particles, PrimVtx, -1, &(ChiToPrimVtx[0]) );
    //Lambdac_bar- -> Lambda_bar pi-
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[2][iPV], -3122, vRTracks[1], -1, vRTracks[1].FirstPion(), vRTracks[1].LastPion(),
                       Particles, PrimVtx, -1, &(ChiToPrimVtx[1]) );
    //Lambdac+ -> p K0s
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[0][iPV], 310, vRTracks[0], 1, vRTracks[0].FirstProton(), vRTracks[0].LastProton(),
                       Particles, PrimVtx, -1, &(ChiToPrimVtx[0]) );
    //Lambdac_bar- -> p_bar K0s
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[0][iPV], 310, vRTracks[1], -1, vRTracks[1].FirstProton(), vRTracks[1].LastProton(),
                       Particles, PrimVtx, -1, &(ChiToPrimVtx[1]) );   
      
    //H0 -> Lambda Lambda
    CombinePartPart(fSecCandidates[1], fSecCandidates[1], Particles, PrimVtx, fCutsPartPart[0], -1, 3000, 1, 1);
    //H0 -> Lambda p pi-
    FindTrackV0Decay(fLPi, 3002, vRTracks[0], 1, vRTracks[0].FirstProton(), vRTracks[0].LastProton(),
                      Particles, PrimVtx, -1);
    //Sigma0 -> Lambda Gamma
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fPrimCandidates[3][iPV], fPrimCandidates[1][iPV], Particles, PrimVtx, fCutsPartPart[1], iPV, 3212);
    //Sigma0_bar -> Lambda_bar Gamma
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fPrimCandidates[3][iPV], fPrimCandidates[2][iPV], Particles, PrimVtx, fCutsPartPart[1], iPV, -3212);
    //pi0 -> gamma gamma
    const float& mPi0 = KFParticleDatabase::Instance()->GetPi0Mass();
    const float& mPi0Sigma = KFParticleDatabase::Instance()->GetPi0MassSigma();
    CombinePartPart(fSecCandidates[3], fSecCandidates[3], Particles, PrimVtx, fCutsPartPart[1], -1, 111, 1, 0, &fPrimCandidates[4], &fSecCandidates[4], mPi0, mPi0Sigma);
    for(int iPV=0; iPV<fNPV; iPV++)
    {
      CombinePartPart(fPrimCandidates[3][iPV], fPrimCandidates[3][iPV], Particles, PrimVtx, fCutsPartPart[1], iPV, 111, 1, 0, &fPrimCandidates[4], &fSecCandidates[4], mPi0, mPi0Sigma);
      CombinePartPart(fSecCandidates[3],       fPrimCandidates[3][iPV], Particles, PrimVtx, fCutsPartPart[1],  -1, 111, 0, 0, &fPrimCandidates[4], &fSecCandidates[4], mPi0, mPi0Sigma);
    }
    for(int iPV=0; iPV<fNPV; iPV++ )
      ExtrapolateToPV(fPrimCandidates[4][iPV],PrimVtx[iPV]);
    //eta -> pi0 pi0 pi0
    //TODO implement this
    //Sigma+ -> p pi0
    FindTrackV0Decay(fSecCandidates[4], 111, vRTracks[0],  1, vRTracks[0].FirstProton(), vRTracks[0].LastProton(),
                      Particles, PrimVtx, -1);
    //Sigma+_bar -> p- pi0
    FindTrackV0Decay(fSecCandidates[4], 111, vRTracks[1], -1, vRTracks[1].FirstProton(), vRTracks[1].LastProton(),
                      Particles, PrimVtx, -1);
    //Xi0 -> Lambda pi0
    CombinePartPart(fSecCandidates[4], fSecCandidates[1], Particles, PrimVtx, fCutsPartPart[0], -1, 3322);
    //Xi0_bar -> Lambda_bar pi0
    CombinePartPart(fSecCandidates[4], fSecCandidates[2], Particles, PrimVtx, fCutsPartPart[0], -1, -3322);
    //K*+ -> K+ pi0
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[4][iPV], 111, vRTracks[2],  1, vRTracks[2].FirstKaon(), vRTracks[2].LastKaon(),
                        Particles, PrimVtx, -1);
    //K*- -> K- pi0
    for(int iPV=0; iPV<fNPV; iPV++)
      FindTrackV0Decay(fPrimCandidates[4][iPV], 111, vRTracks[3], -1, vRTracks[3].FirstKaon(), vRTracks[3].LastKaon(),
                        Particles, PrimVtx, -1);
    //K*0 -> K0 pi0
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fPrimCandidates[4][iPV], fPrimCandidates[0][iPV], Particles, PrimVtx, fCutsPartPart[1], iPV, 100313, 0, 1);    
    //Sigma*0 -> Lambda pi0
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fPrimCandidates[4][iPV], fPrimCandidates[1][iPV], Particles, PrimVtx, fCutsPartPart[1], iPV, 3214, 0, 1);       
    //Sigma*0_bar -> Lambda_bar pi0
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fPrimCandidates[4][iPV], fPrimCandidates[2][iPV], Particles, PrimVtx, fCutsPartPart[1], iPV, -3214, 0, 1);       
    //Xi*- -> Xi- pi0
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fPrimCandidates[4][iPV], fPrimCandidates[5][iPV], Particles, PrimVtx, fCutsPartPart[1], iPV, 3314, 0, 1);   
    //Xi*+ -> Xi+ pi0
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fPrimCandidates[4][iPV], fPrimCandidates[6][iPV], Particles, PrimVtx, fCutsPartPart[1], iPV, -3314, 0, 1);  
    //JPsi -> Lambda Lambda_bar
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fPrimCandidates[1][iPV], fPrimCandidates[2][iPV], Particles, PrimVtx, fCutsPartPart[1], iPV, 300443, 0, 1);  
    //JPsi -> Xi- Xi+
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fPrimCandidates[5][iPV], fPrimCandidates[6][iPV], Particles, PrimVtx, fCutsPartPart[1], iPV, 400443, 0, 1);  
    //Psi -> Omega- Omega+
    for(int iPV=0; iPV<fNPV; iPV++)
      CombinePartPart(fPrimCandidates[7][iPV], fPrimCandidates[8][iPV], Particles, PrimVtx, fCutsPartPart[1], iPV, 500443, 0, 1);  
    
    //reconstruct particles with daughters in ElectroMagnetic Calorimeter
//     if(fEmcClusters)
//     {
//       //pi0 -> gamma gamma, EMC
//       vector< vector<KFParticle> > vPi0PrimEmc(1);
//       vector<KFParticle> vPi0SecEmc;
//       vector< vector<KFParticle> > vD0PrimEmc(1);
//       CombinePartPart(vGammaPrimEmc, vGammaPrimEmc, Particles, PrimVtx, fCutsPartPart[1], 0, 111, 1, 0, &vPi0PrimEmc, &vPi0SecEmc, mPi0, mPi0Sigma);
//           
//       //D+ -> K0 pi+
//       FindTrackV0Decay(fSecCandidates[0],    310, vRTracks[0],  1, vRTracks[0].FirstPion(), vRTracks[0].LastPion(), Particles, PrimVtx, -1/*, &(ChiToPrimVtx[0])*/);
//       //D0 -> K0 pi+ pi-
//       FindTrackV0Decay(fK0PiPlus, 100411, vRTracks[1], -1, vRTracks[1].FirstPion(), vRTracks[1].LastPion(), Particles, PrimVtx, -1/*, &(ChiToPrimVtx[0])*/);
//       //D0 -> K0 pi+ pi- pi0
//       CombinePartPart(fK0PiPi, vPi0PrimEmc[0], Particles, PrimVtx, fCutsPartPart[1], -1, 428, 0, 0, &vD0PrimEmc, 0, 
//                       KFParticleDatabase::Instance()->GetD0Mass(), 0.025);
//       
//       for(int iPV=0; iPV<1; iPV++ )
//       {
//         ExtrapolateToPV(vPi0PrimEmc[iPV],PrimVtx[iPV]);
//         ExtrapolateToPV(vD0PrimEmc[iPV],PrimVtx[iPV]);
//       }
//       //D0* -> D0 pi0
//       CombinePartPart(vD0PrimEmc[0], vPi0PrimEmc[0], Particles, PrimVtx, fCutsPartPart[1], 0, 10428);
//     }
  }
  else
  {
    //D0 -> pi+ K-
    SelectParticles(Particles,fD0,PrimVtx,fCutsCharm[2],fCutsCharm[1],
                    KFParticleDatabase::Instance()->GetD0Mass(), KFParticleDatabase::Instance()->GetD0MassSigma(), fSecCuts[0]);
    //D0_bar -> pi+ K-
    SelectParticles(Particles,fD0bar,PrimVtx,fCutsCharm[2],fCutsCharm[1],
                    KFParticleDatabase::Instance()->GetD0Mass(), KFParticleDatabase::Instance()->GetD0MassSigma(), fSecCuts[0]);
  }
  #endif
}

void KFParticleFinder::ExtrapolateToPV(vector<KFParticle>& vParticles, KFParticleSIMD& PrimVtx)
{
  /** Extrapolates all particles from the input vector to the DCA point with the primary vertex.
   ** \param[in,out] vParticles - array of particles to be transported.
   ** \param[in] PrimVtx - the primary vertex, where particles should be transported.
   **/
  KFParticle* parts[SimdLen];
  KFParticle tmpPart[SimdLen];
  
  for(int iv=0; iv<SimdLen; iv++)
    parts[iv] = &tmpPart[iv];
    
  for(unsigned int iL=0; iL<vParticles.size(); iL += SimdLen)
  {

    unsigned int nPart = vParticles.size();
    unsigned int nEntries = (iL + SimdLen < nPart) ? SimdLen : (nPart - iL);

    
    for(unsigned int iv=0; iv<nEntries; iv++)
      tmpPart[iv] = vParticles[iL+iv];

    KFParticleSIMD tmp(parts,nEntries);

    tmp.TransportToPoint(PrimVtx.Parameters());

    for(unsigned int iv=0; iv<nEntries; iv++)
    {
      tmp.GetKFParticle(vParticles[iL+iv], iv);
    }
  }
}

inline void KFParticleFinder::ConstructV0(KFPTrackVector* vTracks,
                                          int iTrTypePos,
                                          int iTrTypeNeg,
                                          int32_v& idPosDaughters,
                                          int32_v& idNegDaughters,
                                          int32_v& daughterPosPDG,
                                          int32_v& daughterNegPDG,
                                          KFParticleSIMD& mother,
                                          KFParticle& mother_temp,
                                          const unsigned short NTracks,
                                          kfvector_floatv& l,
                                          kfvector_floatv& dl,
                                          vector<KFParticle>& Particles,
                                          std::vector<KFParticleSIMD, KFPSimdAllocator<KFParticleSIMD> >& PrimVtx,
                                          const float* cuts,
                                          const int32_v& pvIndex,
                                          const float* secCuts,
                                          const float32_v& massMotherPDG,
                                          const float32_v& massMotherPDGSigma,
                                          KFParticleSIMD& motherPrimSecCand,
                                          int& nPrimSecCand,
                                          vector< vector<KFParticle> >* vMotherPrim,
                                          vector<KFParticle>* vMotherSec
                                         )
{
  /** Combines two SIMD vectors of particles into 2-daughter candidate.
   ** \param[in] vRTracks - pointer to the array with vectors of tracks:\n
   ** 0) secondary positive at the first hit position; \n
   ** 1) secondary negative at the first hit position; \n
   ** 2) primary positive at the first hit position; \n
   ** 3) primary negative at the first hit position; \n
   ** 4) secondary positive at the last hit position; \n
   ** 5) secondary negative at the last hit position; \n
   ** 6) primary positive at the last hit position; \n
   ** 7) primary negative at the last hit position. \n
   ** \param[in] iTrTypePos - index of the first vector with tracks in the vTracks array.
   ** \param[in] iTrTypeNeg - index of the second vector with tracks in the vTracks array.
   ** \param[in] idPosDaughters - indices of particles from the first vector of tracks.
   ** \param[in] idNegDaughters - indices of particles from the second vector of tracks.
   ** \param[in] daughterPosPDG - PDG hypothesis of the first SIMD vector of tracks.
   ** \param[in] daughterNegPDG - PDG hypothesis of the second SIMD vector of tracks.
   ** \param[out] mother - constructed 2-daughter SIMD-candidate.
   ** \param[in] mother_temp - temporary object to extract KFParticle from constructed KFParticleSIMD mother. Preallocated for better performance.
   ** \param[in] NTracks - number of tracks in each SIMD vector.
   ** \param[in] l - SIMD-vector with extracted distance to the primary vertex. Is preallocated for better performance.
   ** \param[in] dl - SIMD-vector with extracted error of distance to the primary vertex. Is preallocated for better performance.
   ** \param[out] Particles - the output array with the reconstructed particle-candidates.
   ** \param[in] PrimVtx - array with primary vertices.
   ** \param[in] cuts - set of cuts: \f$\chi^2_{prim}\f$, \f$\chi^2_{geo}\f$, \f$l/\Delta l\f$.
   ** \param[in] pvIndex - index of the primary vertex for reconstruction of resonances. Tracks should come from the same vertex.
   ** in case of other particles the value should be "-1".
   ** \param[in] secCuts - cuts to select primary and secondary candidates from the reconstructed set: \f$\sigma_{M}\f$, \f$\chi^2_{topo}\f$, \f$l/\Delta l\f$.
   ** \param[in] massMotherPDG - PDG table mass for the mother particle, is used for selection of primary and secondary candidates.
   ** \param[in] massMotherPDGSigma - sigma of the peak width, is used for selection of primary and secondary candidates.
   ** \param[out] motherPrimSecCand - a SIMD-particle with possible primary and secondary candidates.
   ** \param[out] nPrimSecCand - number of possible primary and secondary candidates. Can be "0" if no of them are found.
   ** \param[out] vMotherPrim - array with output primary candidates if any.
   ** \param[out] vMotherSec - array with output secondary candidates if any.
   **/
  mask32_v isPrimary = pvIndex > -1;
  int32_v trackId;
  KFParticleSIMD posDaughter(vTracks[iTrTypePos],idPosDaughters, daughterPosPDG);
  trackId.gather( &(vTracks[iTrTypePos].Id()[0]), idPosDaughters );
  posDaughter.SetId(trackId);

  KFParticleSIMD negDaughter(vTracks[iTrTypeNeg],idNegDaughters, daughterNegPDG);
  trackId.gather( &(vTracks[iTrTypeNeg].Id()[0]), idNegDaughters );
  negDaughter.SetId(trackId);   
#ifdef CBM
  float32_v ds[2] = {0.f,0.f};
  float32_v dsdr[4][6];
  negDaughter.GetDStoParticle( posDaughter, ds, dsdr );
  negDaughter.TransportToDS(ds[0], dsdr[0]);
  posDaughter.TransportToDS(ds[1], dsdr[3]);
#endif
  const KFParticleSIMD* vDaughtersPointer[2] = {&negDaughter, &posDaughter};
  mother.Construct(vDaughtersPointer, 2, 0);
  
  mask32_v saveParticle = int32_v::indicesSequence() < int(NTracks);
  float32_v chi2Cut = cuts[1];
  float32_v ldlCut  = cuts[2];
  const mask32_v isD0 = (abs(mother.PDG()) == 421 || abs(mother.PDG()) == 426 || abs(mother.PDG()) == 420);
  if( !(isD0).isEmpty() )
  {
    chi2Cut = select(isD0, fCutsCharm[0], chi2Cut);
    ldlCut = select(isD0, -1, ldlCut);//fCutsCharm[1];
  }
  
  saveParticle &= (mother.Chi2()/toFloat(mother.NDF()) < chi2Cut );
  saveParticle &= isFinite(mother.GetChi2());
  saveParticle &= (mother.GetChi2() > 0.0f);
  saveParticle &= (mother.GetChi2() == mother.GetChi2());

  if( saveParticle.isEmpty() ) return;
  
  float32_v lMin(1.e8f);
  float32_v ldlMin(1.e8f);
  mask32_v isParticleFromVertex;

  for(int iP=0; iP<fNPV; iP++)
  {
    mask32_v isParticleFromVertexLocal;
    mother.GetDistanceToVertexLine(PrimVtx[iP], l[iP], dl[iP], &isParticleFromVertexLocal);
    isParticleFromVertex |= isParticleFromVertexLocal;
    float32_v ldl = (l[iP]/dl[iP]);
    lMin = select( (l[iP] < lMin) && saveParticle, l[iP], lMin);
    ldlMin = select( (ldl < ldlMin) && saveParticle, ldl, ldlMin);
  }

  saveParticle &= (lMin < 200.f);
#ifdef NonhomogeneousField  
  KFParticleSIMD motherTopo;
    ldlMin = 1.e8f;
  for(int iP=0; iP<fNPV; iP++)
  {
    motherTopo = mother;
    motherTopo.SetProductionVertex(PrimVtx[iP]);
    motherTopo.GetDecayLength(l[iP], dl[iP]);
    float32_v ldl = (l[iP]/dl[iP]);
    ldlMin( (ldl < ldlMin) && saveParticle) = ldl;
  }
#endif
  saveParticle &= ( ((!isPrimary) && ldlMin > ldlCut) || isPrimary );
  
  saveParticle &= ((!isPrimary) && isParticleFromVertex) || isPrimary;
  if( saveParticle.isEmpty() ) return;
  
  const mask32_v isK0     = saveParticle && (mother.PDG() == int32_v(310));
  const mask32_v isLambda = saveParticle && (abs(mother.PDG()) == int32_v(3122));
  const mask32_v isGamma  = saveParticle && (mother.PDG() == int32_v(22));
  const mask32_v isHyperNuclei = saveParticle && (abs(mother.PDG()) > 3000 && abs(mother.PDG()) < 3104);
  
  saveParticle &= ( ((isK0 || isLambda || isHyperNuclei) && lMin > float32_v(fLCut)) || !(isK0 || isLambda || isHyperNuclei) );

  mask32_v saveMother;
  
  if( !(isK0.isEmpty()) || !(isLambda.isEmpty()) || !(isGamma.isEmpty()))
  { 
    float32_v mass, errMass;

    mother.GetMass(mass, errMass);
    saveMother = saveParticle;
    saveMother &= (abs(mass - massMotherPDG)/massMotherPDGSigma) < secCuts[0];
    saveMother &= ((ldlMin > secCuts[2]) && !isGamma) || isGamma;
    saveMother &= (isK0 || isLambda || isGamma);
  }
  
  for(int iv=0; iv<NTracks; iv++)
  {
    if(!saveParticle[iv]) continue;
  
    mother.GetKFParticle(mother_temp, iv);
    int motherId = Particles.size();
    mother_temp.SetId(Particles.size());
    if( mother.PDG()[iv] == 421 ) 
    {
      fD0.push_back(mother_temp);
      continue;
    }
    if( mother.PDG()[iv] == -421 ) 
    {
      fD0bar.push_back(mother_temp);
      continue;
    }
    if( mother.PDG()[iv] == 426 ) 
    {
      fD0KK.push_back(mother_temp);
      continue;
    }
    if( mother.PDG()[iv] == 420 ) 
    {
      fD0pipi.push_back(mother_temp);
      continue;
    }
    if( mother.PDG()[iv] == 3003)
      fDPi.push_back(mother_temp);
    if( mother.PDG()[iv] ==-3003)
      fDPiBar.push_back(mother_temp);
    if( mother.PDG()[iv] == 3103)
      fTPi.push_back(mother_temp);
    if( mother.PDG()[iv] ==-3103)
      fTPiBar.push_back(mother_temp);
    if( mother.PDG()[iv] == 3004)
      fHe3Pi.push_back(mother_temp);
    if( mother.PDG()[iv] ==-3004)
      fHe3PiBar.push_back(mother_temp);
    if( mother.PDG()[iv] == 3005)
      fHe4Pi.push_back(mother_temp);
    if( mother.PDG()[iv] ==-3005)
      fHe4PiBar.push_back(mother_temp);
    
    if( mother.PDG()[iv] == 310) {
      fPipi.push_back(mother_temp);
      fPipi[fPipi.size()-1].SetPDG(100310);
    }
    if( mother.PDG()[iv] == 3122) {
      fPpi.push_back(mother_temp);
      fPpi[fPpi.size()-1].SetPDG(8122);
    }
    if( mother.PDG()[iv] ==-3122) {
      fPpiBar.push_back(mother_temp);
      fPpiBar[fPpiBar.size()-1].SetPDG(-8122);
    }
    
    Particles.push_back(mother_temp);
    
    if( mother.PDG()[iv] == 22 && isPrimary[iv] )
    {
      float negPt2 = negDaughter.Px()[iv]*negDaughter.Px()[iv] + negDaughter.Py()[iv]*negDaughter.Py()[iv];
      float posPt2 = posDaughter.Px()[iv]*posDaughter.Px()[iv] + posDaughter.Py()[iv]*posDaughter.Py()[iv];

      if( (negPt2 >fCutLVMPt*fCutLVMPt) && (posPt2 >fCutLVMPt*fCutLVMPt) )
      {
        mother_temp.SetPDG(100113);
        mother_temp.SetId(Particles.size());
        Particles.push_back(mother_temp);
        
        if( (negPt2 >fCutJPsiPt*fCutJPsiPt) && (posPt2 >fCutJPsiPt*fCutJPsiPt) )
        {
          mother_temp.SetPDG(443);
          mother_temp.SetId(Particles.size());
          Particles.push_back(mother_temp);
        }
      }  
    }

    if( mother.PDG()[iv] == 200113 )
    {
      float negPt2 = negDaughter.Px()[iv]*negDaughter.Px()[iv] + negDaughter.Py()[iv]*negDaughter.Py()[iv];
      float posPt2 = posDaughter.Px()[iv]*posDaughter.Px()[iv] + posDaughter.Py()[iv]*posDaughter.Py()[iv];
      
      if( (negPt2 >fCutJPsiPt*fCutJPsiPt) && (posPt2 >fCutJPsiPt*fCutJPsiPt) && (abs(daughterPosPDG[iv]) == 13) && (abs(daughterNegPDG[iv]) == 13))
      {
        mother_temp.SetPDG(100443);
        mother_temp.SetId(Particles.size());
        Particles.push_back(mother_temp);
      }  
    }
    
    if(saveMother[iv])
    {
      mother.SetId(motherId);
      motherPrimSecCand.SetOneEntry(nPrimSecCand,mother,iv);

      nPrimSecCand++;
      if(nPrimSecCand==SimdLen)
      {
        SaveV0PrimSecCand(motherPrimSecCand,nPrimSecCand,mother_temp,PrimVtx,secCuts,vMotherPrim,vMotherSec);
        nPrimSecCand = 0;
      }
    }
  }
}

inline void KFParticleFinder::SaveV0PrimSecCand(KFParticleSIMD& mother,
                                                int& NParticles,
                                                KFParticle& mother_temp,
                                                std::vector<KFParticleSIMD, KFPSimdAllocator<KFParticleSIMD> >& PrimVtx,
                                                const float* secCuts,
                                                vector< vector<KFParticle> >* vMotherPrim,
                                                vector<KFParticle>* vMotherSec)
{
  /** The function which decides if primary and secondary candidates found by KFParticleFinder::ConstructV0()
   ** should be stored and stores them to the provided arrays.
   ** \param[in] mother - constructed SIMD vector of particle candidates. 
   ** \param[in] NParticles - number of particles in the SIMD vector.
   ** \param[in] mother_temp - temporary object to extract KFParticle from constructed KFParticleSIMD mother. Preallocated for better performance.
   ** \param[in] PrimVtx - array with primary vertices.
   ** \param[in] secCuts - cuts to select primary and secondary candidates from the reconstructed set: \f$\sigma_{M}\f$, \f$\chi^2_{topo}\f$, \f$l/\Delta l\f$.
   ** \param[out] vMotherPrim - array with output primary candidates if any.
   ** \param[out] vMotherSec - array with output secondary candidates if any.
   **/
  
  KFParticleSIMD motherTopo;
  float32_v massMotherPDG, massMotherPDGSigma;
  
  mask32_v isSec;
  mask32_v isPrim;
  vector<int> iPrimVert[SimdLen];

  KFParticleDatabase::Instance()->GetMotherMass(mother.PDG(),massMotherPDG,massMotherPDGSigma);
  
  const mask32_v isK0     = (mother.PDG() == int32_v(310));
  const mask32_v isLambda = (abs(mother.PDG()) == int32_v(3122));
  const mask32_v isGamma  = (mother.PDG() == int32_v(22));

  int32_v arrayIndex(-1); //for saving primary candidates; 

  arrayIndex = select(mother.PDG() ==   int32_v(310), 0, arrayIndex);
  arrayIndex = select(mother.PDG() ==  int32_v(3122), 1, arrayIndex);
  arrayIndex = select(mother.PDG() == int32_v(-3122), 2, arrayIndex);
  arrayIndex = select(mother.PDG() ==    int32_v(22), 3, arrayIndex);

  mask32_v isPrimaryPart;

  float32_v chi2TopoMin = 1.e4f;
  
  for(int iP=0; iP< fNPV; iP++)
  {
    motherTopo = mother;
    motherTopo.SetProductionVertex(PrimVtx[iP]);
    
    const float32_v& motherTopoChi2Ndf = motherTopo.GetChi2()/toFloat(motherTopo.GetNDF());
    chi2TopoMin = select(motherTopoChi2Ndf < chi2TopoMin, motherTopoChi2Ndf, chi2TopoMin);
    const mask32_v isPrimaryPartLocal = ( motherTopoChi2Ndf < secCuts[1] );
    if(isPrimaryPartLocal.isEmpty()) continue;
    isPrimaryPart |= isPrimaryPartLocal;
    for(int iV=0; iV<NParticles; iV++)
    {
      if(isPrimaryPartLocal[iV])
      {
        motherTopo.GetKFParticle(mother_temp, iV);
        fPrimCandidatesTopo[arrayIndex[iV]][iP].push_back(mother_temp);
        iPrimVert[iV].push_back(iP);
      }
    }
    
    motherTopo.SetNonlinearMassConstraint(massMotherPDG);
    for(int iV=0; iV<NParticles; iV++)
    {
      if(isPrimaryPartLocal[iV])
      {
        motherTopo.GetKFParticle(mother_temp, iV);
        fPrimCandidatesTopoMass[arrayIndex[iV]][iP].push_back(mother_temp);
      }
    }
  }
  
  isPrim |= ( ( isPrimaryPart ) && (isK0 || isLambda || isGamma) );
#ifdef __ROOT__
  isSec  |= ( (!isPrimaryPart ) && (isK0 || isLambda || isGamma) && (chi2TopoMin < float32_v(500.f)) );
#else
  isSec  |= ( (!isPrimaryPart ) && (isK0 || isLambda || isGamma) );
#endif
  
  mother.SetNonlinearMassConstraint(massMotherPDG);

  for(int iv=0; iv<NParticles; iv++)
  { 
    if(isPrim[iv] || isSec[iv])
    {  
      mother.GetKFParticle(mother_temp, iv);
      
      if(isPrim[iv] )
      {
        for(unsigned int iP = 0; iP<iPrimVert[iv].size(); iP++)
          vMotherPrim[arrayIndex[iv]][iPrimVert[iv][iP]].push_back(mother_temp);
      }
      
      if(isSec[iv] )
        vMotherSec[arrayIndex[iv]].push_back(mother_temp);
    }
  }
}

void KFParticleFinder::Find2DaughterDecay(KFPTrackVector* vTracks, kfvector_float* ChiToPrimVtx,
                                          vector<KFParticle>& Particles,
                                          std::vector<KFParticleSIMD, KFPSimdAllocator<KFParticleSIMD> >& PrimVtx,
                                          const float* cuts,
                                          const float* secCuts,
                                          vector< vector<KFParticle> >* vMotherPrim,
                                          vector<KFParticle>* vMotherSec )
{
  /** Reconstructs all 2-daughter decays.
   ** \param[in] vRTracks - pointer to the array with vectors of tracks:\n
   ** 0) secondary positive at the first hit position; \n
   ** 1) secondary negative at the first hit position; \n
   ** 2) primary positive at the first hit position; \n
   ** 3) primary negative at the first hit position; \n
   ** 4) secondary positive at the last hit position; \n
   ** 5) secondary negative at the last hit position; \n
   ** 6) primary positive at the last hit position; \n
   ** 7) primary negative at the last hit position. \n
   ** \param[in] ChiToPrimVtx - arrays with vectors of the \f$\chi^2_{prim}\f$ deviations for track vectors 1) and 2).
   ** \param[out] Particles - output vector with particles.
   ** \param[in] PrimVtx - vector with primary vertices.
   ** \param[in] cuts - set of cuts: \f$\chi^2_{prim}\f$, \f$\chi^2_{geo}\f$, \f$l/\Delta l\f$.
   ** \param[in] secCuts - cuts to select primary and secondary candidates from the reconstructed set: \f$\sigma_{M}\f$, \f$\chi^2_{topo}\f$, \f$l/\Delta l\f$.
   ** \param[out] vMotherPrim - array with output primary candidates.
   ** \param[out] vMotherSec - array with output secondary candidates.
   **/
  KFParticle mother_temp;
  KFParticleSIMD mother;
  kfvector_floatv l(fNPV), dl(fNPV);

  KFParticleSIMD daughterNeg, daughterPos;
    
  // for secondary V0
  unsigned int nBufEntry = 0;
  alignas(SimdSize) std::int32_t idNegDaughtersArray[SimdLen];
  alignas(SimdSize) std::int32_t idPosDaughtersArray[SimdLen];
  alignas(SimdSize) std::int32_t daughterPosPDGArray[SimdLen];
  alignas(SimdSize) std::int32_t daughterNegPDGArray[SimdLen];
  alignas(SimdSize) std::int32_t pvIndexMotherArray[SimdLen];
  alignas(SimdSize) std::int32_t V0PDGArray[SimdLen];

  int32_v idNegDaughters;
  int32_v idPosDaughters;
  int32_v daughterPosPDG(-1);
  int32_v daughterNegPDG(-1);

  int32_v pvIndexMother(-1);
  
  float32_v massMotherPDG(0.f), massMotherPDGSigma(0.f);
  int32_v V0PDG(0);

  KFParticleSIMD motherPrimSecCand;
  int nPrimSecCand =0;
  
  int trTypeIndexPos[2] = {0,2};
  int trTypeIndexNeg[2] = {1,3};

  for( int iTrTypeNeg = 0; iTrTypeNeg<2; iTrTypeNeg++)
  {
    KFPTrackVector& negTracks = vTracks[ trTypeIndexNeg[iTrTypeNeg] ];
    
    for(int iTrTypePos=0; iTrTypePos<2; iTrTypePos++)
    {
      KFPTrackVector& posTracks = vTracks[ trTypeIndexPos[iTrTypePos] ];
      int32_v negTracksSize = negTracks.LastBe7(); //negTracks.Size();
      int nPositiveTracks = posTracks.LastBe7(); //posTracks.Size();
      
      //track categories
      int nTC = 5;
      int startTCPos[5] = {0};
      int endTCPos[5] = {0};
      int startTCNeg[5] = {0};
      int endTCNeg[5] = {0};
      
      if((iTrTypeNeg == 0) && (iTrTypePos == 0))
      {
        // Secondary particles
        nTC = 5;
        // e-
        startTCPos[0] = 0; endTCPos[0] = posTracks.LastElectron(); //nPositiveTracks;
        startTCNeg[0] = 0; endTCNeg[0] = negTracks.LastElectron(); //negTracksSize[0]; 
        //mu-
        startTCPos[1] = 0; endTCPos[1] = 0;
        startTCNeg[1] = 0; endTCNeg[1] = 0; 
        //pi- + ghosts
        startTCPos[2] = posTracks.FirstPion(); endTCPos[2] = nPositiveTracks;
        startTCNeg[2] = negTracks.FirstPion(); endTCNeg[2] = negTracks.LastPion();        
        //K-
        startTCPos[3] = posTracks.FirstPion(); endTCPos[3] = posTracks.LastKaon();
        startTCNeg[3] = negTracks.FirstKaon(); endTCNeg[3] = negTracks.LastKaon();  
        //p-, d-, t-, he3-, he4-, he6-, li6-, li7-, be7-
        startTCPos[4] = posTracks.FirstPion(); endTCPos[4] = posTracks.LastPion();
        startTCNeg[4] = negTracks.FirstProton(); endTCNeg[4] = negTracksSize[0];  
      }
      
      if( iTrTypeNeg != iTrTypePos )
      {
        //Mixed particles - only gamma -> e+ e-
        nTC = 1;
        startTCPos[0] = 0; endTCPos[0] = posTracks.LastElectron(); // nPositiveTracks;
        startTCNeg[0] = 0; endTCNeg[0] = negTracks.LastElectron(); // negTracksSize[0];
      }
      
      if((iTrTypeNeg == 1) && (iTrTypePos == 1))
      {
        //primary particles
        nTC = 5;
        // e-
        startTCPos[0] = 0; endTCPos[0] = posTracks.LastElectron(); //nPositiveTracks; 
        startTCNeg[0] = 0; endTCNeg[0] = negTracks.LastElectron(); //negTracksSize[0];
        //mu-
        startTCPos[1] = posTracks.FirstMuon(); endTCPos[1] = posTracks.LastMuon();
        startTCNeg[1] = negTracks.FirstMuon(); endTCNeg[1] = negTracks.LastMuon(); 
        //pi- + ghosts
        startTCPos[2] = posTracks.FirstPion(); endTCPos[2] = nPositiveTracks;
        startTCNeg[2] = negTracks.FirstPion(); endTCNeg[2] = negTracks.LastPion();        
        //K-
        startTCPos[3] = posTracks.FirstPion(); endTCPos[3] = nPositiveTracks;
        startTCNeg[3] = negTracks.FirstKaon(); endTCNeg[3] = negTracks.LastKaon();  
        //p-
        startTCPos[4] = posTracks.FirstPion(); endTCPos[4] = posTracks.LastProton();
        startTCNeg[4] = negTracks.FirstProton(); endTCNeg[4] = negTracks.LastProton();      
      }
      
      for(int iTC=0; iTC<nTC; iTC++)
      {
        for(int iTrN=startTCNeg[iTC]; iTrN < endTCNeg[iTC]; iTrN += SimdLen)
        {
          const int NTracksNeg = (iTrN + SimdLen < negTracksSize[0]) ? SimdLen : (negTracksSize[0] - iTrN);

          int32_v negInd = int32_v::indicesSequence() + int(iTrN);

          int32_v negPDG = reinterpret_cast<const int32_v&>(negTracks.PDG()[iTrN]);
          int32_v negPVIndex = reinterpret_cast<const int32_v&>(negTracks.PVIndex()[iTrN]);
          int32_v negNPixelHits = reinterpret_cast<const int32_v&>(negTracks.NPixelHits()[iTrN]);
          
          int32_v trackPdgNeg = negPDG;
          mask32_v activeNeg = (negPDG != -1);
#ifdef CBM          
          if( !((negPDG == -1).isEmpty()) )
          {
            trackPdgNeg(negPVIndex<0 && (negPDG == -1) ) = -211;
                
            activeNeg |= (negPVIndex < 0) && (negPDG == -1) ;
          }
#endif    
          activeNeg &= (int32_v::indicesSequence() < int(NTracksNeg));
              
          daughterNeg.Load(negTracks, iTrN, negPDG);
                
          float32_v chiPrimNeg(0.f);
          float32_v chiPrimPos(0.f);
          
          if( (iTrTypeNeg == 0) && (iTrTypePos == 0) )
            chiPrimNeg = reinterpret_cast<const float32_v&>( ChiToPrimVtx[trTypeIndexNeg[iTrTypeNeg]][iTrN]);
          
          for(int iTrP=startTCPos[iTC]; iTrP < endTCPos[iTC]; iTrP += SimdLen)
          {
            const int NTracks = (iTrP + SimdLen < nPositiveTracks) ? SimdLen : (nPositiveTracks - iTrP);

            const int32_v& posPDG = reinterpret_cast<const int32_v&>(posTracks.PDG()[iTrP]);
            const int32_v& posPVIndex = reinterpret_cast<const  int32_v&>(posTracks.PVIndex()[iTrP]);     
            const int32_v& posNPixelHits = reinterpret_cast<const int32_v&>(posTracks.NPixelHits()[iTrP]);
            const mask32_v& isPosSecondary = (posPVIndex < 0);

            daughterPos.Load(posTracks, iTrP, posPDG);
            
            if( (iTrTypeNeg == 0) && (iTrTypePos == 0) )
              chiPrimPos = reinterpret_cast<const float32_v&>( ChiToPrimVtx[trTypeIndexPos[iTrTypePos]][iTrP]);
            
            for(int iRot = 0; iRot<SimdLen; iRot++)
            {
//               if(iRot>0)
              {
                negPDG = negPDG.rotate<1>();
                negPVIndex = negPVIndex.rotate<1>();
                negNPixelHits = negNPixelHits.rotate<1>();
                negInd = negInd.rotate<1>();
                trackPdgNeg = trackPdgNeg.rotate<1>();
              
                daughterNeg.Rotate();
                chiPrimNeg = chiPrimNeg.rotate<1>();

                activeNeg = ( (negPDG != -1) || ( (negPVIndex < 0) && (negPDG == -1) ) ) && (negInd < negTracksSize);
              }
              const mask32_v& isSecondary = ( negPVIndex < 0 ) && isPosSecondary;
              const mask32_v& isPrimary   = ( negPVIndex >= 0 ) && (!isPosSecondary);
            
              const mask32_v closeDaughters = (activeNeg && (int32_v::indicesSequence() < int32_v(NTracks)));
              
              if(closeDaughters.isEmpty() && (iTC != 0)) continue;
              
              
              int32_v trackPdgPos[2];
              mask32_v active[2];

              active[0] = (posPDG != -1);
              active[0] &= ((isPrimary && (posPVIndex == negPVIndex)) || !(isPrimary));

              active[1] = mask32_v{};
              
              trackPdgPos[0] = posPDG;
#ifdef CBM
              int nPDGPos = 2;
              if( (posPDG == -1).isEmpty() && (posPDG > 1000000000).isEmpty() && (posPDG == 211).isEmpty() )
              {
                nPDGPos = 1;
              }
              else
              {
                trackPdgPos[0](isSecondary && posPDG == -1) = 211;
                trackPdgPos[1] = 2212;
                
                active[0] |= isSecondary && (posPDG == -1);
                active[1]  = isSecondary && ((posPDG == -1) || (posPDG > 1000000000) || (posPDG == 211));
              }
#else
              int nPDGPos = 1;
#endif
              active[0] &= closeDaughters;
              active[1] &= closeDaughters;
              
              if(iTC==0) 
              {
                nPDGPos = 1;
                active[0] = (negInd < negTracksSize) && (int32_v::indicesSequence() < int32_v(NTracks));
              }

              for(int iPDGPos=0; iPDGPos<nPDGPos; iPDGPos++)
              {
                if(active[iPDGPos].isEmpty()) continue;
                
                //detetrmine a pdg code of the mother particle
                
                int32_v motherPDG(-1);
                
                if(!fMixedEventAnalysis)
                {
                  if(iTC==0)
                  {
                    motherPDG = select( (abs(trackPdgPos[iPDGPos]) == 11) && (abs(trackPdgNeg) == 11) && isSecondary, 22, motherPDG); //gamma -> e+ e-
                    //motherPDG( (abs(trackPdgPos[iPDGPos]) == 11) || (abs(trackPdgNeg) == 11) || isSecondary ) = 22; //gamma -> e+ e-
                  }
                  else if(iTC==1)
                  {
                    motherPDG = select( isPrimary   && (abs(trackPdgPos[iPDGPos])== 13 || abs(trackPdgPos[iPDGPos])==19)
                                                    && ((abs(trackPdgNeg) == 13) || (abs(trackPdgNeg) == 19)), 200113, motherPDG); //rho -> mu+ mu-
                  }
                  else if(iTC==2)
                  {
                    motherPDG = select( isSecondary && (abs(trackPdgPos[iPDGPos])==        211) && (abs(trackPdgNeg) ==  211),  310, motherPDG); //K0 -> pi+ pi-
                    motherPDG = select( isSecondary && (abs(trackPdgPos[iPDGPos])==       2212) && (abs(trackPdgNeg) ==  211), 3122, motherPDG); //Lambda -> p+ pi-
                    motherPDG = select( isSecondary && (abs(trackPdgPos[iPDGPos])== 1000010020) && (abs(trackPdgNeg) ==  211), 3003, motherPDG); //LambdaN -> d+ pi-
                    motherPDG = select( isSecondary && (abs(trackPdgPos[iPDGPos])== 1000010030) && (abs(trackPdgNeg) ==  211), 3103, motherPDG); //LambdaNN -> t+ pi-
                    motherPDG = select( isSecondary && (abs(trackPdgPos[iPDGPos])== 1000020030) && (abs(trackPdgNeg) ==  211), 3004, motherPDG); //H3Lambda -> He3+ pi-
                    motherPDG = select( isSecondary && (abs(trackPdgPos[iPDGPos])== 1000020040) && (abs(trackPdgNeg) ==  211), 3005, motherPDG); //H4Lambda -> He4+ pi-
                    motherPDG = select( isSecondary && (abs(trackPdgPos[iPDGPos])== 1000020060) && (abs(trackPdgNeg) ==  211), 3016, motherPDG); //H6Lambda -> He6+ pi-
                    motherPDG = select( isSecondary && (abs(trackPdgPos[iPDGPos])== 1000030060) && (abs(trackPdgNeg) ==  211), 3019, motherPDG); //He6Lambda -> Li6+ pi-
                    motherPDG = select( isSecondary && (abs(trackPdgPos[iPDGPos])== 1000030070) && (abs(trackPdgNeg) ==  211), 3022, motherPDG); //He7Lambda -> Li7+ pi-
                    motherPDG = select( isSecondary && (abs(trackPdgPos[iPDGPos])== 1000040070) && (abs(trackPdgNeg) ==  211), 3025, motherPDG); //Li7Lambda -> Be7+ pi-
                    motherPDG = select( isSecondary && (abs(trackPdgPos[iPDGPos])==        321) && (abs(trackPdgNeg) ==  211), -421, motherPDG); //D0_bar -> pi- K+
                    motherPDG = select( isPrimary   && (abs(trackPdgPos[iPDGPos])==        321) && (abs(trackPdgNeg) ==  211),  313, motherPDG); //K*0 -> K+ pi-
                    motherPDG = select( isPrimary   && (abs(trackPdgPos[iPDGPos])==        211) && (abs(trackPdgNeg) ==  211),  113, motherPDG); //rho -> pi+ pi-
                    motherPDG = select( isPrimary   && (abs(trackPdgPos[iPDGPos])==       2212) && (abs(trackPdgNeg) ==  211), 2114, motherPDG); //Delta0 -> p pi-

                    motherPDG = select( isPrimary   && (abs(trackPdgPos[iPDGPos])== 1000010020) && (abs(trackPdgNeg) ==  211), 100001, motherPDG); //d   pi-
                    motherPDG = select( isPrimary   && (abs(trackPdgPos[iPDGPos])== 1000010030) && (abs(trackPdgNeg) ==  211), 100003, motherPDG); //t   pi-
                    motherPDG = select( isPrimary   && (abs(trackPdgPos[iPDGPos])== 1000020030) && (abs(trackPdgNeg) ==  211), 100005, motherPDG); //He3 pi-
                    motherPDG = select( isPrimary   && (abs(trackPdgPos[iPDGPos])== 1000020040) && (abs(trackPdgNeg) ==  211), 100007, motherPDG); //He4 pi-
                    motherPDG = select( isPrimary   && (abs(trackPdgPos[iPDGPos])== 1000020060) && (abs(trackPdgNeg) ==  211), 100009, motherPDG); //He6 pi-
                    motherPDG = select( isPrimary   && (abs(trackPdgPos[iPDGPos])== 1000030060) && (abs(trackPdgNeg) ==  211), 100011, motherPDG); //Li6 pi-
                    motherPDG = select( isPrimary   && (abs(trackPdgPos[iPDGPos])== 1000030070) && (abs(trackPdgNeg) ==  211), 100013, motherPDG); //Li7 pi-
                    motherPDG = select( isPrimary   && (abs(trackPdgPos[iPDGPos])== 1000040070) && (abs(trackPdgNeg) ==  211), 100015, motherPDG); //Be7 pi-
                  }
                  else if(iTC==3)
                  {
                    motherPDG = select( isSecondary && (abs(trackPdgPos[iPDGPos])==  211) && (abs(trackPdgNeg) ==  321),  421, motherPDG); //D0 -> pi+ K-
                    motherPDG = select( isSecondary && (abs(trackPdgPos[iPDGPos])==  321) && (abs(trackPdgNeg) ==  321),  426, motherPDG); //D0 -> K+ K-
                    motherPDG = select( isPrimary   && (abs(trackPdgPos[iPDGPos])==  211) && (abs(trackPdgNeg) ==  321), -313, motherPDG); //K*0_bar -> K- pi+
                    motherPDG = select( isPrimary   && (abs(trackPdgPos[iPDGPos])== 2212) && (abs(trackPdgNeg) ==  321), 3124, motherPDG); //Lambda* -> p K-
                    motherPDG = select( isPrimary   && (abs(trackPdgPos[iPDGPos])==  321) && (abs(trackPdgNeg) ==  321),  333, motherPDG); //phi -> K+ K-

                    motherPDG = select( isPrimary   && (abs(trackPdgPos[iPDGPos])== 1000010020) && (abs(trackPdgNeg) ==  321), 110001, motherPDG); //d   K-
                    motherPDG = select( isPrimary   && (abs(trackPdgPos[iPDGPos])== 1000010030) && (abs(trackPdgNeg) ==  321), 110003, motherPDG); //t   K-
                    motherPDG = select( isPrimary   && (abs(trackPdgPos[iPDGPos])== 1000020030) && (abs(trackPdgNeg) ==  321), 110005, motherPDG); //He3 K-
                    motherPDG = select( isPrimary   && (abs(trackPdgPos[iPDGPos])== 1000020040) && (abs(trackPdgNeg) ==  321), 110007, motherPDG); //He4 K-
                    motherPDG = select( isPrimary   && (abs(trackPdgPos[iPDGPos])== 1000020060) && (abs(trackPdgNeg) ==  321), 110009, motherPDG); //He6 K-
                    motherPDG = select( isPrimary   && (abs(trackPdgPos[iPDGPos])== 1000030060) && (abs(trackPdgNeg) ==  321), 110011, motherPDG); //Li6 K-
                    motherPDG = select( isPrimary   && (abs(trackPdgPos[iPDGPos])== 1000030070) && (abs(trackPdgNeg) ==  321), 110013, motherPDG); //Li7 K-
                    motherPDG = select( isPrimary   && (abs(trackPdgPos[iPDGPos])== 1000040070) && (abs(trackPdgNeg) ==  321), 110015, motherPDG); //Be7 K-
                  }
                  else if(iTC==4)
                  {
                    motherPDG = select( isSecondary && (abs(trackPdgPos[iPDGPos])==  211) && (abs(trackPdgNeg) ==       2212),  -3122, motherPDG); //Lambda_bar -> p- pi+
                    motherPDG = select( isSecondary && (abs(trackPdgPos[iPDGPos])==  211) && (abs(trackPdgNeg) == 1000010020),  -3003, motherPDG); //LambdaN_bar -> d- pi+
                    motherPDG = select( isSecondary && (abs(trackPdgPos[iPDGPos])==  211) && (abs(trackPdgNeg) == 1000010030),  -3103, motherPDG); //LambdaNN_bar -> t- pi+
                    motherPDG = select( isSecondary && (abs(trackPdgPos[iPDGPos])==  211) && (abs(trackPdgNeg) == 1000020030),  -3004, motherPDG); //H3Lambda_bar -> He3- pi+
                    motherPDG = select( isSecondary && (abs(trackPdgPos[iPDGPos])==  211) && (abs(trackPdgNeg) == 1000020040),  -3005, motherPDG); //H4Lambda_bar -> He4- pi+
                    motherPDG = select( isSecondary && (abs(trackPdgPos[iPDGPos])==  211) && (abs(trackPdgNeg) == 1000020060),  -3016, motherPDG); //H6Lambda_bar -> He6- pi+
                    motherPDG = select( isSecondary && (abs(trackPdgPos[iPDGPos])==  211) && (abs(trackPdgNeg) == 1000030060),  -3019, motherPDG); //He6Lambda_bar -> Li6- pi+
                    motherPDG = select( isSecondary && (abs(trackPdgPos[iPDGPos])==  211) && (abs(trackPdgNeg) == 1000030070),  -3022, motherPDG); //Li6Lambda_bar -> Li7- pi+
                    motherPDG = select( isSecondary && (abs(trackPdgPos[iPDGPos])==  211) && (abs(trackPdgNeg) == 1000040070),  -3025, motherPDG); //Li7Lambda_bar -> Be7- pi+
                    motherPDG = select( isPrimary   && (abs(trackPdgPos[iPDGPos])==  321) && (abs(trackPdgNeg) ==       2212),  -3124, motherPDG); //Lambda*_bar -> p- K+
                    motherPDG = select( isPrimary   && (abs(trackPdgPos[iPDGPos])== 2212) && (abs(trackPdgNeg) ==       2212), 200443, motherPDG); //JPsi -> p- p
                    motherPDG = select( isPrimary   && (abs(trackPdgPos[iPDGPos])==  211) && (abs(trackPdgNeg) ==       2212),  -2114, motherPDG); //Delta0_bar -> p- pi+
                  }
                }
                else
                {
                  if(iTC==0)
                    motherPDG = select( (abs(trackPdgPos[iPDGPos])==   11) && (abs(trackPdgNeg) ==   11), 22, motherPDG); //gamma -> e+ e-
                  else if(iTC==1)
                    motherPDG = select( isPrimary   && (abs(trackPdgPos[iPDGPos])== 13 || abs(trackPdgPos[iPDGPos])==19)
                                                    && ((abs(trackPdgNeg) == 13) || (abs(trackPdgNeg) == 19)), 200113, motherPDG); //rho -> mu+ mu-
                  else if(iTC==2)
                    motherPDG = select( isSecondary && (abs(trackPdgPos[iPDGPos]) == 321) && (abs(trackPdgNeg) ==  211), -421, motherPDG); //D0_bar -> pi- K+
                  else if(iTC==3)
                    motherPDG = select( isSecondary && (abs(trackPdgPos[iPDGPos])==  211) && (abs(trackPdgNeg) ==  321), 421, motherPDG); //D0 -> pi+ K-
                }
                
                if( (iTrTypeNeg == 0) && (iTrTypePos == 0) )
                {
                  float32_v chiprimCut = fCuts2D[0];
                  chiprimCut = select( abs(motherPDG) == 421 || abs(motherPDG) == 426, fCutCharmChiPrim, chiprimCut);
                  active[iPDGPos] &= (chiPrimNeg > chiprimCut && chiPrimPos > chiprimCut);
                }
                
                active[iPDGPos] &= (motherPDG != -1);
                if(!(fDecayReconstructionList.empty()))
                {
                  alignas(SimdSize) std::int32_t motherPdgArray[SimdLen];
                  motherPDG.store(motherPdgArray);
                  for(int iV=0; iV<SimdLen; iV++)
                  {
                    if(!(active[iPDGPos][iV])) continue;
                    if(fDecayReconstructionList.find(motherPDG[iV]) == fDecayReconstructionList.end())
                      motherPdgArray[iV] = -1;
                  }
                  motherPDG.load(motherPdgArray);
                  active[iPDGPos] &= (motherPDG != -1);
                }
                if(active[iPDGPos].isEmpty()) continue;

                if(!( (iTrTypePos == 1) && (iTrTypeNeg == 1) ) )
                {
                  float32_v dS[2];
                  daughterNeg.GetDStoParticleFast( daughterPos, dS );   
                  float32_v negParameters[8], posParameters[8];
                  daughterNeg.TransportFast( dS[0], negParameters ); 
                  daughterPos.TransportFast( dS[1], posParameters ); 
                  float32_v dx = negParameters[0]-posParameters[0]; 
                  float32_v dy = negParameters[1]-posParameters[1]; 
                  float32_v dz = negParameters[2]-posParameters[2];
                  float32_v dr = sqrt(dx*dx+dy*dy+dz*dz);

                  active[iPDGPos] &= (dr < float32_v(fDistanceCut));
                  if(active[iPDGPos].isEmpty()) continue;
                  
                  float32_v p1p2 = posParameters[3]*negParameters[3] + posParameters[4]*negParameters[4] + posParameters[5]*negParameters[5];
                  float32_v p12  = posParameters[3]*posParameters[3] + posParameters[4]*posParameters[4] + posParameters[5]*posParameters[5];
                  float32_v p22  = negParameters[3]*negParameters[3] + negParameters[4]*negParameters[4] + negParameters[5]*negParameters[5];
                  active[iPDGPos] &= (p1p2 > -p12);
                  active[iPDGPos] &= (p1p2 > -p22);
                }
                
                const float32_v& ptNeg2 = daughterNeg.Px()*daughterNeg.Px() + daughterNeg.Py()*daughterNeg.Py();
                const float32_v& ptPos2 = daughterPos.Px()*daughterPos.Px() + daughterPos.Py()*daughterPos.Py();
                if( !((abs(motherPDG) == 421 || abs(motherPDG) == 426).isEmpty()) )
                {
                  active[iPDGPos] &= ( (abs(motherPDG) == 421 || abs(motherPDG) == 426) && 
                                      (ptNeg2 >= fCutCharmPt*fCutCharmPt) && 
                                      (ptPos2 >= fCutCharmPt*fCutCharmPt) &&
                                      (chiPrimNeg > fCutCharmChiPrim) && (chiPrimPos > fCutCharmChiPrim) &&
                                      (negNPixelHits >= int32_v(3)) && (posNPixelHits >= int32_v(3)) )
                                    || (!(abs(motherPDG) == 421 || abs(motherPDG) == 426));
                }
                
                if(active[iPDGPos].isEmpty()) continue;

                for(int iV=0; iV<SimdLen; iV++)
                {
                  if(!(active[iPDGPos][iV])) continue;
                  

                  idPosDaughtersArray[nBufEntry] = iTrP+iV;
                  idNegDaughtersArray[nBufEntry] = negInd[iV];
                  
                  daughterPosPDGArray[nBufEntry] = trackPdgPos[iPDGPos][iV];
                  daughterNegPDGArray[nBufEntry] = trackPdgNeg[iV];
                  
                  if(motherPDG[iV] == 22)
                  {
                    daughterPosPDGArray[nBufEntry] = -11;
                    daughterNegPDGArray[nBufEntry] =  11;
                  }
                  
                  pvIndexMotherArray[nBufEntry] = isPrimary[iV] ? negPVIndex[iV] : -1;
                  
                  if( iTrTypeNeg != iTrTypePos ) pvIndexMotherArray[nBufEntry] = 0;
                  
                  V0PDGArray[nBufEntry] = motherPDG[iV];
                  
                  nBufEntry++;

                  if(int(nBufEntry) == SimdLen)
                  {
                    idNegDaughters.load(idNegDaughtersArray);
                    idPosDaughters.load(idPosDaughtersArray);
                    daughterNegPDG.load(daughterNegPDGArray);
                    daughterPosPDG.load(daughterPosPDGArray);
                    pvIndexMother.load(pvIndexMotherArray);
                    V0PDG.load(V0PDGArray);

                    KFParticleDatabase::Instance()->GetMotherMass(V0PDG,massMotherPDG,massMotherPDGSigma);
                    mother.SetPDG( V0PDG );
                    ConstructV0(vTracks, trTypeIndexPos[iTrTypePos], trTypeIndexNeg[iTrTypeNeg],                
                                idPosDaughters, idNegDaughters, daughterPosPDG, daughterNegPDG,
                                mother, mother_temp,
                                nBufEntry, l, dl, Particles, PrimVtx,
                                cuts, pvIndexMother, secCuts, massMotherPDG,
                                massMotherPDGSigma, motherPrimSecCand, nPrimSecCand, vMotherPrim, vMotherSec);
                    nBufEntry = 0; 
                  }
                  
                  //TODO optimize this part of code for D-mesons
                  if(motherPDG[iV] == 310 && 
                     (fDecayReconstructionList.empty() ||
                      (!(fDecayReconstructionList.empty()) && !(fDecayReconstructionList.find(420) == fDecayReconstructionList.end()) ) ) &&
                     negNPixelHits[iV] >= 3 && posNPixelHits[iV] >= 3 &&
                     chiPrimNeg[iV] > fCutCharmChiPrim && chiPrimPos[iV] > fCutCharmChiPrim &&
                     ptNeg2[iV] >= fCutCharmPt*fCutCharmPt && ptPos2[iV] >= fCutCharmPt*fCutCharmPt )
                  {
                    idPosDaughtersArray[nBufEntry] = iTrP+iV;
                    idNegDaughtersArray[nBufEntry] = negInd[iV];
                    
                    daughterPosPDGArray[nBufEntry] = trackPdgPos[iPDGPos][iV];
                    daughterNegPDGArray[nBufEntry] = trackPdgNeg[iV];
                    
                    pvIndexMotherArray[nBufEntry] = isPrimary[iV] ? negPVIndex[iV] : -1;
                    
                    V0PDGArray[nBufEntry] = 420;
                    
                    nBufEntry++;

                    if(int(nBufEntry) == SimdLen)
                    {
                      idNegDaughters.load(idNegDaughtersArray);
                      idPosDaughters.load(idPosDaughtersArray);
                      daughterNegPDG.load(daughterNegPDGArray);
                      daughterPosPDG.load(daughterPosPDGArray);
                      pvIndexMother.load(pvIndexMotherArray);
                      V0PDG.load(V0PDGArray);

                      KFParticleDatabase::Instance()->GetMotherMass(V0PDG,massMotherPDG,massMotherPDGSigma);
                      mother.SetPDG( V0PDG );
                      ConstructV0(vTracks, trTypeIndexPos[iTrTypePos], trTypeIndexNeg[iTrTypeNeg],                
                                  idPosDaughters, idNegDaughters, daughterPosPDG, daughterNegPDG,
                                  mother, mother_temp,
                                  nBufEntry, l, dl, Particles, PrimVtx,
                                  cuts, pvIndexMother, secCuts, massMotherPDG,
                                  massMotherPDGSigma, motherPrimSecCand, nPrimSecCand, vMotherPrim, vMotherSec);
                      nBufEntry = 0; 
                    }
                  }
                  
                }//iV
              }//iPDGPos
            }//iRot
          }//iTrP
        }//iTrN
        
        if( nBufEntry>0 )
        {
          for(int iV=nBufEntry; iV<SimdLen; iV++)
          {
            idPosDaughtersArray[iV] = idPosDaughtersArray[0];
            idNegDaughtersArray[iV] = idNegDaughtersArray[0];
          }
          idNegDaughters.load(idNegDaughtersArray);
          idPosDaughters.load(idPosDaughtersArray);
          daughterNegPDG.load(daughterNegPDGArray);
          daughterPosPDG.load(daughterPosPDGArray);
          pvIndexMother.load(pvIndexMotherArray);
          V0PDG.load(V0PDGArray);

          KFParticleDatabase::Instance()->GetMotherMass(V0PDG,massMotherPDG,massMotherPDGSigma);
          mother.SetPDG( V0PDG );
          idNegDaughters.load(idNegDaughtersArray);
          idPosDaughters.load(idPosDaughtersArray);
          daughterNegPDG.load(daughterNegPDGArray);
          daughterPosPDG.load(daughterPosPDGArray);
          pvIndexMother.load(pvIndexMotherArray);
          ConstructV0(vTracks, trTypeIndexPos[iTrTypePos], trTypeIndexNeg[iTrTypeNeg],              
                      idPosDaughters, idNegDaughters, daughterPosPDG, daughterNegPDG,
                      mother, mother_temp,
                      nBufEntry, l, dl, Particles, PrimVtx,
                      cuts, pvIndexMother, secCuts, massMotherPDG,
                      massMotherPDGSigma, motherPrimSecCand, nPrimSecCand, vMotherPrim, vMotherSec);
          nBufEntry = 0; 
        }
        
        if(nPrimSecCand>0)
        {
          SaveV0PrimSecCand(motherPrimSecCand,nPrimSecCand,mother_temp,PrimVtx,secCuts,vMotherPrim,vMotherSec);
          nPrimSecCand = 0;
        }
      }//iTC
    }//iTrTypeNeg
  }//iTrTypePos
}

void KFParticleFinder::Find2DaughterDecayOneSign(KFPTrackVector& vTracks,
                                                 std::vector<KFParticle>& Particles,
                                                 std::vector<KFParticleSIMD, KFPSimdAllocator<KFParticleSIMD> >& PrimVtx)
{
  KFParticle mother_temp;
  kfvector_floatv l(fNPV), dl(fNPV);

  KFParticleSIMD pion, fragment;

  for(int iPion = vTracks.FirstPion(); iPion < vTracks.LastPion(); iPion += SimdLen)
  {
    int32_v pionId    = reinterpret_cast<const int32_v&>(vTracks.Id()[iPion]);
    int32_v pionPDG   = reinterpret_cast<const int32_v&>(vTracks.PDG()[iPion]);
    pion.Load(vTracks, iPion, pionPDG);
    pion.SetId(pionId);

    for(int iFragment = vTracks.FirstDeuteron(); iFragment < vTracks.LastBe7(); iFragment += SimdLen)
    {
      const int32_v fragmentId    = reinterpret_cast<const int32_v&>(vTracks.Id()[iFragment]);
      const int32_v fragmentPDG   = reinterpret_cast<const int32_v&>(vTracks.PDG()[iFragment]);

      fragment.Load(vTracks, iFragment, fragmentPDG);
      fragment.SetId(fragmentId);

      for(int iRot = 0; iRot<SimdLen; iRot++)
      {
        pionPDG = pionPDG.rotate<1>();
        pion.Rotate();
        pion.SetPDG(pionPDG);

        const mask32_v isPion = (pionPDG == 211);

        int32_v motherPDG(-1);
        motherPDG = select( (fragmentPDG == 1000010020) && isPion, 3030, motherPDG); //H2Sp  -> d+   pi+
        motherPDG = select( (fragmentPDG == 1000010030) && isPion, 3031, motherPDG); //H3Sp  -> t+   pi+
        motherPDG = select( (fragmentPDG == 1000020030) && isPion, 3032, motherPDG); //He3Sp -> He3+ pi+
        motherPDG = select( (fragmentPDG == 1000020040) && isPion, 3033, motherPDG); //He4Sp -> He4+ pi+
        motherPDG = select( (fragmentPDG == 1000020060) && isPion, 3034, motherPDG); //He6Sp -> He6+ pi+
        motherPDG = select( (fragmentPDG == 1000030060) && isPion, 3035, motherPDG); //Li6Sp -> Li6+ pi+
        motherPDG = select( (fragmentPDG == 1000030070) && isPion, 3036, motherPDG); //Li7Sp -> Li7+ pi+
        motherPDG = select( (fragmentPDG == 1000040070) && isPion, 3037, motherPDG); //Be7Sp -> Be7+ pi+

        mask32_v saveParticle = (motherPDG != -1);

        KFParticleSIMD mother;
        mother += pion;
        mother += fragment;
        mother.SetPDG(motherPDG);

        saveParticle &= (mother.Chi2()/toFloat(mother.NDF()) < fCuts2D[1] );
        saveParticle &= isFinite(mother.GetChi2());
        saveParticle &= (mother.GetChi2() > 0.0f);
        saveParticle &= (mother.GetChi2() == mother.GetChi2());

        if( saveParticle.isEmpty() ) continue;

        float32_v ldlMin(1.e8f);
        mask32_v isParticleFromVertex;

        for(int iP=0; iP<fNPV; iP++)
        {
          mask32_v isParticleFromVertexLocal;
          mother.GetDistanceToVertexLine(PrimVtx[iP], l[iP], dl[iP], &isParticleFromVertexLocal);
          isParticleFromVertex |= isParticleFromVertexLocal;
          float32_v ldl = (l[iP]/dl[iP]);
          ldlMin = select( (ldl < ldlMin) && saveParticle, ldl, ldlMin);
        }
        saveParticle &= ldlMin > fCuts2D[2];
        saveParticle &= isParticleFromVertex;
        if( saveParticle.isEmpty() ) continue;

        mask32_v isPrimaryParticle;
        for(int iP=0; iP<fNPV; iP++)
        {
          KFParticleSIMD mother_topo = mother;
          mother_topo.SetProductionVertex(PrimVtx[iP]);
          mask32_v isPrimary = (mother_topo.Chi2()/toFloat(mother_topo.NDF()) < fCuts2D[1] );
          isPrimary &= isFinite(mother_topo.GetChi2());
          isPrimary &= (mother_topo.GetChi2() > 0.0f);
          isPrimary &= (mother_topo.GetChi2() == mother_topo.GetChi2());
          isPrimaryParticle |= isPrimary;
        }
        saveParticle &= isPrimaryParticle;
        if( saveParticle.isEmpty() ) continue;

        for(int iv=0; iv<SimdLen; iv++)
        {
          if(!saveParticle[iv]) continue;
          mother.GetKFParticle(mother_temp, iv);
          mother_temp.SetId(Particles.size());
          Particles.push_back(mother_temp);
        }
      }
    }
  }
}

void KFParticleFinder::ConstructResonance(const std::vector<KFParticle>& particles1,
                                          const std::vector<KFParticle>& particles2,
                                          const int motherPdg,
                                          std::vector<KFParticle>& output)
{
  if(!(fDecayReconstructionList.empty()))
  {
    if(fDecayReconstructionList.find(motherPdg) == fDecayReconstructionList.end()) return;
  }

  if(particles1.size() == 0U || particles2.size() == 0U) return;

  const int pdg1 = particles1[0].GetPDG();
  const int pdg2 = particles2[0].GetPDG();

  const float M1  = KFParticleDatabase::Instance()->GetMass(pdg1);
  const float M2  = KFParticleDatabase::Instance()->GetMass(pdg2);

  for(uint32_t iParticle1 = 0; iParticle1<particles1.size(); iParticle1++)
  {
    const KFParticle& particle1 = particles1[iParticle1];
    const float px1 = particle1.Px();
    const float py1 = particle1.Py();
    const float pz1 = particle1.Pz();
    const float E1  = sqrt(px1*px1 + py1*py1 + pz1*pz1 + M1*M1);

    const uint32_t startIndex = (pdg1 == pdg2) ? iParticle1 + 1 : 0;

    for(uint32_t iParticle2 = startIndex; iParticle2<particles2.size(); iParticle2++)
    {
      const KFParticle& particle2 = particles2[iParticle2];
      const float px2 = particle2.Px();
      const float py2 = particle2.Py();
      const float pz2 = particle2.Pz();
      const float E2  = sqrt(px2*px2 + py2*py2 + pz2*pz2 + M2*M2);

      KFParticle resonance;
      resonance.AddDaughterId(particle1.Id());
      resonance.AddDaughterId(particle2.Id());
      resonance.SetPDG(motherPdg);
      resonance.SetId(output.size());

      resonance.Px() = px1 + px2;
      resonance.Py() = py1 + py2;
      resonance.Pz() = pz1 + pz2;
      resonance.E()  = E1 + E2;

      resonance.Covariance( 9) = particle1.GetCovariance( 9) + particle2.GetCovariance( 9);
      resonance.Covariance(13) = particle1.GetCovariance(13) + particle2.GetCovariance(13);
      resonance.Covariance(14) = particle1.GetCovariance(14) + particle2.GetCovariance(14);
      resonance.Covariance(18) = particle1.GetCovariance(18) + particle2.GetCovariance(18);
      resonance.Covariance(19) = particle1.GetCovariance(19) + particle2.GetCovariance(19);
      resonance.Covariance(20) = particle1.GetCovariance(20) + particle2.GetCovariance(20);
      resonance.Covariance(24) = particle1.GetCovariance(24) + particle2.GetCovariance(24);
      resonance.Covariance(25) = particle1.GetCovariance(25) + particle2.GetCovariance(25);
      resonance.Covariance(26) = particle1.GetCovariance(26) + particle2.GetCovariance(26);
      resonance.Covariance(27) = particle1.GetCovariance(27) + particle2.GetCovariance(27);

      output.push_back(resonance);
    }
  }
}

void KFParticleFinder::ConstructResonances2D(KFPTrackVector* vTracks,
                                             std::vector<KFParticle>& Particles,
                                             std::vector<KFParticleSIMD, KFPSimdAllocator<KFParticleSIMD> >& PrimVtx)
{
  std::vector<KFParticle> piPlus;
  std::vector<KFParticle> kPlus;
  std::vector<KFParticle> pPlus;
  std::vector<KFParticle> dPlus;
  std::vector<KFParticle> tPlus;
  std::vector<KFParticle> he3Plus;
  std::vector<KFParticle> he4Plus;
  std::vector<KFParticle> he6Plus;
  std::vector<KFParticle> li6Plus;
  std::vector<KFParticle> li7Plus;
  std::vector<KFParticle> be7Plus;

  std::vector<KFParticle> piMinus;
  std::vector<KFParticle> kMinus;
  std::vector<KFParticle> pMinus;

  const KFParticleSIMD pv = PrimVtx[0];
  KFParticleSIMD trackSIMD;
  KFParticle track;

  for(int iTrack=vTracks[2].FirstPion(); iTrack < vTracks[2].LastBe7(); iTrack+=SimdLen)
  {
    const int32_v& pdg = reinterpret_cast<const int32_v&>(vTracks[2].PDG()[iTrack]);
    const int32_v& id  = reinterpret_cast<const int32_v&>(vTracks[2].Id()[iTrack]);

    trackSIMD.Load(vTracks[2], iTrack, pdg);
    trackSIMD.SetPDG(pdg);
    trackSIMD.SetId(id);
    trackSIMD.SetProductionVertex(pv);

    for(int i=0; i<SimdLen; i++)
    {
      if(iTrack + i >= vTracks[2].LastBe7()) continue;

      trackSIMD.GetKFParticle(track, i);

      // if(track.Chi2() > 3.f * track.NDF()) continue;

      if(track.GetPDG() ==        211) piPlus.push_back(track);
      if(track.GetPDG() ==        321) kPlus.push_back(track);
      if(track.GetPDG() ==       2212) pPlus.push_back(track);
      if(track.GetPDG() == 1000010020) dPlus.push_back(track);
      if(track.GetPDG() == 1000010030) tPlus.push_back(track);
      if(track.GetPDG() == 1000020030) he3Plus.push_back(track);
      if(track.GetPDG() == 1000020040) he4Plus.push_back(track);
      if(track.GetPDG() == 1000020060) he6Plus.push_back(track);
      if(track.GetPDG() == 1000030060) li6Plus.push_back(track);
      if(track.GetPDG() == 1000030070) li7Plus.push_back(track);
      if(track.GetPDG() == 1000040070) be7Plus.push_back(track);
    }
  }

  for(int iTrack=vTracks[3].FirstPion(); iTrack < vTracks[3].LastProton(); iTrack+=SimdLen)
  {
    const int32_v& pdg = reinterpret_cast<const int32_v&>(vTracks[3].PDG()[iTrack]);
    const int32_v& id  = reinterpret_cast<const int32_v&>(vTracks[3].Id()[iTrack]);

    trackSIMD.Load(vTracks[3], iTrack, pdg);
    trackSIMD.SetPDG(pdg);
    trackSIMD.SetId(id);
    trackSIMD.SetProductionVertex(pv);

    for(int i=0; i<SimdLen; i++)
    {
      if(iTrack + i >= vTracks[3].LastProton()) continue;

      trackSIMD.GetKFParticle(track, i);

      // if(track.Chi2() > 3.f * track.NDF()) continue;

      if(track.GetPDG() ==  -211) piMinus.push_back(track);
      if(track.GetPDG() ==  -321) kMinus.push_back(track);
      if(track.GetPDG() == -2212) pMinus.push_back(track);
    }
  }

  
  ConstructResonance(piPlus, piMinus, 113, Particles); // rho0 -> pi+ pi-
  ConstructResonance(kPlus,  piMinus, 313, Particles); // K*0 -> K+ pi-
  ConstructResonance(kMinus, piPlus, -313, Particles); // K*0b -> K- pi+
  ConstructResonance(kPlus,  kMinus,  333, Particles); // phi -> K+ K-

  ConstructResonance(pPlus,  piPlus,   2224, Particles); // Delta++
  ConstructResonance(pMinus, piMinus, -2224, Particles); // Delta--
  ConstructResonance(pPlus,  piMinus,  2114, Particles); // Delta0
  ConstructResonance(pMinus, piPlus,  -2114, Particles); // Delta0 bar

  ConstructResonance(dPlus,   piMinus, 100001, Particles); //dpi-
  ConstructResonance(dPlus,   piPlus,  100002, Particles); //dpi+
  ConstructResonance(tPlus,   piMinus, 100003, Particles); //tpi-
  ConstructResonance(tPlus,   piPlus,  100004, Particles); //tpi+
  ConstructResonance(he3Plus, piMinus, 100005, Particles); //He3pi-
  ConstructResonance(he3Plus, piPlus,  100006, Particles); //He3pi+
  ConstructResonance(he4Plus, piMinus, 100007, Particles); //He4pi-
  ConstructResonance(he4Plus, piPlus,  100008, Particles); //He4pi+
  ConstructResonance(he6Plus, piMinus, 100009, Particles); //He6pi-
  ConstructResonance(he6Plus, piPlus,  100010, Particles); //He6pi+
  ConstructResonance(li6Plus, piMinus, 100011, Particles); //Li6pi-
  ConstructResonance(li6Plus, piPlus,  100012, Particles); //Li6pi+
  ConstructResonance(li7Plus, piMinus, 100013, Particles); //Li7pi-
  ConstructResonance(li7Plus, piPlus,  100014, Particles); //Li7pi+
  ConstructResonance(be7Plus, piMinus, 100015, Particles); //Be7pi-
  ConstructResonance(be7Plus, piPlus,  100016, Particles); //Be7pi+

  ConstructResonance(dPlus,   kMinus, 110001, Particles); //dK-
  ConstructResonance(dPlus,   kPlus,  110002, Particles); //dK+
  ConstructResonance(tPlus,   kMinus, 110003, Particles); //tK-
  ConstructResonance(tPlus,   kPlus,  110004, Particles); //tK+
  ConstructResonance(he3Plus, kMinus, 110005, Particles); //He3K-
  ConstructResonance(he3Plus, kPlus,  110006, Particles); //He3K+
  ConstructResonance(he4Plus, kMinus, 110007, Particles); //He4K-
  ConstructResonance(he4Plus, kPlus,  110008, Particles); //He4K+
  ConstructResonance(he6Plus, kMinus, 110009, Particles); //He6K-
  ConstructResonance(he6Plus, kPlus,  110010, Particles); //He6K+
  ConstructResonance(li6Plus, kMinus, 110011, Particles); //Li6K-
  ConstructResonance(li6Plus, kPlus,  110012, Particles); //Li6K+
  ConstructResonance(li7Plus, kMinus, 110013, Particles); //Li7K-
  ConstructResonance(li7Plus, kPlus,  110014, Particles); //Li7K+
  ConstructResonance(be7Plus, kMinus, 110015, Particles); //Be7K-
  ConstructResonance(be7Plus, kPlus,  110016, Particles); //Be7K+

  ConstructResonance(pPlus,   pPlus,  200001, Particles); //pp
  ConstructResonance(dPlus,   pPlus,  200002, Particles); //dp
  ConstructResonance(tPlus,   pPlus,  200003, Particles); //tp
  ConstructResonance(he3Plus, pPlus,  200004, Particles); //He3p
  ConstructResonance(he4Plus, pPlus,  200005, Particles); //He4p
  ConstructResonance(he6Plus, pPlus,  200006, Particles); //He6p
  ConstructResonance(li6Plus, pPlus,  200007, Particles); //Li6p
  ConstructResonance(li7Plus, pPlus,  200008, Particles); //Li7p
  ConstructResonance(be7Plus, pPlus,  200009, Particles); //Be7p
}

void KFParticleFinder::ConstructPrimaryBG(KFPTrackVector* vTracks,
                                          vector<KFParticle>& Particles,
                                          std::vector<KFParticleSIMD, KFPSimdAllocator<KFParticleSIMD> >& PrimVtx,
                                          const float* cuts,
                                          const float* secCuts,
                                          vector< vector<KFParticle> >* vMotherPrim,
                                          vector<KFParticle>* vMotherSec )
{
  /** Constructs same-sign background candidates for 2-daughter resonances.
   ** \param[in] vRTracks - pointer to the array with vectors of tracks:\n
   ** 0) secondary positive at the first hit position; \n
   ** 1) secondary negative at the first hit position; \n
   ** 2) primary positive at the first hit position; \n
   ** 3) primary negative at the first hit position; \n
   ** 4) secondary positive at the last hit position; \n
   ** 5) secondary negative at the last hit position; \n
   ** 6) primary positive at the last hit position; \n
   ** 7) primary negative at the last hit position. \n
   ** \param[out] Particles - the output array with the reconstructed particle-candidates.
   ** \param[in] PrimVtx - vector with primary vertices.
   ** \param[in] cuts - set of cuts: \f$\chi^2_{prim}\f$, \f$\chi^2_{geo}\f$, \f$l/\Delta l\f$.
   ** \param[in] secCuts - cuts to select primary and secondary candidates from the reconstructed set: \f$\sigma_{M}\f$, \f$\chi^2_{topo}\f$, \f$l/\Delta l\f$.
   ** \param[out] vMotherPrim - array with output primary candidates. Is provided for consistency with KFParticleFinder::ConstructV0().
   ** \param[out] vMotherSec - array with output secondary candidates. Is provided for consistency with KFParticleFinder::ConstructV0().
   **/
  KFParticle mother_temp;
  KFParticleSIMD mother;
  kfvector_floatv l(fNPV), dl(fNPV);

  KFParticleSIMD daughterNeg, daughterPos;

  // for secondary V0
  unsigned int nBufEntry = 0;
  float32_v dS;

  alignas(SimdSize) std::int32_t idNegDaughtersArray[SimdLen];
  alignas(SimdSize) std::int32_t idPosDaughtersArray[SimdLen];
  alignas(SimdSize) std::int32_t daughterPosPDGArray[SimdLen];
  alignas(SimdSize) std::int32_t daughterNegPDGArray[SimdLen];
  alignas(SimdSize) std::int32_t pvIndexMotherArray[SimdLen];
  alignas(SimdSize) std::int32_t V0PDGArray[SimdLen];

  int32_v idNegDaughters;
  int32_v idPosDaughters;
  int32_v daughterPosPDG(-1);
  int32_v daughterNegPDG(-1);
    
  int32_v pvIndexMother(-1);
  
  float32_v massMotherPDG(0.f), massMotherPDGSigma(0.f);
  int32_v V0PDG(0);

  KFParticleSIMD motherPrimSecCand;
  int nPrimSecCand =0;
  
  for(int iSet=2; iSet<4; iSet++)
  {
    int signPDG = 1;
    if(iSet == 3)
      signPDG = -1;
    
    KFPTrackVector& primaryTracks = vTracks[iSet];
    int nPositiveTracks = primaryTracks.LastBe7();

    for(int iTr1 = primaryTracks.FirstPion(); iTr1 < nPositiveTracks; iTr1 ++)
    {
      int pdg1 = primaryTracks.PDG()[iTr1];
      int pvIndex = primaryTracks.PVIndex()[iTr1];

      for(int iTr2 = iTr1+1; iTr2 < nPositiveTracks; iTr2 ++)
      {
        int pdg2 = primaryTracks.PDG()[iTr2];
        if(pvIndex != primaryTracks.PVIndex()[iTr2]) continue;
        
        int motherPDG = -1;
        
        motherPDG = (abs(pdg1) == 211) && (abs(pdg2) ==  211) ? signPDG*9001 : motherPDG; //pi+pi+
        motherPDG = (abs(pdg1) == 211) && (abs(pdg2) ==  321) ? signPDG*9002 : motherPDG; //pi+K+
        motherPDG = (abs(pdg1) == 211) && (abs(pdg2) == 2212) ? signPDG*2224 : motherPDG; //pi+p
        motherPDG = (abs(pdg1) == 321) && (abs(pdg2) ==  321) ? signPDG*9003 : motherPDG; //K+K+
        motherPDG = (abs(pdg1) == 321) && (abs(pdg2) == 2212) ? signPDG*9004 : motherPDG; //K+p

        if(signPDG)
        {
          motherPDG = ( pdg1 == 211) && (pdg2 == 1000010020) ? 100002 : motherPDG; //d+   pi+
          motherPDG = ( pdg1 == 211) && (pdg2 == 1000010030) ? 100004 : motherPDG; //t+   pi+
          motherPDG = ( pdg1 == 211) && (pdg2 == 1000020030) ? 100006 : motherPDG; //He3+ pi+
          motherPDG = ( pdg1 == 211) && (pdg2 == 1000020040) ? 100008 : motherPDG; //He4+ pi+
          motherPDG = ( pdg1 == 211) && (pdg2 == 1000020060) ? 100010 : motherPDG; //He6+ pi+
          motherPDG = ( pdg1 == 211) && (pdg2 == 1000030060) ? 100012 : motherPDG; //Li6+ pi+
          motherPDG = ( pdg1 == 211) && (pdg2 == 1000030070) ? 100014 : motherPDG; //Li7+ pi+
          motherPDG = ( pdg1 == 211) && (pdg2 == 1000040070) ? 100016 : motherPDG; //Be7+ pi+

          motherPDG = ( pdg1 == 321) && (pdg2 == 1000010020) ? 110002 : motherPDG; //d+   K+
          motherPDG = ( pdg1 == 321) && (pdg2 == 1000010030) ? 110004 : motherPDG; //t+   K+
          motherPDG = ( pdg1 == 321) && (pdg2 == 1000020030) ? 110006 : motherPDG; //He3+ K+
          motherPDG = ( pdg1 == 321) && (pdg2 == 1000020040) ? 110008 : motherPDG; //He4+ K+
          motherPDG = ( pdg1 == 321) && (pdg2 == 1000020060) ? 110010 : motherPDG; //He6+ K+
          motherPDG = ( pdg1 == 321) && (pdg2 == 1000030060) ? 110012 : motherPDG; //Li6+ K+
          motherPDG = ( pdg1 == 321) && (pdg2 == 1000030070) ? 110014 : motherPDG; //Li7+ K+
          motherPDG = ( pdg1 == 321) && (pdg2 == 1000040070) ? 110016 : motherPDG; //Be7+ K+

          motherPDG = ( pdg1 == 2212) && (pdg2 ==       2212) ? 200001 : motherPDG; //p    p
          motherPDG = ( pdg1 == 2212) && (pdg2 == 1000010020) ? 200002 : motherPDG; //d+   p
          motherPDG = ( pdg1 == 2212) && (pdg2 == 1000010030) ? 200003 : motherPDG; //t+   p
          motherPDG = ( pdg1 == 2212) && (pdg2 == 1000020030) ? 200004 : motherPDG; //He3+ p
          motherPDG = ( pdg1 == 2212) && (pdg2 == 1000020040) ? 200005 : motherPDG; //He4+ p
          motherPDG = ( pdg1 == 2212) && (pdg2 == 1000020060) ? 200006 : motherPDG; //He6+ p
          motherPDG = ( pdg1 == 2212) && (pdg2 == 1000030060) ? 200007 : motherPDG; //Li6+ p
          motherPDG = ( pdg1 == 2212) && (pdg2 == 1000030070) ? 200008 : motherPDG; //Li7+ p
          motherPDG = ( pdg1 == 2212) && (pdg2 == 1000040070) ? 200009 : motherPDG; //Be7+ p
        }
        
        if(motherPDG == -1) continue;

        if(!(fDecayReconstructionList.empty()))
        {
          if(fDecayReconstructionList.find(motherPDG) == fDecayReconstructionList.end()) continue;
        }

        {
          idPosDaughtersArray[nBufEntry] = iTr2;
          idNegDaughtersArray[nBufEntry] = iTr1;
          
          daughterPosPDGArray[nBufEntry] = pdg2;
          daughterNegPDGArray[nBufEntry] = pdg1;
          
          pvIndexMotherArray[nBufEntry] = pvIndex;
          
          V0PDGArray[nBufEntry] = motherPDG;
          
          nBufEntry++;

          if(int(nBufEntry) == SimdLen)
          {
            idNegDaughters.load(idNegDaughtersArray);
            idPosDaughters.load(idPosDaughtersArray);
            daughterNegPDG.load(daughterNegPDGArray);
            daughterPosPDG.load(daughterPosPDGArray);
            pvIndexMother.load(pvIndexMotherArray);
            V0PDG.load(V0PDGArray);

            KFParticleDatabase::Instance()->GetMotherMass(V0PDG,massMotherPDG,massMotherPDGSigma);
            mother.SetPDG( V0PDG );
            ConstructV0(vTracks, iSet, iSet,                
                        idPosDaughters, idNegDaughters, daughterPosPDG, daughterNegPDG,
                        mother, mother_temp,
                        nBufEntry, l, dl, Particles, PrimVtx,
                        cuts, pvIndexMother, secCuts, massMotherPDG,
                        massMotherPDGSigma, motherPrimSecCand, nPrimSecCand, vMotherPrim, vMotherSec);
            nBufEntry = 0; 
          }
        }//iV
      }//iTrP
          
      if( nBufEntry>0 )
      {
        for(int iV=nBufEntry; iV<SimdLen; iV++)
        {
          idPosDaughtersArray[iV] = idPosDaughters[0];
          idNegDaughtersArray[iV] = idNegDaughters[0];
        }

        idNegDaughters.load(idNegDaughtersArray);
        idPosDaughters.load(idPosDaughtersArray);
        daughterNegPDG.load(daughterNegPDGArray);
        daughterPosPDG.load(daughterPosPDGArray);
        pvIndexMother.load(pvIndexMotherArray);
        V0PDG.load(V0PDGArray);

        KFParticleDatabase::Instance()->GetMotherMass(V0PDG,massMotherPDG,massMotherPDGSigma);
        mother.SetPDG( V0PDG );

        ConstructV0(vTracks, iSet, iSet,              
                    idPosDaughters, idNegDaughters, daughterPosPDG, daughterNegPDG,
                    mother, mother_temp,
                    nBufEntry, l, dl, Particles, PrimVtx,
                    cuts, pvIndexMother, secCuts, massMotherPDG,
                    massMotherPDGSigma, motherPrimSecCand, nPrimSecCand, vMotherPrim, vMotherSec);
        nBufEntry = 0; 
      }
      
      if(nPrimSecCand>0)
      {
        SaveV0PrimSecCand(motherPrimSecCand,nPrimSecCand,mother_temp,PrimVtx,secCuts,vMotherPrim,vMotherSec);
        nPrimSecCand = 0;
      }
    }
  }
}

void KFParticleFinder::ConstructTrackV0Cand(KFPTrackVector& vTracks,
                                            int32_v& idTracks,
                                            int32_v& trackPDG,
                                            KFParticle* vV0[],
                                            KFParticleSIMD& mother,
                                            std::vector<KFParticleSIMD, KFPSimdAllocator<KFParticleSIMD> >& motherTopo,
                                            KFParticle& mother_temp,
                                            const unsigned short nElements,
                                            kfvector_floatv& l,
                                            kfvector_floatv& dl,
                                            std::vector<KFParticle>& Particles,
                                            std::vector<KFParticleSIMD, KFPSimdAllocator<KFParticleSIMD> >& PrimVtx,
                                            const float32_v* cuts,
                                            const int32_v& pvIndex,
                                            const float32_v& massMotherPDG,
                                            const float32_v& massMotherPDGSigma,
                                            std::vector< std::vector<KFParticle> >* vMotherPrim,
                                            std::vector<KFParticle>* vMotherSec)
{
  /** Constructs a candidate from a track and already reconstructed particle candidate.
   ** \param[in] vTracks - vector with tracks.
   ** \param[in] idTracks - indices of particles from the vector of tracks.
   ** \param[in] trackPDG - PDG hypothesis of the SIMD vector of tracks.
   ** \param[in] vV0 - array with already reconstructed particle candidate with the size of SIMD vector.
   ** \param[out] mother - constructed 2-daughter SIMD-candidate.
   ** \param[in] motherTopo - preallocated SIMD vector for topological constraint for better performance.
   ** \param[in] mother_temp - temporary object to extract KFParticle from constructed KFParticleSIMD mother. Preallocated for better performance.
   ** \param[in] nElements - number of elements in each SIMD vector.
   ** \param[in] l - SIMD-vector with extracted distance to the primary vertex. Is preallocated for better performance.
   ** \param[in] dl - SIMD-vector with extracted error of distance to the primary vertex. Is preallocated for better performance.
   ** \param[out] Particles - the output array with the reconstructed particle-candidates.
   ** \param[in] PrimVtx - array with primary vertices.
   ** \param[in] cuts - set of cuts: \f$l/\Delta l\f$, \f$\chi^2_{topo}\f$, \f$\chi^2_{geo}\f$.
   ** \param[in] pvIndex - index of the primary vertex for reconstruction of resonances. Tracks should come from the same vertex.
   ** in case of other particles the value should be "-1".
   ** \param[in] massMotherPDG - PDG table mass for the mother particle, is used for selection of primary and secondary candidates.
   ** \param[in] massMotherPDGSigma - sigma of the peak width, is used for selection of primary and secondary candidates.
   ** \param[out] vMotherPrim - array with output primary candidates if any. If pointer is set to NULL - not filled.
   ** \param[out] vMotherSec - array with output secondary candidates if any. If pointer is set to NULL - not filled.
   **/
  
  mask32_v isPrimary = (pvIndex>-1);
  
  int32_v trackId(KFP::SIMD::UninitializeTag{});
  trackId.gather( &(vTracks.Id()[0]), idTracks );

  KFParticleSIMD V0(vV0,nElements);
  KFParticleSIMD track(vTracks, idTracks, trackPDG);
  track.SetId(trackId);
    
  mask32_v isSameParticle = ((abs(mother.PDG()) ==    int32_v(4122)) ||
                             (abs(mother.PDG()) ==  int32_v(114122)) ||
                             (abs(mother.PDG()) ==  int32_v(204122)) ||
                             (abs(mother.PDG()) ==  int32_v(504122)) ||
                             (abs(mother.PDG()) ==  int32_v(404122)) ||
                             (abs(mother.PDG()) ==     int32_v(425)) ||
                             (abs(mother.PDG()) ==     int32_v(427)) ||
                             (abs(mother.PDG()) ==  int32_v(200411)) ||
                             (abs(mother.PDG()) ==  int32_v(300411)) ||
                             (abs(mother.PDG()) ==  int32_v(300431)) ||
                             (abs(mother.PDG()) ==  int32_v(400431)) ||
                             (abs(mother.PDG()) ==     int32_v(411)) ||
                             (abs(mother.PDG()) ==     int32_v(431)) ||
                             (abs(mother.PDG()) ==     int32_v(429)) ||
                             (abs(mother.PDG()) == int32_v(1003334)) ||
                             (abs(mother.PDG()) ==    int32_v(3001)) ||
                             (abs(mother.PDG()) ==    int32_v(3006)) ||
                             (abs(mother.PDG()) ==    int32_v(3007)) ||
                             (abs(mother.PDG()) ==    int32_v(3009)) ||
                             (abs(mother.PDG()) ==  int32_v(100321)) ||
                             (abs(mother.PDG()) >= int32_v(3011) && abs(mother.PDG()) <= int32_v(3039))
  );
  if( isSameParticle.isEmpty() )
  {
#ifdef CBM
    float32_v ds[2] = {0.f,0.f};
    float32_v dsdr[4][6];
    track.GetDStoParticle( V0, ds, dsdr );
    track.TransportToDS(ds[0], dsdr[0]);
    V0.TransportToDS(ds[1], dsdr[3]);
#endif
    const KFParticleSIMD* vDaughtersPointer[2] = {&track, &V0};
    mother.Construct(vDaughtersPointer, 2, 0);
  }
  else
  {
    int32_v motherPDG = mother.PDG();
    mother = V0;
    mother.SetPDG(motherPDG);
    track.TransportToPoint(V0.Parameters());
    mother += track;
  }

  mask32_v active = (int32_v::indicesSequence() < int(nElements));
  
  mask32_v saveParticle = active;
  saveParticle &= (mother.Chi2()/toFloat(mother.NDF()) < cuts[2] );
  saveParticle &= isFinite(mother.GetChi2());
  saveParticle &= (mother.GetChi2() > 0.0f);
  saveParticle &= (mother.GetChi2() == mother.GetChi2());

  if( saveParticle.isEmpty() ) { return; }

  mask32_v isSameTrack;
  for(unsigned int iD=0; iD<V0.DaughterIds().size(); iD++)
    isSameTrack |= ( int32_v(V0.DaughterIds()[iD]) == int32_v(trackId) );
  
  saveParticle &= !isSameTrack;
  if( saveParticle.isEmpty() ) { return; }        
      
  float32_v lMin(1.e8f);
  float32_v ldlMin(1.e8f);
  mask32_v isParticleFromVertex;

  for(int iP=0; iP<fNPV; iP++)
  {
    mask32_v isParticleFromVertexLocal;
    mother.GetDistanceToVertexLine(PrimVtx[iP], l[iP], dl[iP], &isParticleFromVertexLocal);
    isParticleFromVertex |= isParticleFromVertexLocal;
    float32_v ldl = (l[iP]/dl[iP]);
    lMin = select( (l[iP] < lMin) && active, l[iP], lMin);
    ldlMin = select( (ldl < ldlMin) && active, ldl, ldlMin);
  }
  saveParticle &= (lMin < 200.f);
  saveParticle &= (((!isPrimary) && isParticleFromVertex) || isPrimary );
  if( saveParticle.isEmpty() ) { return; }

  isSameParticle = isSameParticle || isPrimary;
  if(!((isSameParticle).isFull()))
  {
    mask32_v isParticleFromVertexLocal;
    float32_v l1, dl1;
    V0.GetDistanceToVertexLine(mother, l1, dl1, &isParticleFromVertexLocal);
    
    saveParticle &= ( isSameParticle || ((!isSameParticle) && isParticleFromVertexLocal));
    if( saveParticle.isEmpty() ) { return; }
  }

  saveParticle &= ( ((!isPrimary) && ldlMin > cuts[0]) || isPrimary );

  float32_v p1p2 = track.Px()*V0.Px() + track.Py()*V0.Py() + track.Pz()*V0.Pz();
  float32_v p12  = track.Px()*track.Px() + track.Py()*track.Py() + track.Pz()*track.Pz();
  float32_v p22  = V0.Px()*V0.Px() + V0.Py()*V0.Py() + V0.Pz()*V0.Pz();
  saveParticle &= p1p2 > -p12;
  saveParticle &= p1p2 > -p22;
  
  mask32_v setLCut = abs(mother.PDG()) == 3312 || abs(mother.PDG()) == 3334 || abs(mother.PDG()) == 3001;
  saveParticle &= ( (setLCut && lMin > float32_v(fLCut)) || (!setLCut) );

  ldlMin = 1.e8f;
  for(int iP=0; iP<fNPV; iP++)
  {
    motherTopo[iP] = mother;
    motherTopo[iP].SetProductionVertex(PrimVtx[iP]);
    motherTopo[iP].GetDecayLength(l[iP], dl[iP]);
    float32_v ldl = (l[iP]/dl[iP]);
    ldlMin = select( (ldl < ldlMin) && active, ldl, ldlMin);
  }
                       
  vector<int> iPrimVert[SimdLen];
  mask32_v isPrimaryPart;

  for(int iP=0; iP<fNPV; iP++)
  {
    const float32_v& motherTopoChi2Ndf = motherTopo[iP].GetChi2()/toFloat(motherTopo[iP].GetNDF());
    const mask32_v isPrimaryPartLocal = ( motherTopoChi2Ndf < cuts[1] );
    isPrimaryPart |= isPrimaryPartLocal;
    for(int iV=0; iV<SimdLen; iV++)
    {
      if(isPrimaryPartLocal[iV])
        iPrimVert[iV].push_back(iP);
    }
  }
            
  for(unsigned int iv=0; iv<nElements; iv++)
  {
    if(!saveParticle[iv]) continue; 
    
    mother.GetKFParticle(mother_temp, iv);
    if( mother.PDG()[iv] == 3312 )
    {
      fLPi.push_back(mother_temp);
      fLPiPIndex.push_back( V0.DaughterIds()[1][iv] );
    }
    
//     if(  mother.PDG()[iv] == 100411 )
//     {
//       fK0PiPlus.push_back(mother_temp);
//       fK0PiMinusIndex.push_back( V0.DaughterIds()[0][iv] );
//       continue;
//     }
    
    //TODO check if needed with current implementation
    // reset daughter ids for 3- and 4-particle decays
    if( (abs(mother.PDG()[iv]) == 411)  ||
        (abs(mother.PDG()[iv]) == 429) ||
        (abs(mother.PDG()[iv]) == 431)  ||
        (abs(mother.PDG()[iv]) == 4122) ||
        (abs(mother.PDG()[iv]) == 114122) ||
        (abs(mother.PDG()[iv]) == 204122) ||
        (abs(mother.PDG()[iv]) == 314122) ||
        (abs(mother.PDG()[iv]) == 404122) ||
        (abs(mother.PDG()[iv]) == 504122) ||
        (abs(mother.PDG()[iv]) == 425) ||
        (abs(mother.PDG()[iv]) == 427) ||
        (abs(mother.PDG()[iv]) == 200411) ||
        (abs(mother.PDG()[iv]) == 300411) ||
        (abs(mother.PDG()[iv]) == 300431) ||
        (abs(mother.PDG()[iv]) == 400431) ||
        (abs(mother.PDG()[iv]) == 1003334) ||
        (abs(mother.PDG()[iv]) == 3001) )
    {
      mother_temp.CleanDaughtersId();
      for(int iD=0; iD < vV0[iv]->NDaughters(); iD++)
        mother_temp.AddDaughterId( vV0[iv]->DaughterIds()[iD] );
      mother_temp.AddDaughterId(trackId[iv]);
    }
    
    if( mother.PDG()[iv] == 411 ) 
    {
      fDPlus.push_back(mother_temp);
      continue;
    }
    if( mother.PDG()[iv] == -411 ) 
    {
      fDMinus.push_back(mother_temp);
      continue;
    }
    if( mother.PDG()[iv] == 300411 ) 
    {
      fDPlus3Pi.push_back(mother_temp);
      continue;
    }
    if( mother.PDG()[iv] == -300411 ) 
    {
      fDMinus3Pi.push_back(mother_temp);
      continue;
    }
    if( mother.PDG()[iv] == 400431 ) 
    {
      fDsPlusK2Pi.push_back(mother_temp);
      continue;
    }
    if( mother.PDG()[iv] == -400431 ) 
    {
      fDsMinusK2Pi.push_back(mother_temp);
      continue;
    }
    if( mother.PDG()[iv] == 504122 ) 
    {
      fLcPlusP2Pi.push_back(mother_temp);
      continue;
    }
    if( mother.PDG()[iv] == -504122 ) 
    {
      fLcMinusP2Pi.push_back(mother_temp);
      continue;
    }
//     if( mother.PDG()[iv] == 427 )
//     {
//       fK0PiPi.push_back(mother_temp);
//       continue;
//     }
    
    if( mother.PDG()[iv] == 429 ) 
    {
      fD04.push_back(mother_temp);
      continue;
    }
    if( mother.PDG()[iv] == -429 ) 
    {
      fD04bar.push_back(mother_temp);
      continue;
    }
    
    if( mother.PDG()[iv] == 3203 ) 
      fLLn.push_back(mother_temp);
    if( mother.PDG()[iv] == 3010 ) 
      fH5LL.push_back(mother_temp);
    if( mother.PDG()[iv] == 3028 )
      fPPpi.push_back(mother_temp);
    if( mother.PDG()[iv] ==-3028 )
      fPPpiBar.push_back(mother_temp);
    
    if( mother.PDG()[iv] == 3006)
      fHe3PPi.push_back(mother_temp);
    if( mother.PDG()[iv] == 3007)
      fHe4PPi.push_back(mother_temp);

    mother_temp.SetId(Particles.size());

    if( !(isPrimaryPart[iv]) /*|| (mother.PDG()[iv] == 3006) || (mother.PDG()[iv] == 3007) */)
    {
      if( vMotherSec )
      {
        float mass, errMass;
        mother_temp.GetMass(mass, errMass);
        if(abs(mother.PDG()[iv]) == 3324)
        {
          vMotherSec->push_back(mother_temp);
        }
        else
        {    
          if( (fabs(mass - massMotherPDG[iv])/massMotherPDGSigma[iv]) <= 3 )
          {
            KFParticle mother_sec = mother_temp;
            mother_sec.SetNonlinearMassConstraint(massMotherPDG[iv]);
            vMotherSec->push_back(mother_temp);
          }
        }
      }
      // if(!(mother.PDG()[iv] == 3006 || mother.PDG()[iv] == 3007))
        continue;
    }
    
    if( (mother.PDG()[iv] == 3006) || (mother.PDG()[iv] == 3007) )
    {
      float mass, errMass;
      mother_temp.GetMass(mass, errMass);
      if( (fabs(mass - massMotherPDG[iv])/massMotherPDGSigma[iv]) <= 6.f )
      {
        KFParticle mother_sec = mother_temp;
        mother_sec.SetNonlinearMassConstraint(massMotherPDG[iv]);
        if(mother.PDG()[iv] == 3006) fHe4L.push_back(mother_sec);
        if(mother.PDG()[iv] == 3007) fHe5L.push_back(mother_sec);
      }
    }

    //check Ds+ and Lc+ candidates not to be D+
//     if(abs(mother_temp.GetPDG())==431 || abs(mother_temp.GetPDG())==4122)
//     {
//       KFPTrack dPionTrack;
//       vTracks.GetTrack(dPionTrack, idTracks[iv]);
//       KFParticle dPion(dPionTrack, 211);
//       
//       KFParticle dMeson = *vV0[iv];
//       dMeson += dPion;
//       float dMass, dMassError;
//       dMeson.GetMass(dMass, dMassError);
//       if(fabs(dMass - KFParticleDatabase::Instance()->GetDPlusMass())/KFParticleDatabase::Instance()->GetDPlusMassSigma() < 3) continue;
//     }
    
    if(abs(mother.GetPDG()[iv]) == 521 || abs(mother.GetPDG()[iv]) == 529 || abs(mother.GetPDG()[iv]) == 511 || abs(mother.GetPDG()[iv]) == 519)
    {
      KFParticle daughter_temp = *vV0[iv];
      float massPDG = KFParticleDatabase::Instance()->GetDPlusMass();
      float massSigmaPDG = KFParticleDatabase::Instance()->GetDPlusMassSigma();
      if(abs(mother.GetPDG()[iv]) == 521 || abs(mother.GetPDG()[iv]) == 529)
      {
        massPDG = KFParticleDatabase::Instance()->GetD0Mass();
        massSigmaPDG = KFParticleDatabase::Instance()->GetD0MassSigma();
      }
      float mass, dm;
      daughter_temp.GetMass(mass,dm);
      if( (fabs(mass - massPDG)/massSigmaPDG) > 3 ) continue;
      
//       KFParticleSIMD daughter_tempSIMD(daughter_temp);
//       daughter_tempSIMD.SetProductionVertex(PrimVtx[0]);
//       if(daughter_tempSIMD.GetChi2()[0]/daughter_tempSIMD.GetNDF()[0] < 3. ) continue;
      
      daughter_temp.SetId(Particles.size());
      daughter_temp.SetPDG(-1);
      mother_temp.SetId(Particles.size()+1);
      mother_temp.CleanDaughtersId();
      mother_temp.AddDaughterId(Particles.size());
      mother_temp.AddDaughterId(trackId[iv]);
      Particles.push_back(daughter_temp);
    }
    Particles.push_back(mother_temp);

    if( abs(mother.GetPDG()[iv]) == 3334 ) //Omega-
    {
      float mass, errMass;
      mother_temp.GetMass(mass, errMass);
      
      vector< vector<KFParticle> >* motherVector = &fPrimCandidates[7];
      if( mother.GetPDG()[iv] == 3334 )
        motherVector = &fPrimCandidates[8];
        
      mother_temp.SetNonlinearMassConstraint(massMotherPDG[iv]);

      if( (fabs(mass - massMotherPDG[iv])/massMotherPDGSigma[iv]) <= 3 )
        for(unsigned int iP=0; iP<iPrimVert[iv].size(); iP++)
          (*motherVector)[iPrimVert[iv][iP]].push_back(mother_temp);
    }
    
    if(vMotherPrim)
    {
      if( !((abs(mother.GetPDG()[iv]) == 3312) || 
            (abs(mother.GetPDG()[iv]) == 3324) || 
            (abs(mother.GetPDG()[iv]) == 100321) )) continue;
      if(abs(mother.PDG()[iv]) == 3324 || abs(mother.GetPDG()[iv]) == 100321)
      {
        for(unsigned int iP=0; iP<iPrimVert[iv].size(); iP++)
          (*vMotherPrim)[iPrimVert[iv][iP]].push_back(mother_temp);
      }
      else
      {
        float mass, errMass;
        mother_temp.GetMass(mass, errMass);
        mother_temp.SetNonlinearMassConstraint(massMotherPDG[iv]);
        if( (fabs(mass - massMotherPDG[iv])/massMotherPDGSigma[iv]) <= 3 )
          for(unsigned int iP=0; iP<iPrimVert[iv].size(); iP++)
            (*vMotherPrim)[iPrimVert[iv][iP]].push_back(mother_temp);
      }
    }
  }
}

void KFParticleFinder::FindTrackV0Decay(vector<KFParticle>& vV0,
                                        const int V0PDG,
                                        KFPTrackVector& vTracks,
                                        const int q,
                                        const int firstTrack,
                                        const int lastTrack,
                                        vector<KFParticle>& Particles,    
                                        std::vector<KFParticleSIMD, KFPSimdAllocator<KFParticleSIMD> >& PrimVtx,
                                        int v0PVIndex,
                                        kfvector_float* ChiToPrimVtx,
                                        vector< vector<KFParticle> >* vMotherPrim,
                                        vector<KFParticle>* vMotherSec)
{
  /** Combines tracks and already reconstructed particle candidate of certain type in the next candidate.
   ** \param[in] vV0 - the input vector with already reconstructed particle candidate.
   ** \param[in] V0PDG - PDG code of the provided particle candidates.
   ** \param[in] vTracks - vector with input tracks.
   ** \param[in] q - charge of the provided tracks.
   ** \param[in] firstTrack - index of the first track to be used.
   ** \param[in] lastTrack - index of the last track to be used.
   ** \param[out] Particles - the output array with the reconstructed particle-candidates.
   ** \param[in] PrimVtx - array with primary vertices.
   ** \param[in] v0PVIndex - index of the corresponding primary vertex if the tracks are primary. If not "-1" should be set.
   ** \param[in] ChiToPrimVtx - vector with the \f$\chi^2_{prim}\f$ deviations for provided tracks. If tracks are primary NULL pointer is provided.
   ** \param[out] vMotherPrim - array with output primary candidates.
   ** \param[out] vMotherSec - array with output secondary candidates.
   **/
  
  if( (vV0.size() < 1) || ((lastTrack-firstTrack) < 1) ) return;
  KFParticle mother_temp;

  KFParticle* v0Pointer[SimdLen];

  KFParticleSIMD mother, track;
  std::vector<KFParticleSIMD, KFPSimdAllocator<KFParticleSIMD> > motherTopo(fNPV);

  kfvector_floatv l(fNPV), dl(fNPV);

  float32_v cuts[3];

  // for secondary V0
  unsigned int nBufEntry = 0;
  float32_v dS;
  int32_v idTrack;
  int32_v trackPDGMother(-1);  
  int32_v pvIndexMother(-1);
  
  float32_v massMotherPDG(0.f), massMotherPDGSigma(0.f);
  int32_v motherParticlePDG(0);

  alignas(SimdSize) std::int32_t idTrackArray[SimdLen];
  alignas(SimdSize) std::int32_t trackPDGMotherArray[SimdLen];
  alignas(SimdSize) std::int32_t pvIndexMotherArray[SimdLen];
  alignas(SimdSize) std::int32_t motherParticlePDGArray[SimdLen];
  alignas(SimdSize) float massMotherPDGArray[SimdLen];
  alignas(SimdSize) float massMotherPDGSigmaArray[SimdLen];
  alignas(SimdSize) float cutsArray[3][SimdLen];


//   Particles.reserve(Particles.size() + vV0.size());

  bool isCharm = ((abs(V0PDG) == 421) || (abs(V0PDG) == 411) || (abs(V0PDG) == 429) || (abs(V0PDG) == 420) || (abs(V0PDG) == 419)) && (v0PVIndex<0);

  mask32_v isPvIndexNeg, isPvIndexPos;
  if(v0PVIndex >= 0) isPvIndexPos.setTrue();
  else isPvIndexNeg.setTrue();

  for(unsigned int iV0=0; iV0 < vV0.size(); iV0++)
  {
    if(vV0[iV0].GetPDG() != V0PDG) continue;

    int iNegDaughter = vV0[iV0].DaughterIds()[0];
    int iPosDaughter = vV0[iV0].DaughterIds()[1];
    
    for(int iTr=firstTrack; iTr < lastTrack; iTr += SimdLen)
    {
      const int NTracks = (iTr + SimdLen < lastTrack) ? SimdLen : (lastTrack - iTr);

      const int32_v& trackPDG = reinterpret_cast<const int32_v&>(vTracks.PDG()[iTr]);
      const int32_v& trackPVIndex = reinterpret_cast<const  int32_v&>(vTracks.PVIndex()[iTr]);
      
      const mask32_v& isTrackSecondary = (trackPVIndex < 0);
      const mask32_v& isSecondary = isPvIndexNeg && isTrackSecondary;
      const mask32_v& isPrimary   = isPvIndexPos && (!isTrackSecondary);
      const mask32_v& isSamePV = (isPrimary && (v0PVIndex == trackPVIndex)) || !(isPrimary);

      mask32_v closeDaughters = isSamePV && (int32_v::indicesSequence() < int(NTracks));

//       if(v0PVIndex < 0)
//       {
//         KFParticleSIMD v0(vV0[iV0]);
//         track.Load(vTracks, iTr, trackPDG);
     
//         float32_v dsV0, dsTrack;
//         float32_v dsdrV0[6] = {0.f,0.f,0.f,0.f,0.f,0.f}; 
//         float32_v dsdrTrack[6] = {0.f,0.f,0.f,0.f,0.f,0.f};
//         float32_v par1[8], cov1[36], par2[8], cov2[36];
//         v0.GetDStoParticle(track, dsV0, dsTrack);
//         v0.Transport(dsV0, dsdrV0, par1, cov1);
//         track.Transport(dsTrack, dsdrTrack, par2, cov2);
//   
//         const float32_v& dx = par1[0] - par2[0];
//         const float32_v& dy = par1[1] - par2[1];
//         const float32_v& dz = par1[2] - par2[2];
//         const float32_v& r2 = dx*dx + dy*dy + dz*dz;
//         
//         const float32_v vtx[3] = {(par1[0] + par2[0])/2.f,
//                                 (par1[1] + par2[1])/2.f,
//                                 (par1[2] + par2[2])/2.f, };
// 
//         v0.CorrectErrorsOnS(par1, vtx, cov1);
//         track.CorrectErrorsOnS(par2, vtx, cov2);
//         
//         const float32_v cov[6] = {cov1[0]+cov2[0],
//                                 cov1[1]+cov2[1],
//                                 cov1[2]+cov2[2],
//                                 cov1[3]+cov2[3],
//                                 cov1[4]+cov2[4],
//                                 cov1[5]+cov2[5] };
//         const float32_v& err2 = cov[0]*dx*dx + cov[2]*dy*dy + cov[5]*dz*dz + 2.f*( cov[1]*dx*dy + cov[3]*dx*dz + cov[4]*dy*dz );
//                 
//         closeDaughters &= ( (r2 < float32_v(1.f)) && (r2*r2/err2) < float32_v(3.f) && isSecondary);
//         closeDaughters &= v0.GetDeviationFromParticle(track) < float32_v(10.f);
//       }
      
      if(v0PVIndex < 0)
      {
        KFParticleSIMD v0(vV0[iV0]);
        track.Load(vTracks, iTr, trackPDG);
        closeDaughters &= v0.GetDistanceFromParticle(track) < float32_v(fDistanceCut);
        if(closeDaughters.isEmpty()) continue;
      }
      
      int32_v trackPdgPos[2];
      mask32_v active[2];

      int nPDGPos = 2;
      
      active[0] = closeDaughters;
      active[1] = (trackPDG == -1) && isSecondary && closeDaughters;
      
      trackPdgPos[0] = trackPDG;
      
      if( (trackPDG == -1).isEmpty() || (abs(V0PDG) ==  421) || (abs(V0PDG) ==  411) )
      {
        nPDGPos = 1;
      }
      else
      {
        trackPdgPos[0] = select(trackPDG == -1, q*211, trackPdgPos[0]);
        nPDGPos = 1;//TODO
        trackPdgPos[1] = select(isSecondary, q*321, trackPdgPos[1]);
      }

      for(int iPDGPos=0; iPDGPos<nPDGPos; iPDGPos++)
      {
        if(active[iPDGPos].isEmpty()) continue;
        
        //detetrmine a pdg code of the mother particle
        
        int32_v motherPDG(-1);
        
        if( V0PDG == 3122 )
        {
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos]      == -211)       ,    3312, motherPDG);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos]      ==  211)       ,  304122, motherPDG);
          motherPDG = select( isSecondary && (abs(trackPdgPos[iPDGPos]) ==  321)       ,    3334, motherPDG);
          motherPDG = select( isPrimary   && (trackPdgPos[iPDGPos]      ==  211)       ,    3224, motherPDG); 
          motherPDG = select( isPrimary   && (trackPdgPos[iPDGPos]      ==  -211)      ,    3114, motherPDG);
          motherPDG = select( isPrimary   && (trackPdgPos[iPDGPos]      == -321)       , 1003314, motherPDG);          
          motherPDG = select( isPrimary   && (trackPdgPos[iPDGPos]      == 2212)       , 1003003, motherPDG);
          motherPDG = select( isPrimary   && (trackPdgPos[iPDGPos]      == 1000010020) , 1003004, motherPDG);
          motherPDG = select( isPrimary   && (trackPdgPos[iPDGPos]      == 1000010030) , 1003005, motherPDG);
          motherPDG = select( isPrimary   && (trackPdgPos[iPDGPos]      == 1000020030) , 1003006, motherPDG);
          motherPDG = select( isPrimary   && (trackPdgPos[iPDGPos]      == 1000020040) , 1003007, motherPDG);
        }
        else if( V0PDG == -3122 )
        {
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos]      ==         211),    -3312, motherPDG);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos]      ==        -211),  -304122, motherPDG);
          motherPDG = select( isSecondary && (abs(trackPdgPos[iPDGPos]) ==         321),    -3334, motherPDG);
          motherPDG = select( isPrimary   && (trackPdgPos[iPDGPos]      ==        -211),    -3224, motherPDG);
          motherPDG = select( isPrimary   && (trackPdgPos[iPDGPos]      ==         211),    -3114, motherPDG);
          motherPDG = select( isPrimary   && (trackPdgPos[iPDGPos]      ==         321), -1003314, motherPDG);
          motherPDG = select( isPrimary   && (trackPdgPos[iPDGPos]      ==       -2212), -1003003, motherPDG);
          motherPDG = select( isPrimary   && (trackPdgPos[iPDGPos]      == -1000010020), -1003004, motherPDG);
          motherPDG = select( isPrimary   && (trackPdgPos[iPDGPos]      == -1000010030), -1003005, motherPDG);
          motherPDG = select( isPrimary   && (trackPdgPos[iPDGPos]      == -1000020030), -1003006, motherPDG);
          motherPDG = select( isPrimary   && (trackPdgPos[iPDGPos]      == -1000020040), -1003007, motherPDG);
        }
        else if( V0PDG == 310 )
        {
          motherPDG = select( isPrimary   && (trackPdgPos[iPDGPos] ==   211),     323, motherPDG);
          motherPDG = select( isPrimary   && (trackPdgPos[iPDGPos] ==  -211),    -323, motherPDG);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] ==   211),  100411, motherPDG);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] ==  -211), -100411, motherPDG);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] ==   321),  100431, motherPDG);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] ==  -321), -100431, motherPDG);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] ==  2212),  104122, motherPDG);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == -2212), -104122, motherPDG);
        }
        else if( V0PDG == 100310 )
        {
          const int32_v& id = reinterpret_cast<const int32_v&>(vTracks.Id()[iTr]);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] ==  211) && (id>iPosDaughter),  100321, motherPDG);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == -211) && (id>iNegDaughter), -100321, motherPDG);
        }
        else if( V0PDG == 3312 )
          motherPDG = select( isPrimary && (trackPdgPos[iPDGPos] ==  211), 3324, motherPDG);
        else if( V0PDG == -3312)
          motherPDG = select( isPrimary && (trackPdgPos[iPDGPos] == -211), -3324, motherPDG);
        else if( V0PDG == 3324 )
          motherPDG = select( isPrimary && (trackPdgPos[iPDGPos] == -321), 1003334, motherPDG);
        else if( V0PDG == -3324 )
          motherPDG = select( isPrimary && (trackPdgPos[iPDGPos] ==  321), -1003334, motherPDG);
        else if(V0PDG ==  421)
        {
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] ==  211),  411, motherPDG);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] ==  321),  431, motherPDG);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == 2212), 4122, motherPDG);
          const int32_v& id = reinterpret_cast<const int32_v&>(vTracks.Id()[iTr]);
          mask32_v isDMeson = isSecondary && (trackPdgPos[iPDGPos] ==  211);
          active[iPDGPos] &= (!(isDMeson)) || (isDMeson && ( id > iPosDaughter) );
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == -211),  -521, motherPDG);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == -321),  -529, motherPDG);
          motherPDG = select( isPrimary  && (trackPdgPos[iPDGPos] ==   211), 10411, motherPDG);
        }
        else if(V0PDG == -421)
        {
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == -211),  -411, motherPDG);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == -321),  -431, motherPDG);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] ==-2212), -4122, motherPDG);
          const int32_v& id = reinterpret_cast<const int32_v&>(vTracks.Id()[iTr]);
          mask32_v isDMeson = isSecondary && (trackPdgPos[iPDGPos] == -211);
          active[iPDGPos] &= (!(isDMeson)) || (isDMeson && ( id > iNegDaughter) );
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] ==  211) ,    521, motherPDG);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] ==  321) ,    529, motherPDG);
          motherPDG = select( isPrimary   && (trackPdgPos[iPDGPos] ==  -211), -10411, motherPDG);
        }
        else if(V0PDG == 420 && q>0)
        {
          motherPDG = select( isSecondary && (abs(trackPdgPos[iPDGPos]) ==  211), 300411, motherPDG);
          motherPDG = select( isSecondary && (abs(trackPdgPos[iPDGPos]) ==  321), 400431, motherPDG);
          motherPDG = select( isSecondary && (abs(trackPdgPos[iPDGPos]) == 2212), 504122, motherPDG);
          const int32_v& id = reinterpret_cast<const int32_v&>(vTracks.Id()[iTr]);
          mask32_v isDMeson = isSecondary && (abs(trackPdgPos[iPDGPos]) ==  211);
          active[iPDGPos] &= (!(isDMeson)) || (isDMeson && ( id > iPosDaughter) );
        }
        else if(V0PDG == 420 && q<0)
        {
          motherPDG = select( isSecondary && (abs(trackPdgPos[iPDGPos]) ==  211), -300411, motherPDG);
          motherPDG = select( isSecondary && (abs(trackPdgPos[iPDGPos]) ==  321), -400431, motherPDG);
          motherPDG = select( isSecondary && (abs(trackPdgPos[iPDGPos]) == 2212), -504122, motherPDG);
          const int32_v& id = reinterpret_cast<const int32_v&>(vTracks.Id()[iTr]);
          mask32_v isDMeson = isSecondary && (abs(trackPdgPos[iPDGPos]) ==  211);
          active[iPDGPos] &= (!(isDMeson)) || (isDMeson && ( id > iNegDaughter) );
        }
        else if(V0PDG == 411)
        {
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == -211),   429, motherPDG);
          motherPDG = select( isPrimary   && (trackPdgPos[iPDGPos] == -211), 10421, motherPDG);
        }
        else if(V0PDG == -411)
        {
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] ==  211),   -429, motherPDG);
          motherPDG = select( isPrimary   && (trackPdgPos[iPDGPos] ==  211), -10421, motherPDG);
        }
        else if(V0PDG == 419)
        {
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == -211), -511, motherPDG);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == -321), -519, motherPDG);
        }
        else if(V0PDG == -419)
        {
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == 211), 511, motherPDG);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == 321), 519, motherPDG);
        }
        else if(V0PDG == 429)      
          motherPDG = select( isPrimary && (trackPdgPos[iPDGPos] == 211), 20411, motherPDG);
        else if(V0PDG == -429)
          motherPDG = select( isPrimary && (trackPdgPos[iPDGPos] ==  -211), -20411, motherPDG);
        else if( V0PDG == 3002 )
        {
          const int32_v& id = reinterpret_cast<const int32_v&>(vTracks.Id()[iTr]);
          mask32_v isSameProton = (id == fLPiPIndex[iV0]);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == 2212) && (!isSameProton), 3001, motherPDG);
        }
        else if( V0PDG == 100411 )
        {
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == -211), 425, motherPDG);
        }
        else if( V0PDG == 100431 )
        {
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == -321), 427, motherPDG);
        }
        else if( V0PDG == 425)
        {
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] ==  211),  200411, motherPDG);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == -211), -200411, motherPDG);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] ==  321),  300431, motherPDG);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == -321), -300431, motherPDG);
        }
        else if( V0PDG == 111 )
        {
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] ==  2212),    3222, motherPDG);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == -2212),   -3222, motherPDG);
          motherPDG = select( isPrimary   && (trackPdgPos[iPDGPos] ==   321),  100323, motherPDG);
          motherPDG = select( isPrimary   && (trackPdgPos[iPDGPos] ==  -321), -100323, motherPDG);
        }
        else if( V0PDG == 3003 )
        {
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == 2212), 3012, motherPDG);
          const int32_v& id = reinterpret_cast<const int32_v&>(vTracks.Id()[iTr]);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == 1000010020) && (id>iPosDaughter), 3014, motherPDG);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == 1000010030), 3015, motherPDG);
        }
        else if( V0PDG == -3003 )
        {
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == -2212), -3012, motherPDG);
          const int32_v& id = reinterpret_cast<const int32_v&>(vTracks.Id()[iTr]);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == -1000010020) && (id>iNegDaughter), -3014, motherPDG);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == -1000010030), -3015, motherPDG);
        }
        else if( V0PDG == 3103 )
        {
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == 2212), 3013, motherPDG);
          const int32_v& id = reinterpret_cast<const int32_v&>(vTracks.Id()[iTr]);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == 1000010030) && (id>iPosDaughter), 3017, motherPDG);
        }
        else if( V0PDG ==-3103 )
        {
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == -2212), -3013, motherPDG);
          const int32_v& id = reinterpret_cast<const int32_v&>(vTracks.Id()[iTr]);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == -1000010030) && (id>iNegDaughter), -3017, motherPDG);
        }
        else if( V0PDG == 3004 )
        {
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] ==  2212) , 3006, motherPDG);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == 1000010020), 3018, motherPDG);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == 1000010030), 3020, motherPDG);
          const int32_v& id = reinterpret_cast<const int32_v&>(vTracks.Id()[iTr]);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == 1000020030) && (id>iPosDaughter), 3024, motherPDG);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == 1000020040), 3026, motherPDG);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] ==  -211) && (id!=iNegDaughter), 3203, motherPDG);
        }
        else if( V0PDG == -3004 )
        {
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == -2212), -3006, motherPDG);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == -1000010020), -3018, motherPDG);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == -1000010030), -3020, motherPDG);
          const int32_v& id = reinterpret_cast<const int32_v&>(vTracks.Id()[iTr]);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == -1000020030) && (id>iNegDaughter), -3024, motherPDG);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == -1000020040), -3026, motherPDG);
        }
        else if( V0PDG == 3005 )
        {
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] ==  2212) , 3007, motherPDG);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == 1000010020), 3021, motherPDG);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == 1000010030), 3023, motherPDG);
          const int32_v& id = reinterpret_cast<const int32_v&>(vTracks.Id()[iTr]);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == 1000020040) && (id>iPosDaughter), 3027, motherPDG);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] ==  -211) && (id!=iNegDaughter) , 3040, motherPDG);
        }
        else if( V0PDG == -3005 )
        {
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == -2212), -3007, motherPDG);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == -1000010020), -3021, motherPDG);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == -1000010030), -3023, motherPDG);
          const int32_v& id = reinterpret_cast<const int32_v&>(vTracks.Id()[iTr]);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == -1000020040) && (id>iNegDaughter), -3027, motherPDG);
        }
        else if( V0PDG == 3006 )
        {
          const int32_v& id = reinterpret_cast<const int32_v&>(vTracks.Id()[iTr]);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == 2212) && (id>vV0[iV0].DaughterIds()[2]), 3038, motherPDG);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == -211) && (id>iNegDaughter), 3008, motherPDG);
        }
        else if( V0PDG == 3007 )
        {
          const int32_v& id = reinterpret_cast<const int32_v&>(vTracks.Id()[iTr]);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == 2212) && (id>vV0[iV0].DaughterIds()[2]), 3039, motherPDG);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == -211) && (id>iNegDaughter), 3010, motherPDG);
        }
        else if( V0PDG == 3203 )
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == 2212), 3009, motherPDG);
        else if( V0PDG == 3010 )
        {
          const int32_v& id = reinterpret_cast<const int32_v&>(vTracks.Id()[iTr]);
          mask32_v isSameProton = (id == Particles[vV0[iV0].DaughterIds()[1]].DaughterIds()[2]);
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == 2212) && (!isSameProton), 3011, motherPDG);
        }
        else if( V0PDG == 8122) 
        {
          const int32_v& id = reinterpret_cast<const int32_v&>(vTracks.Id()[iTr]);
          motherPDG = select( isSecondary && (abs(trackPdgPos[iPDGPos]) ==  2212) && (id>iPosDaughter), 3028, motherPDG);
        }
        else if( V0PDG ==-8122) 
        {
          const int32_v& id = reinterpret_cast<const int32_v&>(vTracks.Id()[iTr]);
          motherPDG = select( isSecondary && (abs(trackPdgPos[iPDGPos]) ==  2212) && (id>iNegDaughter), -3028, motherPDG);
        }
        else if( V0PDG == 3028 )
        {
          const int32_v& id = reinterpret_cast<const int32_v&>(vTracks.Id()[iTr]);
          motherPDG = select( isSecondary && (abs(trackPdgPos[iPDGPos]) ==  2212) && (id>vV0[iV0].DaughterIds()[2]), 3029, motherPDG);
        }
        else if( V0PDG ==-3028 )
        {
          const int32_v& id = reinterpret_cast<const int32_v&>(vTracks.Id()[iTr]);
          motherPDG = select( isSecondary && (abs(trackPdgPos[iPDGPos]) ==  2212) && (id>vV0[iV0].DaughterIds()[2]), -3029, motherPDG);
        }
        else if(V0PDG ==  304122)
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] ==  211),  314122, motherPDG);
        else if(V0PDG == -304122)
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == -211), -314122, motherPDG);
        else if(V0PDG ==  314122)
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == -211),  404122, motherPDG);
        else if(V0PDG == -314122)
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] ==  211), -404122, motherPDG);
        else if(V0PDG ==  104122)
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] ==  211),  114122, motherPDG);
        else if(V0PDG == -104122)
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == -211), -114122, motherPDG);
        else if(V0PDG ==  114122)
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] == -211),  204122, motherPDG);
        else if(V0PDG == -114122)
          motherPDG = select( isSecondary && (trackPdgPos[iPDGPos] ==  211), -204122, motherPDG);
        
        active[iPDGPos] &= (motherPDG != -1);
        if(!(fDecayReconstructionList.empty()))
        {
          alignas(SimdSize) std::int32_t motherPdgArray[SimdLen];
          motherPDG.store(motherPdgArray);
          for(int iV=0; iV<SimdLen; iV++)
          {
            if(!(active[iPDGPos][iV])) continue;
            if(fDecayReconstructionList.find(motherPDG[iV]) == fDecayReconstructionList.end())
              motherPdgArray[iV] = -1;
          }
          motherPDG.load(motherPdgArray);
          active[iPDGPos] &= (motherPDG != -1);
        }
        if(ChiToPrimVtx)
          active[iPDGPos] &= ( !( (abs(motherPDG) == 3334 || abs(motherPDG) == 3312 ) ) ||
                             ( (abs(motherPDG) == 3334 || abs(motherPDG) == 3312 ) && (reinterpret_cast<const float32_v&>((*ChiToPrimVtx)[iTr]) > float32_v(fCuts2D[0])) ) );
        
        if(active[iPDGPos].isEmpty()) continue;
        
        if(isCharm)
        {
          track.Load(vTracks, iTr, trackPDG);
          const float32_v& trackPt = track.Px()*track.Px() + track.Py()*track.Py();
          const int32_v& nPixelHits = reinterpret_cast<const int32_v&>(vTracks.NPixelHits()[iTr]);
          
          active[iPDGPos] &= (trackPt >= fCutCharmPt*fCutCharmPt) && (reinterpret_cast<const float32_v&>((*ChiToPrimVtx)[iTr]) > fCutCharmChiPrim ) && (nPixelHits >= int32_v(3));
        }
        {
          mask32_v isCharmParticle = (abs(motherPDG) == 104122) ||
                                  (abs(motherPDG) == 204122) ||
                                  (abs(motherPDG) == 304122) ||
                                  (abs(motherPDG) == 404122) ||
                                  (abs(motherPDG) ==    425) ||
                                  (abs(motherPDG) ==    426) ||
                                  (abs(motherPDG) ==    427) ||
                                  (abs(motherPDG) == 100411) ||
                                  (abs(motherPDG) == 200411) ||
                                  (abs(motherPDG) == 100431) ||
                                  (abs(motherPDG) == 300431) ;
                 
          if(!(isCharmParticle.isEmpty()))
          {
            track.Load(vTracks, iTr, trackPDG);
            const float32_v& trackPt = track.Px()*track.Px() + track.Py()*track.Py();
            const int32_v& nPixelHits = reinterpret_cast<const int32_v&>(vTracks.NPixelHits()[iTr]);
            
            active[iPDGPos] &= ( ((trackPt >= fCutCharmPt*fCutCharmPt) && (reinterpret_cast<const float32_v&>((*ChiToPrimVtx)[iTr]) > fCutCharmChiPrim ) && (nPixelHits >= int32_v(3)) ) && isCharmParticle ) || (!isCharmParticle);
          }
        }
        
        for(int iV=0; iV<NTracks; iV++)
        {
          if(!(active[iPDGPos][iV])) continue;
          

          idTrackArray[nBufEntry] = iTr+iV;
          v0Pointer[nBufEntry] = &vV0[iV0];
          
          trackPDGMotherArray[nBufEntry] = trackPdgPos[iPDGPos][iV];
          
          pvIndexMotherArray[nBufEntry] = v0PVIndex;
          
          float massMother, massMotherSigma;
          KFParticleDatabase::Instance()->GetMotherMass(motherPDG[iV],massMother,massMotherSigma);

          massMotherPDGArray[nBufEntry] = massMother;
          massMotherPDGSigmaArray[nBufEntry] = massMotherSigma;
          motherParticlePDGArray[nBufEntry] = motherPDG[iV];
                    
          int motherType = 0;

          switch (abs(motherPDG[iV]))
          {
            case   3312: motherType = 0; break; //Xi
            case   3334: motherType = 0; break; //Omega
            case 100321: motherType = 0; break; //K -> 3pi
            case   4122: motherType = 1; break; //LambdaC
            case 104122: motherType = 1; break; //LambdaC
            case 304122: motherType = 1; break; //LambdaC
            case 504122: motherType = 1; break; //LambdaC
            case    425: motherType = 1; break; //D0
            case    426: motherType = 1; break; //D0
            case    427: motherType = 1; break; //D0
            case 100411: motherType = 1; break; //D+
            case 300411: motherType = 1; break; //D+
            case 100431: motherType = 1; break; //Ds+
            case 400431: motherType = 1; break; //Ds+
            case    431: motherType = 1; break; //Ds+-
            case    411: motherType = 1; break; //D+-
            case    428: motherType = 1; break; //D0
            case    429: motherType = 1; break; //D0
            case    521: motherType = 1; break; //B+
            case    529: motherType = 1; break; //B+
            case    511: motherType = 1; break; //B0
            case    519: motherType = 1; break; //B0
            case   3001: motherType = 1; break; //H0
            case   3222: motherType = 1; break; //Sigma+
            case   3012: motherType = 1; break; //H3L
            case   3013: motherType = 1; break; //H4L
            case   3014: motherType = 1; break; //H4L
            case   3015: motherType = 1; break; //H5L
            case   3017: motherType = 1; break; //H6L
            case   3006: motherType = 1; break; //He4L
            case   3007: motherType = 1; break; //He5L
            case   3018: motherType = 1; break; //He5L
            case   3020: motherType = 1; break; //He6L
            case   3021: motherType = 1; break; //He6L
            case   3023: motherType = 1; break; //He7L
            case   3024: motherType = 1; break; //Li6L
            case   3026: motherType = 1; break; //Li7L
            case   3027: motherType = 1; break; //Li8L
            case   3203: motherType = 1; break; //LLn
            case   3040: motherType = 1; break; //LLnn
            case   3008: motherType = 1; break; //H4LL
            case   3009: motherType = 1; break; //H4LL
            case   3011: motherType = 1; break; //He6LL
            case   3028: motherType = 1; break; //H2L
            case   3029: motherType = 1; break; //He3L
            case   3038: motherType = 1; break; //Li5L
            case   3039: motherType = 1; break; //Li6L
            default:   motherType = 2; break; //resonances
          }
          for(int iCut=0; iCut<3; iCut++)
            cutsArray[iCut][nBufEntry] = fCutsTrackV0[motherType][iCut];

          nBufEntry++;

          if(int(nBufEntry) == SimdLen)
          {
            idTrack.load(idTrackArray);
            trackPDGMother.load(trackPDGMotherArray);
            pvIndexMother.load(pvIndexMotherArray);
            massMotherPDG.load(massMotherPDGArray);
            massMotherPDGSigma.load(massMotherPDGSigmaArray);
            motherParticlePDG.load(motherParticlePDGArray);
            cuts[0].load(cutsArray[0]);
            cuts[1].load(cutsArray[1]);
            cuts[2].load(cutsArray[2]);

            mother.SetPDG( motherParticlePDG );
            ConstructTrackV0Cand(vTracks,   
                                 idTrack, trackPDGMother, v0Pointer,
                                 mother, motherTopo, mother_temp,
                                 nBufEntry, l, dl, Particles, PrimVtx,
                                 cuts, pvIndexMother, massMotherPDG,
                                 massMotherPDGSigma, vMotherPrim, vMotherSec);
            nBufEntry = 0; 
          }
        }//iV
      }//iPDGPos
    }//iTr
  }

  if(nBufEntry > 0)
  {
    for(int iV=nBufEntry; iV<SimdLen; iV++)
      idTrackArray[iV] = idTrack[0];

    idTrack.load(idTrackArray);
    trackPDGMother.load(trackPDGMotherArray);
    pvIndexMother.load(pvIndexMotherArray);
    massMotherPDG.load(massMotherPDGArray);
    massMotherPDGSigma.load(massMotherPDGSigmaArray);
    motherParticlePDG.load(motherParticlePDGArray);
    cuts[0].load(cutsArray[0]);
    cuts[1].load(cutsArray[1]);
    cuts[2].load(cutsArray[2]);

    mother.SetPDG( motherParticlePDG );
    ConstructTrackV0Cand(vTracks,  
                          idTrack, trackPDGMother, v0Pointer,
                          mother, motherTopo, mother_temp,
                          nBufEntry, l, dl, Particles, PrimVtx,
                          cuts, pvIndexMother, massMotherPDG,
                          massMotherPDGSigma, vMotherPrim, vMotherSec);
    nBufEntry = 0; 
  }
}

void KFParticleFinder::FindLL(const int motherPDG,
                              const int correctTrackPDG,
                              const vector<KFParticle>& vParticles,
                              const KFPTrackVector& vTracks,
                              const int firstTrack,
                              const int lastTrack,
                              const KFParticleSIMD& PrimVtx,
                              vector<KFParticle>& Particles)
{
  if( (vParticles.size() < 1) || ((lastTrack-firstTrack) < 1) ) return;

  if(!(fDecayReconstructionList.empty()))
  {
    if(fDecayReconstructionList.find(motherPDG) == fDecayReconstructionList.end()) return;
  }

  // float massMother, massMotherSigma;
  // KFParticleDatabase::Instance()->GetMotherMass(motherPDG, massMother, massMotherSigma);

  KFParticleSIMD track;
  KFParticle candidate;

  for(unsigned int iP=0; iP < vParticles.size(); iP++)
  {
    const KFParticle& particle = vParticles[iP];
    const KFParticleSIMD particleSIMD(particle);

    for(int iTr=firstTrack; iTr < lastTrack; iTr += SimdLen)
    {
      const int NTracks = (iTr + SimdLen < lastTrack) ? SimdLen : (lastTrack - iTr);
      const int32_v& trackPDG = reinterpret_cast<const int32_v&>(vTracks.PDG()[iTr]);
      const int32_v& trackId  = reinterpret_cast<const int32_v&>(vTracks.Id()[iTr]);

      mask32_v isOk = (trackPDG == correctTrackPDG);

      for(int iDaughter=0; iDaughter<particle.NDaughters(); iDaughter++)
      {
        isOk &= (trackId != particle.DaughterIds()[iDaughter]);
      }

      track.Load(vTracks, iTr, trackPDG);
      track.SetId(trackId);

      KFParticleSIMD mother;
      mother += track;
      mother += particleSIMD;
      mother.SetPDG(motherPDG);

      isOk &= (mother.Chi2()/toFloat(mother.NDF()) < 10.f );
      isOk &= isFinite(mother.GetChi2());
      isOk &= (mother.GetChi2() > 0.0f);
      isOk &= (mother.GetChi2() == mother.GetChi2());

      if( isOk.isEmpty() ) continue;

      // float32_v l
      // float32_v dl;
      // mask32_v isParticleFromVertex;
      // mother.GetDistanceToVertexLine(PrimVtx, l, dl, &isParticleFromVertex);

      for(int iV=0; iV<NTracks; iV++)
      {
        if(!isOk[iV]) continue;

        mother.GetKFParticle(candidate, iV);
        candidate.SetId(Particles.size());
        Particles.push_back(candidate);
      }
    }
  }
}

void KFParticleFinder::SelectParticles(vector<KFParticle>& Particles,
                                       vector<KFParticle>& vCandidates,
                                       std::vector<KFParticleSIMD, KFPSimdAllocator<KFParticleSIMD> >& PrimVtx,
                                       const float& cutChi2Topo,
                                       const float& cutLdL,
                                       const float& mass,
                                       const float& massErr,
                                       const float& massCut)
{
  /** Selects particles from a set of candidates "vCandidates" according to the provided cuts
   ** on \f$\chi^2_{topo}\f$ and \f$l/\Delta l\f$
   ** and stores them to the output array "Particles". Also, "vCandidates" is cleaned, only
   ** selected particles with additional cut on \f$\sigma_{M}\f$ are left there.
   ** \param[out] Particles - output vector with particles.
   ** \param[in,out] vCandidates - vector with the input candidates.
   ** \param[in] PrimVtx - vector with primary vertices.
   ** \param[in] cutChi2Topo - \f$\chi^2_{topo}\f$ cut.
   ** \param[in] cutLdL - \f$l/\Delta l\f$ cut.
   ** \param[in] mass - table mass for the given PDG hypothesis.
   ** \param[in] massErr - sigma of the peak width for the given PDG hypothesis.
   ** \param[in] massCut - \f$\sigma_{M}\f$ cut.
   **/
  KFParticle* cand[SimdLen];
  int nCand = vCandidates.size();
  
  vector<KFParticle> newCandidates;
  kfvector_floatv l(fNPV), dl(fNPV);

  for(int iC=0; iC < nCand; iC += SimdLen)
  {
    int nEntries = (iC + SimdLen < nCand) ? SimdLen : (nCand - iC);

    for(int iv=0; iv<nEntries; iv++)
      cand[iv] = &vCandidates[iC+iv];

    KFParticleSIMD mother(cand,nEntries);
    
    mask32_v saveParticle(int32_v::indicesSequence() < int(nEntries));

    float32_v lMin(1.e8f);
    float32_v ldlMin(1.e8f);
    mask32_v isParticleFromVertex;

    for(int iP=0; iP<fNPV; iP++)
    {
      mask32_v isParticleFromVertexLocal;
      mother.GetDistanceToVertexLine(PrimVtx[iP], l[iP], dl[iP], &isParticleFromVertexLocal);
      isParticleFromVertex |= isParticleFromVertexLocal;
      float32_v ldl = (l[iP]/dl[iP]);
      lMin = select( (l[iP] < lMin) && saveParticle, l[iP], lMin);
      ldlMin = select( (ldl < ldlMin) && saveParticle, ldl, ldlMin);
    }
    saveParticle &= ldlMin > cutLdL;
    saveParticle &= (lMin < 200.f);
    saveParticle &= isParticleFromVertex;
    if( saveParticle.isEmpty() ) continue;

    KFParticleSIMD* candTopo = new KFParticleSIMD[fNPV];

    for(int iP=0; iP<fNPV; iP++)
    {
      candTopo[iP] = mother;
      candTopo[iP].SetProductionVertex(PrimVtx[iP]);
    }
    
    for(int iv=0; iv<nEntries; iv++)
    {
      if(!saveParticle[iv]) continue;
      
      bool isPrimary = 0;
      for(int iP=0; iP<fNPV; iP++)
      {
        if( !(isFinite(candTopo[iP].GetChi2())[iv]) ) continue;
        if(!(candTopo[iP].GetChi2()[iv] > 0.0f)) continue;
        if(!(candTopo[iP].GetChi2()[iv]==candTopo[iP].GetChi2()[iv])) continue;
      
        if(float(candTopo[iP].GetChi2()[iv])/float(candTopo[iP].GetNDF()[iv]) <= cutChi2Topo )
          isPrimary = 1;
      }
      if(!isPrimary)
        continue;

      vCandidates[iC+iv].SetId(Particles.size());
      Particles.push_back(vCandidates[iC+iv]);
      
      float m, dm;
      vCandidates[iC+iv].GetMass(m,dm);
      if( (fabs(m - mass)/massErr) > massCut ) continue;

      vCandidates[iC+iv].SetNonlinearMassConstraint(mass);
      newCandidates.push_back(vCandidates[iC+iv]);
    }
    if(candTopo) delete [] candTopo;
  }
  
  vCandidates = newCandidates;
}

void KFParticleFinder::CombinePartPart(vector<KFParticle>& particles1,
                                       vector<KFParticle>& particles2,
                                       vector<KFParticle>& Particles,
                                       std::vector<KFParticleSIMD, KFPSimdAllocator<KFParticleSIMD> >& PrimVtx,
                                       const float* cuts,
                                       int iPV,
                                       const int MotherPDG,
                                       bool isSameInputPart,
                                       bool saveOnlyPrimary,
                                       vector< vector<KFParticle> >* vMotherPrim,
                                       vector<KFParticle>* vMotherSec,
                                       float massMotherPDG,
                                       float massMotherPDGSigma)
{
  /** Combines two already constructed candidates into a new particle.
   ** \param[in] particles1 - vector with the first set of particles.
   ** \param[in] particles2 - vector with the second set of particles.
   ** \param[out] Particles - output vector with particles.
   ** \param[in] PrimVtx - vector with primary vertices.
   ** \param[in] cuts - set of cuts: \f$l/\Delta l\f$, \f$\chi^2_{topo}\f$, \f$\chi^2_{geo}\f$.
   ** \param[in] iPV - index of the primary vertex for reconstruction of resonances. Tracks should come from the same vertex.
   ** \param[in] MotherPDG - PDG hypothesis of the constructed mother particles.
   ** \param[in] isSameInputPart - shows if vectors of input particles are the same to avoid double reconstruction of the same candidate.
   ** \param[in] saveOnlyPrimary - defines if only primary particles should be searched and \f$\chi^2_{topo}\f$ should be applied.
   ** \param[out] vMotherPrim - array with output primary candidates if any. If pointer is set to NULL - not filled.
   ** \param[out] vMotherSec - array with output secondary candidates if any. If pointer is set to NULL - not filled.
   ** \param[in] massMotherPDG - PDG table mass for the mother particle, is used for selection of primary and secondary candidates.
   ** \param[in] massMotherPDGSigma - sigma of the peak width, is used for selection of primary and secondary candidates.
   **/
  if( (particles1.size() ==  0) || (particles2.size() ==  0) ) return;  
  if(!(fDecayReconstructionList.empty()) && (fDecayReconstructionList.find(MotherPDG) == fDecayReconstructionList.end())) return;

  KFParticle mother_temp;
  KFParticleSIMD mother;
  KFParticleSIMD *motherTopo = new KFParticleSIMD[fNPV];
  mother.SetPDG( MotherPDG );

  kfvector_floatv l(fNPV), dl(fNPV);

  KFParticle* tmpPart2[SimdLen];
  int nPart2 = particles2.size();

  mask32_v isPrimary;
  if(iPV >= 0) isPrimary.setTrue();

  bool isCharm = (MotherPDG == 425) ||
                 (MotherPDG == 427) || 
                 (abs(MotherPDG) == 200411) ||
                 (abs(MotherPDG) == 404122) ||
                 (abs(MotherPDG) ==   4132) ||
                 (abs(MotherPDG) == 300431) ||
                 (abs(MotherPDG) == 204122);
  mask32_v isCharmMask;
  if(isCharm) isCharmMask.setTrue();
  
  for(unsigned int iP1=0; iP1 < particles1.size(); iP1++)
  {
    KFParticleSIMD vDaughters[2] = {KFParticleSIMD(particles1[iP1]), KFParticleSIMD()};

    unsigned int startIndex=0;
    if(isSameInputPart) startIndex=iP1+1;
    for(int iP2=startIndex; iP2 < nPart2; iP2 += SimdLen)
    {
      int nElements = (iP2 + SimdLen < nPart2) ? SimdLen : (nPart2 - iP2);
      mask32_v active((int32_v::indicesSequence() < int(nElements)));

      for(int iv=0; iv<nElements; iv++)
        tmpPart2[iv] = &particles2[iP2+iv];

      vDaughters[1] = KFParticleSIMD(tmpPart2,nElements);

//       if( reconstructPi0 )
//       {
//         int indexOffset = fEmcClusters->Id()[0];
//         int32_v gammaIndex1( (unsigned int)0);
//         int32_v gammaIndex2( (unsigned int)0);
//         for(int iv=0; iv<nElements; iv++)
//         {
//           gammaIndex1[iv] = Particles[ particles2[iP2+iv].DaughterIds()[0] ].DaughterIds()[0] - indexOffset;
//           gammaIndex2[iv] = Particles[ particles2[iP2+iv].DaughterIds()[1] ].DaughterIds()[0] - indexOffset;
//         }
//         
//         KFParticleSIMD gamma1(*fEmcClusters, gammaIndex1, vDaughters[0]);
//         KFParticleSIMD gamma2(*fEmcClusters, gammaIndex2, vDaughters[0]);
//         const KFParticleSIMD* pi0Daughters[2] = {&gamma1, &gamma2};
//         
//         int32_v gammaId = vDaughters[1].Id();
//         vDaughters[1].SetVtxGuess(vDaughters[0].X(), vDaughters[0].Y(), vDaughters[0].Z());
//         vDaughters[1].Construct(pi0Daughters, 2);
//         vDaughters[1].SetId(gammaId);
//         
//         float32_v mass, dm;
//         vDaughters[1].GetMass(mass,dm);
//         const float& mPi0 = KFParticleDatabase::Instance()->GetPi0Mass();
//         const float& mPi0Sigma = KFParticleDatabase::Instance()->GetPi0MassSigma();
//         active &= (abs(mass - mPi0)/mPi0Sigma) < 3.f;
//         vDaughters[1].SetNonlinearMassConstraint(mPi0);
//         if(active.isEmpty()) continue;
//       }
      
      if(isCharm)
      {
        mother  = vDaughters[0];
        mother += vDaughters[1];
        mother.SetPDG( MotherPDG );
      }
      else
      {
        const KFParticleSIMD* vDaughtersPointer[2] = {&vDaughters[0], &vDaughters[1]};
        mother.Construct(vDaughtersPointer, 2, 0);
      }
  
      mask32_v saveParticle(active);
      saveParticle &= (mother.Chi2()/toFloat(mother.NDF()) < cuts[2] );
      saveParticle &= isFinite(mother.GetChi2());
      saveParticle &= (mother.GetChi2() >= 0.0f);
      saveParticle &= (mother.GetChi2() == mother.GetChi2());
      
      if( saveParticle.isEmpty() ) { continue; }

      mask32_v isSameTrack;
      for(unsigned int iD=0; iD<vDaughters[0].DaughterIds().size(); iD++)
        for(unsigned int iD1=0; iD1<vDaughters[1].DaughterIds().size(); iD1++)
          isSameTrack |= ( vDaughters[0].DaughterIds()[iD] == vDaughters[1].DaughterIds()[iD1] );
      saveParticle &= ( !isSameTrack );
      if( saveParticle.isEmpty() ) { continue; }        
      
      float32_v lMin(1.e8f);
      float32_v ldlMin(1.e8f);
      mask32_v isParticleFromVertex;

      for(int iP=0; iP<fNPV; iP++)
      {
        if( (iPV > -1) && (iP !=iPV) ) continue;
        mask32_v isParticleFromVertexLocal;
        mother.GetDistanceToVertexLine(PrimVtx[iP], l[iP], dl[iP], &isParticleFromVertexLocal);
        isParticleFromVertex |= isParticleFromVertexLocal;
        float32_v ldl = (l[iP]/dl[iP]);
        lMin = select( (l[iP] < lMin) && active, l[iP], lMin);
        ldlMin = select( (ldl < ldlMin) && active, ldl, ldlMin);
      }
      saveParticle &= ( ((!isPrimary) && ldlMin > cuts[0]) || isPrimary );
      saveParticle &= (lMin < 200.f);
    
      mask32_v setLCut = abs(mother.PDG()) == 3000;
      saveParticle &= ( (setLCut && lMin > float32_v(fLCut)) || (!setLCut) );

//         if(isPrimary && (float(ldlMin > 3) )) continue;
      saveParticle &= (((!isPrimary) && isParticleFromVertex) || isPrimary );
      if( saveParticle.isEmpty() ) { continue; }

      mask32_v isSameParticle(isPrimary || isCharmMask);
      if(!((isSameParticle).isFull()))
      {
        mask32_v isParticleFromVertexLocal;
        float32_v l1, dl1;
        vDaughters[0].GetDistanceToVertexLine(mother, l1, dl1, &isParticleFromVertexLocal);
        
        saveParticle &= ( isSameParticle || ((!isSameParticle) && isParticleFromVertexLocal));
        if( saveParticle.isEmpty() ) { continue; }
      }
  
      for(int iP=0; iP<fNPV; iP++)
      {
        if( (iPV > -1) && (iP !=iPV) ) continue;
        motherTopo[iP] = mother;
        motherTopo[iP].SetProductionVertex(PrimVtx[iP]);
      }
  
      vector<int> iPrimVert[SimdLen];
      mask32_v isPrimaryPart;

      for(int iP=0; iP<fNPV; iP++)
      {
        if( (iPV > -1) && (iP !=iPV) ) continue;
        const float32_v& motherTopoChi2Ndf = motherTopo[iP].GetChi2()/toFloat(motherTopo[iP].GetNDF());
        const mask32_v isPrimaryPartLocal = ( motherTopoChi2Ndf < float32_v(cuts[1]) );
        isPrimaryPart |= isPrimaryPartLocal;
        for(int iV=0; iV<SimdLen; iV++)
        {
          if(isPrimaryPartLocal[iV])
            iPrimVert[iV].push_back(iP);
        }
      }
      
      for(int iv=0; iv<nElements; iv++)
      {
        if(!saveParticle[iv]) continue; 
        
        mother.GetKFParticle(mother_temp, iv);

        // reset daughter ids for 3- and 4-particle decays
        if( (abs(mother.PDG()[iv]) == 428))
        {
          mother_temp.CleanDaughtersId();
          for(int iD=0; iD < particles1[iP1].NDaughters(); iD++)
            mother_temp.AddDaughterId( particles1[iP1].DaughterIds()[iD] );
          mother_temp.AddDaughterId(tmpPart2[iv]->Id());
        }
        
        if(saveOnlyPrimary)
        {
          if(isPrimaryPart[iv])
          {
            mother_temp.SetId(Particles.size());
            Particles.push_back(mother_temp);
          }
        }
        else
        {
          mother_temp.SetId(Particles.size());
          Particles.push_back(mother_temp);
        }
        
        if(vMotherPrim || vMotherSec)
        {
          float mass, errMass;
          mother_temp.GetMass(mass, errMass);
          if( (fabs(mass - massMotherPDG)/massMotherPDGSigma) > 3.f ) continue;
          mother_temp.SetNonlinearMassConstraint(massMotherPDG);
          
          if(MotherPDG == 428)
          {
            mother_temp.CleanDaughtersId();
            for(int iD=0; iD < tmpPart2[iv]->NDaughters(); iD++)
              mother_temp.AddDaughterId( tmpPart2[iv]->DaughterIds()[iD] );
            for(int iD=0; iD < particles1[iP1].NDaughters(); iD++)
              mother_temp.AddDaughterId( particles1[iP1].DaughterIds()[iD] );            
          }
          
          if(vMotherSec && (!(isPrimaryPart[iv])) )
            vMotherSec->push_back(mother_temp);
          if(vMotherPrim)
            for(unsigned int iP=0; iP<iPrimVert[iv].size(); iP++)
              (*vMotherPrim)[iPrimVert[iv][iP]].push_back(mother_temp);
        }
      }
    }
  }
  
  if(motherTopo) delete [] motherTopo;
}

void KFParticleFinder::MatchKaons(KFPTrackVector* vTracks, 
                                  std::vector<KFParticleSIMD, KFPSimdAllocator<KFParticleSIMD> >& PrimVtx,
                                  std::vector<KFParticle>& Particles)
{
  constexpr const int nKaonSets=2;
  constexpr const int primCandidatesSet[nKaonSets]{11,12};
  constexpr const int trackSet[nKaonSets]{6,7}; // primary "+" and "-" tracks at last hit
  constexpr const int trackPdg[nKaonSets]{321, -321};
  constexpr const int newPdg[nKaonSets]{200321, -200321};
  
  KFParticleSIMD kaonTrack;

  for(int iKaonSet=0; iKaonSet<nKaonSets; iKaonSet++) {
    if(!(fDecayReconstructionList.empty()) && (fDecayReconstructionList.find(newPdg[iKaonSet]) == fDecayReconstructionList.end())) continue;
    
    std::vector< std::vector<KFParticle> >& candidateSets = fPrimCandidates[primCandidatesSet[iKaonSet]];
    KFPTrackVector& primTracks = vTracks[trackSet[iKaonSet]];
    const int firstTrack = primTracks.FirstKaon();
    const int lastTrack = primTracks.LastKaon();
    
    for(unsigned int iPV=0; iPV < candidateSets.size(); iPV++) {
      std::vector<KFParticle>& candidates = candidateSets[iPV];

      for(unsigned int iCandidate=0; iCandidate<candidates.size(); iCandidate++) {
        KFParticleSIMD candidate(candidates[iCandidate]);
        
        for(int iTrack=firstTrack; iTrack<lastTrack; iTrack+=SimdLen) {
          const int NTracks = (iTrack + SimdLen < lastTrack) ? SimdLen : (lastTrack - iTrack);
          const int32_v& trackPDG = reinterpret_cast<const int32_v&>(primTracks.PDG()[iTrack]);
          const int32_v& trackPVIndex = reinterpret_cast<const  int32_v&>(primTracks.PVIndex()[iTrack]);
          const mask32_v& isSamePV = (iPV == trackPVIndex);

          mask32_v active = (abs(trackPDG)==321) && isSamePV && (int32_v::indicesSequence() < int(NTracks));
            
          if(active.isEmpty()) continue;
          
          kaonTrack.Load(primTracks, iTrack, trackPdg[iTrack]);
          
          float32_v dx = candidate.X() - kaonTrack.X();
          float32_v dy = candidate.Y() - kaonTrack.Y();
          float32_v dz = candidate.Z() - kaonTrack.Z();
          float32_v distance = sqrt(dx*dx + dy*dy + dz*dz);
          active &= (distance <= float32_v(20.0f));
          if( active.isEmpty() ) continue;

          //Chi2 should be correct, momentum should be 0 within errors
          KFParticleSIMD check = candidate;
          check.SubtractDaughter(kaonTrack);
          
          active &= (check.NDF() >= 0);
          active &= (check.Chi2()/toFloat(check.NDF()) <= fCuts2D[1]);
          //fit should converge
          active &= (check.Chi2() >= 0.f);
          active &= (check.Chi2() == check.Chi2());
          //momentum shoud be 0 after subtraction
          active &= (check.Px() <= 5.f*check.GetErrPx());
          active &= (check.Py() <= 5.f*check.GetErrPy());
          active &= (check.Pz() <= 5.f*check.GetErrPz());
          if( active.isEmpty() ) continue;

          for(int iV=0; iV<NTracks; iV++) {
            if(!active[iV]) continue;
            KFParticle mother_temp = candidates[iCandidate];
            mother_temp.AddDaughterId(primTracks.Id()[iTrack+iV]);
            mother_temp.SetId(Particles.size());
            mother_temp.SetPDG(newPdg[iKaonSet]);
            Particles.push_back(mother_temp);
          }
        }
        
      }
    }
  }
}

#if 0 //old method
void KFParticleFinder::NeutralDaughterDecay(KFPTrackVector* vTracks, vector<KFParticle>& Particles,
                                            std::vector<KFParticleSIMD, KFPSimdAllocator<KFParticleSIMD> >& PrimVtx)
{
  /** Reconstructs particles by the missing mass method.
   ** \param[in] vRTracks - pointer to the array with vectors of tracks:\n
   ** 0) secondary positive at the first hit position; \n
   ** 1) secondary negative at the first hit position; \n
   ** 2) primary positive at the first hit position; \n
   ** 3) primary negative at the first hit position; \n
   ** 4) secondary positive at the last hit position; \n
   ** 5) secondary negative at the last hit position; \n
   ** 6) primary positive at the last hit position; \n
   ** 7) primary negative at the last hit position. \n
   ** \param[out] Particles - the output array with the reconstructed particle-candidates.
   **/
  KFParticle mother_temp;
  KFParticleSIMD ChargedDaughter, MotherTrack;

  int32_v idMotherTrack;
  int32_v idChargedDaughter;
  int32_v ChargedDaughterPDG(-1);
    
  int32_v pvIndexMother(-1); 
  
  int outNeutralDaughterPDG[4][5]; //[iTC][iHypothesis]
  int outMotherPDG[4][5];
  
  int trTypeIndexMother[2] = {6,7};
  int trTypeIndexDaughter[2] = {0,1};

  for( int iTrTypeDaughter = 0; iTrTypeDaughter<2; iTrTypeDaughter++)
  {
    KFPTrackVector& DaughterTracks = vTracks[ trTypeIndexDaughter[iTrTypeDaughter] ];
    KFPTrackVector& MotherTracks = vTracks[ trTypeIndexMother[iTrTypeDaughter] ];

    int32_v DaughterTracksSize = DaughterTracks.Size();
    int MotherTracksSize = MotherTracks.Size();

    //track categories
    int nTC = 4;
    int startTCMother[4] = {0,0,0,0};
    int endTCMother[4] = {0,0,0,0};
    int startTCDaughter[4] = {0,0,0,0};
    int endTCDaughter[4] = {0,0,0,0};

    nTC = 4;
    vector<int> nMotherHypothesis(nTC,0);
    vector< vector<int> > motherPDGHypothesis(nTC);
    vector< vector<float> > neutralDaughterMassHypothesis(nTC);


    //mu+, mu-
    startTCMother[0] = 0; endTCMother[0] = MotherTracksSize;
    startTCDaughter[0] = DaughterTracks.FirstMuon(); endTCDaughter[0] = DaughterTracks.LastPion(); 

    nMotherHypothesis[0] = 2;


    motherPDGHypothesis[0].push_back(211);
    motherPDGHypothesis[0].push_back(321);

    neutralDaughterMassHypothesis[0].push_back(0.);
    neutralDaughterMassHypothesis[0].push_back(0.);

    outNeutralDaughterPDG[0][0]=-7000014;
    outNeutralDaughterPDG[0][1]=-8000014;

    outMotherPDG[0][0]=-7000211;
    outMotherPDG[0][1]=-7000321;

    //Pi+, Pi-
    startTCMother[1] = 0; endTCMother[1] = MotherTracksSize;
    startTCDaughter[1] = DaughterTracks.FirstPion(); endTCDaughter[1] = DaughterTracks.LastPion();

    nMotherHypothesis[1] = 5;

    motherPDGHypothesis[1].push_back(3112);
    motherPDGHypothesis[1].push_back(3222);
    motherPDGHypothesis[1].push_back(3312);
    motherPDGHypothesis[1].push_back(3334);
    motherPDGHypothesis[1].push_back(321);
    
    neutralDaughterMassHypothesis[1].push_back(0.939565);
    neutralDaughterMassHypothesis[1].push_back(0.939565);
    neutralDaughterMassHypothesis[1].push_back(1.115683);
    neutralDaughterMassHypothesis[1].push_back(1.31486);
    neutralDaughterMassHypothesis[1].push_back(0.1349766);
    
    outNeutralDaughterPDG[1][0]= 7002112;
    outNeutralDaughterPDG[1][1]=-8002112;
    outNeutralDaughterPDG[1][2]= 7003122;
    outNeutralDaughterPDG[1][3]= 7003322;
    outNeutralDaughterPDG[1][4]=-9000111;
    
    outMotherPDG[1][0]= 7003112;
    outMotherPDG[1][1]=-7003222;
    outMotherPDG[1][2]= 7003312;
    outMotherPDG[1][3]= 7003334;
    outMotherPDG[1][4]=-9000321;
    
    //K+, K-
    startTCMother[2] = 0; endTCMother[2] = MotherTracksSize;
    startTCDaughter[2] = DaughterTracks.FirstKaon(); endTCDaughter[2] = DaughterTracks.LastKaon();

    nMotherHypothesis[2] = 1;
    
    motherPDGHypothesis[2].push_back(3334);
    
    neutralDaughterMassHypothesis[2].push_back(1.115683);
    
    outNeutralDaughterPDG[2][0]= 8003122;
    
    outMotherPDG[2][0]= 8003334;

    //p+, p-
    startTCMother[3] = 0; endTCMother[3] = MotherTracksSize;
    startTCDaughter[3] = DaughterTracks.FirstProton(); endTCDaughter[3] = DaughterTracks.LastProton(); 

    nMotherHypothesis[3] = 1;
    
    motherPDGHypothesis[3].push_back(3222);
    
    neutralDaughterMassHypothesis[3].push_back(0.1349766);
    
    outNeutralDaughterPDG[3][0]=-8000111;
    
    outMotherPDG[3][0]=-8003222;
    
    

    for(int iTC=0; iTC<nTC; iTC++)
    {
      for(unsigned short iTrD=startTCDaughter[iTC]; iTrD < endTCDaughter[iTC]; iTrD += SimdLen)
      {
        const unsigned short NTracksDaughter = (iTrD + SimdLen < DaughterTracks.Size()) ? SimdLen : (DaughterTracks.Size() - iTrD);

        int32_v DaughterInd = int32_v::indicesSequence(iTrD);

        int32_v DaughterPDG = reinterpret_cast<const int32_v&>(DaughterTracks.PDG()[iTrD]);
        int32_v DaughterPVIndex = reinterpret_cast<const int32_v&>(DaughterTracks.PVIndex()[iTrD]);
        int32_v daughterId = reinterpret_cast<const int32_v&>(DaughterTracks.Id()[iTrD]);
        
        mask32_v activeDaughter = (int32_v::indicesSequence() < int(NTracksDaughter));
            
        ChargedDaughter.Load(DaughterTracks, iTrD, DaughterPDG);
        ChargedDaughter.SetId(daughterId);

        for(unsigned short iTrM=startTCMother[iTC]; iTrM < endTCMother[iTC]; iTrM += SimdLen)
        {
          const unsigned short NTracks = (iTrM + SimdLen < MotherTracksSize) ? SimdLen : (MotherTracksSize - iTrM);

          const int32_v& MotherPDG = reinterpret_cast<const int32_v&>(MotherTracks.PDG()[iTrM]);
          //const int32_v& MotherPVIndex = reinterpret_cast<const  int32_v&>(MotherTracks.PVIndex()[iTrM]);              
          const int32_v& motherTrackId = reinterpret_cast<const  int32_v&>(MotherTracks.Id()[iTrM]);
          
          for(int iRot = 0; iRot<SimdLen; iRot++)
          {
            if(iRot>0)
            {
              DaughterPDG = DaughterPDG.rotate<1>();
              DaughterPVIndex = DaughterPVIndex.rotate<1>();
              DaughterInd = DaughterInd.rotate<1>();
            
              ChargedDaughter.Rotate();

              activeDaughter = /*( (DaughterPDG != -1) || ( (DaughterPVIndex < 0) && (DaughterPDG == -1) ) ) &&*/ (DaughterInd < DaughterTracksSize);
            }
            
            int32_v trackPdgMother;

            if(iTC==0)
              activeDaughter &= abs(DaughterPDG)==13;
            if(iTC==1)
              activeDaughter &= abs(DaughterPDG)==211;
            if(iTC==2)
              activeDaughter &= abs(DaughterPDG)==321;
            if(iTC==3)
              activeDaughter &= abs(DaughterPDG)==2212;
            if (activeDaughter.isEmpty()) continue;
            
            
            for(int iHypothesis=0; iHypothesis<nMotherHypothesis[iTC]; iHypothesis++)
            {
              int motherKFPDG = outMotherPDG[iTC][iHypothesis];
              if(iTrTypeDaughter==0) motherKFPDG = -outMotherPDG[iTC][iHypothesis];
              if(!(fDecayReconstructionList.empty()) && (fDecayReconstructionList.find(motherKFPDG) == fDecayReconstructionList.end())) continue;
              
              mask32_v active = activeDaughter && (int32_v::indicesSequence() < int(NTracks));

              if(abs(motherPDGHypothesis[iTC][iHypothesis]) < 1000)
                active &= (abs(MotherPDG)==abs(motherPDGHypothesis[iTC][iHypothesis]));
              else
                active &= (abs(MotherPDG)==2000003112);

              MotherTrack.Load(MotherTracks, iTrM, motherPDGHypothesis[iTC][iHypothesis]);
              
              float32_v zMother = MotherTrack.Z();
              float32_v zCD = ChargedDaughter.Z();
              float32_v xMother = MotherTrack.X();
              float32_v xCD = ChargedDaughter.X();
              float32_v yMother = MotherTrack.Y();
              float32_v yCD = ChargedDaughter.Y();
              float32_v distance = sqrt((zMother-zCD)*(zMother-zCD)+(xMother-xCD)*(xMother-xCD)+(yMother-yCD)*(yMother-yCD));
              float32_v lMotherTrak, dlMotherTrak;
              MotherTrack.GetDistanceToVertexLine(PrimVtx[0], lMotherTrak, dlMotherTrak);
              float32_v lChargedTrak, dChargedTrak;
              ChargedDaughter.GetDistanceToVertexLine(PrimVtx[0], lChargedTrak, dChargedTrak);

              //daughter particle should start after the last hit of a mother track
//               active &= (lChargedTrak >= (lMotherTrak - float32_v(0.5f)));
              active &= (distance <= float32_v(10.0f));
              if( active.isEmpty() ) continue;
              
              KFParticleSIMD neutralDaughter = MotherTrack;
              //energy of the mother particle should be greater then of the daughter particle
              active &= (neutralDaughter.E() > ChargedDaughter.E());
              if( active.isEmpty() ) continue;
              
              neutralDaughter.AddDaughterId(motherTrackId);
              neutralDaughter.NDF() = -1;
              neutralDaughter.Chi2() = 0.f;
              neutralDaughter.SubtractDaughter(ChargedDaughter);
              
              float32_v neutralMass, neutralMassError;
              neutralDaughter.GetMass(neutralMass, neutralMassError);
              if(iTC > 0)
                active &= (neutralMass > 0);

              //decay point shoud be between mother and daughter tracks
              //TODO all PV should be checked
              float32_v lNeutral, dlNeutral;
              neutralDaughter.GetDistanceToVertexLine(PrimVtx[0], lNeutral, dlNeutral);

              active &= (lNeutral >= (lMotherTrak - float32_v(10.0f)));
              active &= (lNeutral <= (lChargedTrak + float32_v(10.0f)));
              //set cut on chi2 of the fit of the neutral daughter
              active &= (neutralDaughter.NDF() >= 0);
              active &= (neutralDaughter.Chi2()/toFloat(neutralDaughter.NDF()) <= fCuts2D[1]);
              //fit should converge
              active &= (neutralDaughter.Chi2() >= 0.f);
              active &= (neutralDaughter.Chi2() == neutralDaughter.Chi2());
              if( active.isEmpty() ) continue;
              
              //kill particle-candidates produced by clones
              active &= ( neutralDaughter.GetRapidity()<6.f /*&& neutralDaughter.GetRapidity()>0.f*/);
              if ((iTC==1 && iHypothesis<4) || iTC==2)
                active &= ( !( (neutralDaughter.GetPt())<0.5f && neutralDaughter.GetRapidity()<0.5f ) );
              if (iTC==3)
                active &= ( !( (neutralDaughter.GetPt())<0.2f && neutralDaughter.GetRapidity()<1.f ) );
              if( active.isEmpty() ) continue;
              
              KFParticleSIMD neutralDaughterUnconstr = neutralDaughter;
              neutralDaughter.SetNonlinearMassConstraint(neutralDaughterMassHypothesis[iTC][iHypothesis]);
              
              const KFParticleSIMD* daughters[2] = {&neutralDaughter, &ChargedDaughter};
              KFParticleSIMD mother;
              mother.Construct(daughters, 2);
              
              //decay point shoud be between mother and daughter tracks
              float32_v lMother, dlMother;
              mother.GetDistanceToVertexLine(PrimVtx[0], lMother, dlMother);

              active &= (lMother >= lMotherTrak);
              active &= (lMother <= lChargedTrak);
              //set cut on chi2 of the fit of the mother particle
              active &= (mother.NDF() >= 0);
              active &= (mother.Chi2()/toFloat(mother.NDF()) <= fCuts2D[1]);
              //fit should converge
              active &= (mother.Chi2() >= 0.f);
              active &= (mother.Chi2() == mother.Chi2());
              if( active.isEmpty() ) continue;

              for(int iV=0; iV<NTracks; iV++)
              {
                if(!active[iV]) continue;
                
                neutralDaughterUnconstr.GetKFParticle(mother_temp, iV);
                int neutralId = Particles.size();
                mother_temp.SetId(neutralId);
                if (iTrTypeDaughter==0)
                  mother_temp.SetPDG(-outNeutralDaughterPDG[iTC][iHypothesis]);
                else
                  mother_temp.SetPDG(outNeutralDaughterPDG[iTC][iHypothesis]);
                Particles.push_back(mother_temp);

                mother.GetKFParticle(mother_temp, iV);
                mother_temp.SetId(Particles.size());
                mother_temp.CleanDaughtersId();
                mother_temp.AddDaughterId(ChargedDaughter.Id()[iV]);
                mother_temp.AddDaughterId(neutralId);
                
                if (iTrTypeDaughter==0)  
                  mother_temp.SetPDG(-outMotherPDG[iTC][iHypothesis]);
                else
                  mother_temp.SetPDG(outMotherPDG[iTC][iHypothesis]);
                Particles.push_back(mother_temp);
              }
            }
          }//iRot
        }//iTrM
      }//iTrD
    }//iTC
  }//iTrTypeDaughter
}
#else // new method
void KFParticleFinder::NeutralDaughterDecay(KFPTrackVector* vTracks, vector<KFParticle>& Particles,
                                            std::vector<KFParticleSIMD, KFPSimdAllocator<KFParticleSIMD> >& PrimVtx)
{
  /** Reconstructs particles by the missing mass method.
   ** \param[in] vRTracks - pointer to the array with vectors of tracks:\n
   ** 0) secondary positive at the first hit position; \n
   ** 1) secondary negative at the first hit position; \n
   ** 2) primary positive at the first hit position; \n
   ** 3) primary negative at the first hit position; \n
   ** 4) secondary positive at the last hit position; \n
   ** 5) secondary negative at the last hit position; \n
   ** 6) primary positive at the last hit position; \n
   ** 7) primary negative at the last hit position. \n
   ** \param[out] Particles - the output array with the reconstructed particle-candidates.
   **/
  KFParticle mother_temp;
  KFParticleSIMD ChargedDaughter, MotherTrack, MotherFiltered, cDaughterFiltered;

  int32_v idMotherTrack;
  int32_v idChargedDaughter;
  int32_v ChargedDaughterPDG(-1);
    
  int32_v pvIndexMother(-1); 
  
  int outNeutralDaughterPDG[6][5]; //[iTC][iHypothesis]
  int outMotherPDG[6][5];
  
  int trTypeIndexMother[2] = {6,7};
  int trTypeIndexDaughter[2] = {0,1};

  for( int iTrTypeDaughter = 0; iTrTypeDaughter<2; iTrTypeDaughter++)
  {
    KFPTrackVector& DaughterTracks = vTracks[ trTypeIndexDaughter[iTrTypeDaughter] ];
    KFPTrackVector& MotherTracks = vTracks[ trTypeIndexMother[iTrTypeDaughter] ];

    int32_v DaughterTracksSize = DaughterTracks.Size();
    int MotherTracksSize = MotherTracks.Size();

    //track categories
    int nTC = 6;
    int startTCMother[6] = {0,0,0,0,0,0};
    int endTCMother[6] = {0,0,0,0,0,0};
    int startTCDaughter[6] = {0,0,0,0,0,0};
    int endTCDaughter[6] = {0,0,0,0,0,0};

    vector<int> nMotherHypothesis(nTC,0);
    vector< vector<int> > motherPDGHypothesis(nTC);
    vector< vector<float> > neutralDaughterMassHypothesis(nTC);


    //mu+, mu-
    startTCMother[0] = 0; endTCMother[0] = MotherTracksSize;
    startTCDaughter[0] = DaughterTracks.FirstMuon(); endTCDaughter[0] = DaughterTracks.LastMuon(); 

    nMotherHypothesis[0] = 2;

    motherPDGHypothesis[0].push_back(211);
    motherPDGHypothesis[0].push_back(321);

    neutralDaughterMassHypothesis[0].push_back(0.);
    neutralDaughterMassHypothesis[0].push_back(0.);

    outNeutralDaughterPDG[0][0]=-7000014;
    outNeutralDaughterPDG[0][1]=-8000014;

    outMotherPDG[0][0]=-7000211;
    outMotherPDG[0][1]=-7000321;

    //Pi+, Pi-
    startTCMother[1] = 0; endTCMother[1] = MotherTracksSize;
    startTCDaughter[1] = DaughterTracks.FirstPion(); endTCDaughter[1] = DaughterTracks.LastPion();

    nMotherHypothesis[1] = 5;

    motherPDGHypothesis[1].push_back(3112);
    motherPDGHypothesis[1].push_back(3222);
    motherPDGHypothesis[1].push_back(3312);
    motherPDGHypothesis[1].push_back(3334);
    motherPDGHypothesis[1].push_back(321);

    neutralDaughterMassHypothesis[1].push_back(0.939565);
    neutralDaughterMassHypothesis[1].push_back(0.939565);
    neutralDaughterMassHypothesis[1].push_back(1.115683);
    neutralDaughterMassHypothesis[1].push_back(1.31486);
    neutralDaughterMassHypothesis[1].push_back(0.1349766);
    
    outNeutralDaughterPDG[1][0]= 7002112;
    outNeutralDaughterPDG[1][1]=-8002112;
    outNeutralDaughterPDG[1][2]= 7003122;
    outNeutralDaughterPDG[1][3]= 7003322;
    outNeutralDaughterPDG[1][4]=-9000111;
    
    outMotherPDG[1][0]= 7003112;
    outMotherPDG[1][1]=-7003222;
    outMotherPDG[1][2]= 7003312;
    outMotherPDG[1][3]= 7003334;
    outMotherPDG[1][4]=-9000321;
    
    //K+, K-
    startTCMother[2] = 0; endTCMother[2] = MotherTracksSize;
    startTCDaughter[2] = DaughterTracks.FirstKaon(); endTCDaughter[2] = DaughterTracks.LastKaon();

    nMotherHypothesis[2] = 1;
    
    motherPDGHypothesis[2].push_back(3334);

    neutralDaughterMassHypothesis[2].push_back(1.115683);
    
    outNeutralDaughterPDG[2][0]= 8003122;
    
    outMotherPDG[2][0]= 8003334;

    //p+, p-
    startTCMother[3] = 0; endTCMother[3] = MotherTracksSize;
    startTCDaughter[3] = DaughterTracks.FirstProton(); endTCDaughter[3] = DaughterTracks.LastProton(); 

    nMotherHypothesis[3] = 1;
    
    motherPDGHypothesis[3].push_back(3222);

    neutralDaughterMassHypothesis[3].push_back(0.1349766);
    
    outNeutralDaughterPDG[3][0]=-8000111;
    
    outMotherPDG[3][0]=-8003222;
    
    //He3
    startTCMother[4] = 0; endTCMother[4] = MotherTracksSize;
    startTCDaughter[4] = DaughterTracks.FirstHe3(); endTCDaughter[4] = DaughterTracks.LastHe3();

    nMotherHypothesis[4] = 1;

    motherPDGHypothesis[4].push_back(7003029);

    neutralDaughterMassHypothesis[4].push_back(0.1349766);
    
    outNeutralDaughterPDG[4][0]= 7700111;
    
    outMotherPDG[4][0]= 7003029;

    //He4
    startTCMother[5] = 0; endTCMother[5] = MotherTracksSize;
    startTCDaughter[5] = DaughterTracks.FirstHe4(); endTCDaughter[5] = DaughterTracks.LastHe4();

    nMotherHypothesis[5] = 1;

    motherPDGHypothesis[5].push_back(7003006);

    neutralDaughterMassHypothesis[5].push_back(0.1349766);
    
    outNeutralDaughterPDG[5][0]= 7800111;
    
    outMotherPDG[5][0]= 7003006;

    for(int iTC=0; iTC<nTC; iTC++)
    {
      for(unsigned short iTrD=startTCDaughter[iTC]; iTrD < endTCDaughter[iTC]; iTrD += SimdLen)
      {
        const unsigned short NTracksDaughter = (iTrD + SimdLen < DaughterTracks.Size()) ? SimdLen : (DaughterTracks.Size() - iTrD);

        int32_v DaughterInd = int32_v::indicesSequence(iTrD);

        int32_v DaughterPDG = reinterpret_cast<const int32_v&>(DaughterTracks.PDG()[iTrD]);
        int32_v DaughterPVIndex = reinterpret_cast<const int32_v&>(DaughterTracks.PVIndex()[iTrD]);
        int32_v daughterId = reinterpret_cast<const int32_v&>(DaughterTracks.Id()[iTrD]);
        
        mask32_v activeDaughter = (int32_v::indicesSequence() < int(NTracksDaughter));
            
        ChargedDaughter.Load(DaughterTracks, iTrD, DaughterPDG);
        ChargedDaughter.SetId(daughterId);

        for(unsigned short iTrM=startTCMother[iTC]; iTrM < endTCMother[iTC]; iTrM += SimdLen)
        {
          const unsigned short NTracks = (iTrM + SimdLen < MotherTracksSize) ? SimdLen : (MotherTracksSize - iTrM);

          const int32_v& MotherPDG = reinterpret_cast<const int32_v&>(MotherTracks.PDG()[iTrM]);

          //const int32_v& MotherPVIndex = reinterpret_cast<const  int32_v&>(MotherTracks.PVIndex()[iTrM]);              
          const int32_v& motherTrackId = reinterpret_cast<const  int32_v&>(MotherTracks.Id()[iTrM]);
          
          for(int iRot = 0; iRot<SimdLen; iRot++)
          {
            if(iRot>0)
            {
              DaughterPDG = DaughterPDG.rotate<1>();
              DaughterPVIndex = DaughterPVIndex.rotate<1>();
              DaughterInd = DaughterInd.rotate<1>();
            
              ChargedDaughter.Rotate();

              activeDaughter = /*( (DaughterPDG != -1) || ( (DaughterPVIndex < 0) && (DaughterPDG == -1) ) ) &&*/ (DaughterInd < DaughterTracksSize);
            }
            
            int32_v trackPdgMother;

            if(iTC==0)
              activeDaughter &= abs(DaughterPDG)==13;
            if(iTC==1)
              activeDaughter &= abs(DaughterPDG)==211;
            if(iTC==2)
              activeDaughter &= abs(DaughterPDG)==321;
            if(iTC==3)
              activeDaughter &= abs(DaughterPDG)==2212;
            if(iTC==4)
              activeDaughter &= abs(DaughterPDG)==1000020030;
            if(iTC==5)
              activeDaughter &= abs(DaughterPDG)==1000020040;
            if (activeDaughter.isEmpty()) continue;
            
            
            for(int iHypothesis=0; iHypothesis<nMotherHypothesis[iTC]; iHypothesis++)
            {
              int motherKFPDG = outMotherPDG[iTC][iHypothesis];
              if(iTrTypeDaughter==0) motherKFPDG = -outMotherPDG[iTC][iHypothesis];
              if(!(fDecayReconstructionList.empty()) && (fDecayReconstructionList.find(motherKFPDG) == fDecayReconstructionList.end())) continue;
              
              mask32_v active = activeDaughter && (int32_v::indicesSequence() < int(NTracks));
                             
              if(abs(motherPDGHypothesis[iTC][iHypothesis]) < 1000)
                active &= (abs(MotherPDG)==abs(motherPDGHypothesis[iTC][iHypothesis]));
              else if(abs(motherPDGHypothesis[iTC][iHypothesis]) < 10000)
                active &= (abs(MotherPDG)==2000003112);
              else
                active &= (abs(MotherPDG)>=1000020030);
              
              MotherTrack.Load(MotherTracks, iTrM, motherPDGHypothesis[iTC][iHypothesis]);
              
              float32_v zMother = MotherTrack.Z();
              float32_v zCD = ChargedDaughter.Z();
              float32_v xMother = MotherTrack.X();
              float32_v xCD = ChargedDaughter.X();
              float32_v yMother = MotherTrack.Y();
              float32_v yCD = ChargedDaughter.Y();
              float32_v distance = sqrt((zMother-zCD)*(zMother-zCD)+(xMother-xCD)*(xMother-xCD)+(yMother-yCD)*(yMother-yCD));
              float32_v lMotherTrak, dlMotherTrak;
              MotherTrack.GetDistanceToVertexLine(PrimVtx[0], lMotherTrak, dlMotherTrak);
              float32_v lChargedTrak, dChargedTrak;
              ChargedDaughter.GetDistanceToVertexLine(PrimVtx[0], lChargedTrak, dChargedTrak);

              //daughter particle should start after the last hit of a mother track
//               active &= (lChargedTrak >= (lMotherTrak - float32_v(0.5f)));
              active &= (distance <= float32_v(10.0f));
              if( active.isEmpty() ) continue;
              
              KFParticleSIMD neutralDaughter = MotherTrack;
              //energy of the mother particle should be greater then of the daughter particle
              active &= (neutralDaughter.E() > ChargedDaughter.E());
              if( active.isEmpty() ) continue;
              
              neutralDaughter.AddDaughterId(motherTrackId);
              neutralDaughter.NDF() = -1;
              neutralDaughter.Chi2() = 0.f;
              neutralDaughter.ReconstructMissingMass(ChargedDaughter, MotherFiltered, cDaughterFiltered, neutralDaughterMassHypothesis[iTC][iHypothesis]);
              
              float32_v neutralMass, neutralMassError;
              neutralDaughter.GetMass(neutralMass, neutralMassError);
              if(iTC > 0)
                active &= (neutralMass > 0.f);

              //decay point shoud be between mother and daughter tracks
              //TODO all PV should be checked
              float32_v lNeutral, dlNeutral;
              neutralDaughter.GetDistanceToVertexLine(PrimVtx[0], lNeutral, dlNeutral);

              active &= (lNeutral >= (lMotherTrak - float32_v(10.0f)));
              active &= (lNeutral <= (lChargedTrak + float32_v(10.0f)));

              //set cut on chi2 of the fit of the neutral daughter
              active &= (neutralDaughter.NDF() >= 0);
              active &= (neutralDaughter.Chi2()/toFloat(neutralDaughter.NDF()) <= fCuts2D[1]);
              //fit should converge
              active &= (neutralDaughter.Chi2() >= 0.f);
              active &= (neutralDaughter.Chi2() == neutralDaughter.Chi2());

              if(abs(motherPDGHypothesis[iTC][iHypothesis]) > 1000)
              {
                active &= (lNeutral < 90.f);
              }
              
              if( active.isEmpty() ) continue;
              
              //kill particle-candidates produced by clones
              active &= ( neutralDaughter.GetRapidity()<6.f /*&& neutralDaughter.GetRapidity()>0.f*/);
              if ((iTC==1 && iHypothesis<4) || iTC==2)
                active &= ( !( (neutralDaughter.GetPt())<0.5f && neutralDaughter.GetRapidity()<0.5f ) );
              if (iTC==3)
                active &= ( !( (neutralDaughter.GetPt())<0.2f && neutralDaughter.GetRapidity()<1.f ) );
              if( active.isEmpty() ) continue;

              
              for(int iV=0; iV<NTracks; iV++)
              {
                if(!active[iV]) continue;
                //save neutral
                neutralDaughter.GetKFParticle(mother_temp, iV);
                int neutralId = Particles.size();
                mother_temp.SetId(neutralId);
                if (iTrTypeDaughter==0)
                  mother_temp.SetPDG(-outNeutralDaughterPDG[iTC][iHypothesis]);
                else
                  mother_temp.SetPDG(outNeutralDaughterPDG[iTC][iHypothesis]);
                Particles.push_back(mother_temp);

                MotherFiltered.GetKFParticle(mother_temp, iV);
                mother_temp.SetId(Particles.size());
                mother_temp.CleanDaughtersId();
                mother_temp.AddDaughterId(ChargedDaughter.Id()[iV]);
                mother_temp.AddDaughterId(neutralId);
                if (iTrTypeDaughter==0)  
                  mother_temp.SetPDG(-outMotherPDG[iTC][iHypothesis]);
                else
                  mother_temp.SetPDG(outMotherPDG[iTC][iHypothesis]);
                Particles.push_back(mother_temp);
              }
            }
          }//iRot
        }//iTrM
      }//iTrD
    }//iTC
  }//iTrTypeDaughter
}
#endif

void KFParticleFinder::AddCandidate(const KFParticle& candidate, int iPV)
{
  /** Adds an externally found particle to either set of secondary or primary candidates:\n
   ** 1) if iPV is negative the candidate is stored to KFParticleFinder::fSecCandidates;\n
   ** 2) if iPV is not negative and smaller then the set number of primary vertices and
   ** NDF=2 the candidate is stored to KFParticleFinder::fPrimCandidates;\n
   ** 3) if iPV is not negative and smaller then the set number of primary vertices and
   ** NDF=3 the candidate is stored to KFParticleFinder::fPrimCandidatesTopo;\n
   ** 4) if iPV is not negative and smaller then the set number of primary vertices and
   ** NDF=4 the candidate is stored to KFParticleFinder::fPrimCandidatesTopoMass.\n
   ** Only those particles will be stored which have accepted PDG. Please check the documentation
   ** of the corresponding vectors for the list of particles.
   ** \param[in] candidate - candidate to be added
   ** \param[in] iPV - index of the associated PV
   **/

  //0 Ks, 1 Lambda,2 LambdaBar, 3 gamma, 4 pi0, 5 Xi, 6 XiBar, 7 Omega, 8 OmegaBar, 9 XiStar
  int iSet = -1;
  if(candidate.GetPDG() ==   310) iSet = 0;
  if(candidate.GetPDG() ==  3122) iSet = 1;
  if(candidate.GetPDG() == -3122) iSet = 2;
  if(candidate.GetPDG() ==    22) iSet = 3;
  if(candidate.GetPDG() ==   111) iSet = 4;
  if(candidate.GetPDG() ==  3312) iSet = 5;
  if(candidate.GetPDG() == -3312) iSet = 6;
  if(candidate.GetPDG() ==  3334) iSet = 7;
  if(candidate.GetPDG() == -3334) iSet = 8;
  
  if(iSet > -1)
  {
    if(iPV >= 0 && iPV<fNPV)
    {
      if(candidate.NDF() == 2)
        fPrimCandidates[iSet][iPV].push_back(candidate);
      if(candidate.NDF() == 3)
        fPrimCandidatesTopo[iSet][iPV].push_back(candidate);
      if(candidate.NDF() == 4)
        fPrimCandidatesTopoMass[iSet][iPV].push_back(candidate);
    }
    else if(iPV < 0)
    {
      fSecCandidates[iSet].push_back(candidate);
    }
  }
}

void KFParticleFinder::SetNPV(int nPV)
{
  /** Sets the number of primary vertices to "nPV", resizes all vectors
   ** with primary candidates correspondingly.
   ** \param[in] nPV - number of the primary vertices in the event to be set.
   **/
  
  fNPV = nPV;
  
  for(int iCandidates=0; iCandidates<fNPrimCandidatesSets; iCandidates++)
  {
    fPrimCandidates[iCandidates].clear();
    fPrimCandidates[iCandidates].resize(fNPV);
  }
  
  for(int iCandidates=0; iCandidates<fNPrimCandidatesTopoSets; iCandidates++)
  {
    fPrimCandidatesTopo[iCandidates].clear();
    fPrimCandidatesTopo[iCandidates].resize(fNPV);
    
    fPrimCandidatesTopoMass[iCandidates].clear();
    fPrimCandidatesTopoMass[iCandidates].resize(fNPV);
  }
}
